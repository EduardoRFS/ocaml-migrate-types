open Ppxlib
(**
#how to use

dune exec tools/make_copies.exe x.mli > y.ml
*)

let loc = Location.none

open Ast_builder.Make (struct
  let loc = loc
end)

let to_lid { txt; loc } = { txt = Lident txt; loc }

module Var = struct
  let counter = ref 0

  let reset () = counter := 0

  let next () =
    incr counter;
    Printf.sprintf "param_%d" !counter
end

let mk_copy name = "copy_" ^ name

module From_core_type = struct
  let should_ignore name =
    List.exists (( = ) name)
      [
        "int";
        "string";
        "bool";
        "mutable_flag";
        "private_flag";
        "arg_label";
        "unit";
      ]

  let function_to_apply constr =
    List.find_opt
      (fun (typ, _f) -> typ = constr)
      [ ("array", "Array.map"); ("list", "List.map"); ("option", "Option.map") ]
    |> Option.map snd

  let should_apply constr = function_to_apply constr <> None

  let function_to_copy = function
    | Ldot (Lident "Uid", "t") -> Some (Ldot (Lident "Uid", mk_copy "t"))
    | _ -> None

  let should_copy lid = function_to_copy lid <> None

  let rec pattern (typ : core_type) =
    match typ.ptyp_desc with
    | Ptyp_tuple types -> ppat_tuple (List.map pattern types)
    | _ -> pvar (Var.next ())

  let rec expression (typ : core_type) =
    match typ.ptyp_desc with
    | Ptyp_tuple types -> pexp_tuple (List.map expression types)
    | Ptyp_constr ({ txt = Lident name; _ }, []) when not (should_ignore name)
      ->
        eapply (evar (mk_copy name)) [ evar (Var.next ()) ]
    | Ptyp_constr
        ( { txt = Lident constr; _ },
          [ { ptyp_desc = Ptyp_constr ({ txt = Lident name; _ }, []); _ } ] )
      when should_apply constr && not (should_ignore name) ->
        let f = Option.get (function_to_apply constr) in
        eapply (evar f) [ evar (mk_copy name); evar (Var.next ()) ]
    | Ptyp_constr
        ( { txt = Lident "ref"; _ },
          [ { ptyp_desc = Ptyp_constr ({ txt = Lident name; _ }, []); _ } ] )
      when not (should_ignore name) ->
        let param = evar (Var.next ()) in
        [%expr ref ([%e evar (mk_copy name)] ![%e param])]
    | Ptyp_constr ({ txt = lid; _ }, []) when should_copy lid ->
        let f = Option.get (function_to_copy lid) in
        eapply (pexp_ident { txt = f; loc }) [ evar (Var.next ()) ]
    | _ -> evar (Var.next ())
end

module From_variant = struct
  let pattern decl =
    Var.reset ();
    let args =
      match decl.pcd_args with
      | Pcstr_tuple [] -> None
      | Pcstr_tuple types ->
          Some (ppat_tuple (List.map From_core_type.pattern types))
      (* TODO: Pcstr_record *)
      | Pcstr_record _ -> None
    in
    ppat_construct (to_lid decl.pcd_name) args

  let expression decl =
    Var.reset ();
    let args =
      match decl.pcd_args with
      | Pcstr_tuple [] -> None
      | Pcstr_tuple types ->
          Some (pexp_tuple (List.map From_core_type.expression types))
      (* TODO: Pcstr_record *)
      | Pcstr_record _ -> None
    in
    pexp_construct (to_lid decl.pcd_name) args

  let case decl = case ~guard:None ~lhs:(pattern decl) ~rhs:(expression decl)

  let copy decl = pexp_function (List.map case decl)
end

module From_record = struct
  let pattern field =
    (to_lid field.pld_name, From_core_type.pattern field.pld_type)

  let expression field =
    (to_lid field.pld_name, From_core_type.expression field.pld_type)

  let copy decl =
    Var.reset ();
    let pattern = ppat_record (List.map pattern decl) Closed in
    Var.reset ();
    let expression = pexp_record (List.map expression decl) None in
    pexp_fun Nolabel None pattern expression
end

module From_type = struct
  let binding decl =
    let ( let+ ) v f = Option.map f v in
    let+ expr =
      match decl.ptype_kind with
      | Ptype_variant decl -> Some (From_variant.copy decl)
      | Ptype_record decl -> Some (From_record.copy decl)
      | _ -> None
    in
    let name = decl.ptype_name.txt in
    let typ =
      ptyp_arrow Nolabel
        (ptyp_constr { txt = Ldot (Lident "From", name); loc } [])
        (ptyp_constr { txt = Ldot (Lident "To", name); loc } [])
    in
    let pat_name = mk_copy name in
    let pat = ppat_constraint (pvar pat_name) (Ast_helper.Typ.poly [] typ) in
    let expr = pexp_constraint expr typ in
    value_binding ~pat ~expr
end

module From_signature = struct
  let structure_item signature =
    let open Ocaml_common in
    let bindings = ref [] in
    let iterate =
      {
        Ast_iterator.default_iterator with
        type_declaration =
          (fun _ decl ->
            let decl = Selected_ast.Of_ocaml.copy_type_declaration decl in
            match From_type.binding decl with
            | Some binding -> bindings := binding :: !bindings
            | None -> ());
      }
    in
    iterate.signature iterate signature;
    pstr_value Recursive !bindings
end

let file = Sys.argv.(1)

let signature = Ocaml_common.Pparse.parse_interface ~tool_name:"<none>" file
(* 
let () =
  Format.printf "%a\n%!" Ocaml_common.Printast.implementation
    [%str let x : int -> int = fun a -> a]

let () =
  Format.printf "%a\n%!" Pprintast.structure_item
    [%stri let x : int -> int = fun a -> a] *)

let () =
  Format.printf "%a\n%!" Pprintast.structure_item
    (From_signature.structure_item signature)
