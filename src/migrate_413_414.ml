module From = Types_413.Types
module To = Types_414.Types
module To_ast = Migrate_parsetree.Ast_414

module Uid = struct
  let copy_t : From.Uid.t -> To.Uid.t = fun v -> v
end

module Variance = struct
  (* CHECK: this needs to be checked on every update *)
  external copy_t : From.Variance.t -> To.Variance.t = "%identity"

  let copy_f : From.Variance.f -> To.Variance.f = function
    | May_pos -> May_pos
    | May_neg -> May_neg
    | May_weak -> May_weak
    | Inj -> Inj
    | Pos -> Pos
    | Neg -> Neg
    | Inv -> Inv
end

module Separability = struct
  let copy_t : From.Separability.t -> To.Separability.t = function
    | Ind -> Ind
    | Sep -> Sep
    | Deepsep -> Deepsep
end

module Vars = struct
  let copy_t : 'a From.Vars.t -> 'a To.Vars.t =
   fun param -> From.Vars.bindings param |> List.to_seq |> To.Vars.of_seq
end

module Asttypes = struct
  open Migrate_parsetree
  let copy_mutable_flag = Migrate_413_414.copy_mutable_flag

  let copy_private_flag = Migrate_413_414.copy_private_flag

  let copy_arg_label = Migrate_413_414.copy_arg_label

  let copy_virtual_flag = Migrate_413_414.copy_virtual_flag
end

module Parsetree = struct
  let copy_attributes = Migrate_parsetree.Migrate_413_414.copy_attributes
end

module Type_immediacy = struct
  let copy_t : Type_immediacy_413.t -> Type_immediacy_414.t = function
    | Unknown -> Unknown
    | Always -> Always
    | Always_on_64bits -> Always_on_64bits
end

let rec copy_label_description : From.label_description -> To.label_description
    =
 fun {
       lbl_name = param_1;
       lbl_res = param_2;
       lbl_arg = param_3;
       lbl_mut = param_4;
       lbl_pos = param_5;
       lbl_all = param_6;
       lbl_repres = param_7;
       lbl_private = param_8;
       lbl_loc = param_9;
       lbl_attributes = param_10;
       lbl_uid = param_11;
     } ->
  {
    lbl_name = param_1;
    lbl_res = copy_type_expr param_2;
    lbl_arg = copy_type_expr param_3;
    lbl_mut = Asttypes.copy_mutable_flag param_4;
    lbl_pos = param_5;
    lbl_all = Array.map copy_label_description param_6;
    lbl_repres = copy_record_representation param_7;
    lbl_private = Asttypes.copy_private_flag param_8;
    lbl_loc = param_9;
    lbl_attributes = Parsetree.copy_attributes param_10;
    lbl_uid = Uid.copy_t param_11;
  }

and copy_constructor_tag : From.constructor_tag -> To.constructor_tag = function
  | Cstr_constant param_1 -> Cstr_constant param_1
  | Cstr_block param_1 -> Cstr_block param_1
  | Cstr_unboxed -> Cstr_unboxed
  | Cstr_extension (param_1, param_2) -> Cstr_extension (param_1, param_2)

and copy_constructor_description :
    From.constructor_description -> To.constructor_description =
 fun {
       cstr_name = param_1;
       cstr_res = param_2;
       cstr_existentials = param_3;
       cstr_args = param_4;
       cstr_arity = param_5;
       cstr_tag = param_6;
       cstr_consts = param_7;
       cstr_nonconsts = param_8;
       (* https://github.com/ocaml/ocaml/commit/650ba029a5c1c3e9f2edc8cacd30ff27d3184348a *)
       cstr_normal = _param_9;
       cstr_generalized = param_10;
       cstr_private = param_11;
       cstr_loc = param_12;
       cstr_attributes = param_13;
       cstr_inlined = param_14;
       cstr_uid = param_15;
     } ->
  {
    cstr_name = param_1;
    cstr_res = copy_type_expr param_2;
    cstr_existentials = List.map copy_type_expr param_3;
    cstr_args = List.map copy_type_expr param_4;
    cstr_arity = param_5;
    cstr_tag = copy_constructor_tag param_6;
    cstr_consts = param_7;
    cstr_nonconsts = param_8;
    cstr_generalized = param_10;
    cstr_private = Asttypes.copy_private_flag param_11;
    cstr_loc = param_12;
    cstr_attributes = Parsetree.copy_attributes param_13;
    cstr_inlined = Option.map copy_type_declaration param_14;
    cstr_uid = Uid.copy_t param_15;
  }

and copy_ext_status : From.ext_status -> To.ext_status = function
  | Text_first -> Text_first
  | Text_next -> Text_next
  | Text_exception -> Text_exception

and copy_rec_status : From.rec_status -> To.rec_status = function
  | Trec_not -> Trec_not
  | Trec_first -> Trec_first
  | Trec_next -> Trec_next

and copy_modtype_declaration :
    From.modtype_declaration -> To.modtype_declaration =
 fun {
       mtd_type = param_1;
       mtd_attributes = param_2;
       mtd_loc = param_3;
       mtd_uid = param_4;
     } ->
  {
    mtd_type = Option.map copy_module_type param_1;
    mtd_attributes = Parsetree.copy_attributes param_2;
    mtd_loc = param_3;
    mtd_uid = Uid.copy_t param_4;
  }

and copy_module_declaration : From.module_declaration -> To.module_declaration =
 fun {
       md_type = param_1;
       md_attributes = param_2;
       md_loc = param_3;
       md_uid = param_4;
     } ->
  {
    md_type = copy_module_type param_1;
    md_attributes = Parsetree.copy_attributes param_2;
    md_loc = param_3;
    md_uid = Uid.copy_t param_4;
  }

and copy_signature_item : From.signature_item -> To.signature_item = function
  | Sig_value (param_1, param_2, param_3) ->
      Sig_value
        (param_1, copy_value_description param_2, copy_visibility param_3)
  | Sig_type (param_1, param_2, param_3, param_4) ->
      Sig_type
        ( param_1,
          copy_type_declaration param_2,
          copy_rec_status param_3,
          copy_visibility param_4 )
  | Sig_typext (param_1, param_2, param_3, param_4) ->
      Sig_typext
        ( param_1,
          copy_extension_constructor param_2,
          copy_ext_status param_3,
          copy_visibility param_4 )
  | Sig_module (param_1, param_2, param_3, param_4, param_5) ->
      Sig_module
        ( param_1,
          copy_module_presence param_2,
          copy_module_declaration param_3,
          copy_rec_status param_4,
          copy_visibility param_5 )
  | Sig_modtype (param_1, param_2, param_3) ->
      Sig_modtype
        (param_1, copy_modtype_declaration param_2, copy_visibility param_3)
  | Sig_class (param_1, param_2, param_3, param_4) ->
      Sig_class
        ( param_1,
          copy_class_declaration param_2,
          copy_rec_status param_3,
          copy_visibility param_4 )
  | Sig_class_type (param_1, param_2, param_3, param_4) ->
      Sig_class_type
        ( param_1,
          copy_class_type_declaration param_2,
          copy_rec_status param_3,
          copy_visibility param_4 )

and copy_module_presence : From.module_presence -> To.module_presence = function
  | Mp_present -> Mp_present
  | Mp_absent -> Mp_absent

and copy_functor_parameter : From.functor_parameter -> To.functor_parameter =
  function
  | Unit -> Unit
  | Named (param_1, param_2) -> Named (param_1, copy_module_type param_2)

and copy_module_type : From.module_type -> To.module_type = function
  | Mty_ident param_1 -> Mty_ident param_1
  | Mty_signature param_1 -> Mty_signature (copy_signature param_1)
  | Mty_functor (param_1, param_2) ->
      Mty_functor (copy_functor_parameter param_1, copy_module_type param_2)
  | Mty_alias param_1 -> Mty_alias param_1

and copy_visibility : From.visibility -> To.visibility = function
  | Exported -> Exported
  | Hidden -> Hidden

and copy_class_type_declaration :
    From.class_type_declaration -> To.class_type_declaration =
 fun {
       clty_params = param_1;
       clty_type = param_2;
       clty_path = param_3;
       clty_variance = param_4;
       clty_loc = param_5;
       clty_attributes = param_6;
       clty_uid = param_7;
     } ->
  {
    clty_params = List.map copy_type_expr param_1;
    clty_type = copy_class_type param_2;
    clty_path = param_3;
    clty_variance = List.map Variance.copy_t param_4;
    clty_loc = param_5;
    clty_attributes = Parsetree.copy_attributes param_6;
    clty_uid = Uid.copy_t param_7;
  }

and copy_class_declaration : From.class_declaration -> To.class_declaration =
 fun {
       cty_params = param_1;
       cty_type = param_2;
       cty_path = param_3;
       cty_new = param_4;
       cty_variance = param_5;
       cty_loc = param_6;
       cty_attributes = param_7;
       cty_uid = param_8;
     } ->
  {
    cty_params = List.map copy_type_expr param_1;
    cty_type = copy_class_type param_2;
    cty_path = param_3;
    cty_new = Option.map copy_type_expr param_4;
    cty_variance = List.map Variance.copy_t param_5;
    cty_loc = param_6;
    cty_attributes = Parsetree.copy_attributes param_7;
    cty_uid = Uid.copy_t param_8;
  }

and copy_class_signature : From.class_signature -> To.class_signature =
 fun {
       csig_self = param_1;
       csig_vars = param_2;
       csig_concr = param_3;
       csig_inher = _param_4;
     } ->
  let rec repr typ =
    match typ.From.desc with Tlink typ -> repr typ | _ -> typ
  in
  let rec into_meths meths typ =
    match (repr typ).From.desc with
    | Tnil -> meths
    | Tfield ("*dummy method*", _, _, ts) -> into_meths meths ts
    | Tfield (name, _field_kind, typ, ts) ->
        let typ = copy_type_expr typ in
        let concr =
          if From.Concr.mem name param_3 then To_ast.Asttypes.Concrete
          else Virtual
        in
        (* TODO: I assume that externally it's always public *)
        let meths = To.Meths.add name (To.Mpublic, concr, typ) meths in
        into_meths meths ts
    | _ -> assert false
  in
  let meths =
    match (repr param_1).desc with
    | Tobject (typ, _) -> into_meths To.Meths.empty typ
    | _ -> assert false
  in
  {
    csig_self = copy_type_expr param_1;
    (* from what I understand this variable is only used during typing *)
    csig_self_row = Obj.magic 0;
    csig_vars =
      Vars.copy_t
        (From.Vars.map
           (fun (mut, virt, typ) ->
             ( Asttypes.copy_mutable_flag mut,
               Asttypes.copy_virtual_flag virt,
               copy_type_expr typ ))
           param_2);
    csig_meths = meths;
  }

and copy_class_type : From.class_type -> To.class_type = function
  | Cty_constr (param_1, param_2, param_3) ->
      Cty_constr
        (param_1, List.map copy_type_expr param_2, copy_class_type param_3)
  | Cty_signature param_1 -> Cty_signature (copy_class_signature param_1)
  | Cty_arrow (param_1, param_2, param_3) ->
      Cty_arrow
        ( Asttypes.copy_arg_label param_1,
          copy_type_expr param_2,
          copy_class_type param_3 )

and copy_type_transparence : From.type_transparence -> To.type_transparence =
  function
  | Type_public -> Type_public
  | Type_new -> Type_new
  | Type_private -> Type_private

and copy_extension_constructor :
    From.extension_constructor -> To.extension_constructor =
 fun {
       ext_type_path = param_1;
       ext_type_params = param_2;
       ext_args = param_3;
       ext_ret_type = param_4;
       ext_private = param_5;
       ext_loc = param_6;
       ext_attributes = param_7;
       ext_uid = param_8;
     } ->
  {
    ext_type_path = param_1;
    ext_type_params = List.map copy_type_expr param_2;
    ext_args = copy_constructor_arguments param_3;
    ext_ret_type = Option.map copy_type_expr param_4;
    ext_private = Asttypes.copy_private_flag param_5;
    ext_loc = param_6;
    ext_attributes = Parsetree.copy_attributes param_7;
    ext_uid = Uid.copy_t param_8;
  }

and copy_constructor_arguments :
    From.constructor_arguments -> To.constructor_arguments = function
  | Cstr_tuple param_1 -> Cstr_tuple (List.map copy_type_expr param_1)
  | Cstr_record param_1 -> Cstr_record (List.map copy_label_declaration param_1)

and copy_constructor_declaration :
    From.constructor_declaration -> To.constructor_declaration =
 fun {
       cd_id = param_1;
       cd_args = param_2;
       cd_res = param_3;
       cd_loc = param_4;
       cd_attributes = param_5;
       cd_uid = param_6;
     } ->
  {
    cd_id = param_1;
    cd_args = copy_constructor_arguments param_2;
    cd_res = Option.map copy_type_expr param_3;
    cd_loc = param_4;
    cd_attributes = Parsetree.copy_attributes param_5;
    cd_uid = Uid.copy_t param_6;
  }

and copy_label_declaration : From.label_declaration -> To.label_declaration =
 fun {
       ld_id = param_1;
       ld_mutable = param_2;
       ld_type = param_3;
       ld_loc = param_4;
       ld_attributes = param_5;
       ld_uid = param_6;
     } ->
  {
    ld_id = param_1;
    ld_mutable = Asttypes.copy_mutable_flag param_2;
    ld_type = copy_type_expr param_3;
    ld_loc = param_4;
    ld_attributes = Parsetree.copy_attributes param_5;
    ld_uid = Uid.copy_t param_6;
  }

and copy_record_representation :
    From.record_representation -> To.record_representation = function
  | Record_regular -> Record_regular
  | Record_float -> Record_float
  | Record_unboxed param_1 -> Record_unboxed param_1
  | Record_inlined param_1 -> Record_inlined param_1
  | Record_extension param_1 -> Record_extension param_1

and copy_variant_representation :
    From.variant_representation -> To.variant_representation = function
  | Variant_regular -> Variant_regular
  | Variant_unboxed -> Variant_unboxed

and copy_type_decl_kind : From.type_decl_kind -> To.type_decl_kind = function
  | Type_abstract -> Type_abstract
  | Type_record (param_1, param_2) ->
      Type_record
        ( List.map copy_label_declaration param_1,
          copy_record_representation param_2 )
  | Type_variant (param_1, param_2) ->
      Type_variant
        ( List.map copy_constructor_declaration param_1,
          copy_variant_representation param_2 )
  | Type_open -> Type_open

and copy_type_declaration : From.type_declaration -> To.type_declaration =
 fun {
       type_params = param_1;
       type_arity = param_2;
       type_kind = param_3;
       type_private = param_4;
       type_manifest = param_5;
       type_variance = param_6;
       type_separability = param_7;
       type_is_newtype = param_8;
       type_expansion_scope = param_9;
       type_loc = param_10;
       type_attributes = param_11;
       type_immediate = param_12;
       type_unboxed_default = param_13;
       type_uid = param_14;
     } ->
  {
    type_params = List.map copy_type_expr param_1;
    type_arity = param_2;
    type_kind = copy_type_decl_kind param_3;
    type_private = Asttypes.copy_private_flag param_4;
    type_manifest = Option.map copy_type_expr param_5;
    type_variance = List.map Variance.copy_t param_6;
    type_separability = List.map Separability.copy_t param_7;
    type_is_newtype = param_8;
    type_expansion_scope = param_9;
    type_loc = param_10;
    type_attributes = Parsetree.copy_attributes param_11;
    type_immediate = Type_immediacy.copy_t param_12;
    type_unboxed_default = param_13;
    type_uid = Uid.copy_t param_14;
  }

and copy_value_kind : From.value_kind -> To.value_kind = function
  (* TODO: check this function *)
  | Val_reg -> Val_reg
  | Val_prim param_1 -> Val_prim param_1
  | Val_ivar (param_1, param_2) ->
      Val_ivar (Asttypes.copy_mutable_flag param_1, param_2)
  | Val_self _ ->
      (* TODO: used only by Env, not on cmi *)
      assert false
  | Val_anc _ ->
      (* TODO: used only by Env, not on cmi *)
      assert false

and copy_value_description : From.value_description -> To.value_description =
 fun {
       val_type = param_1;
       val_kind = param_2;
       val_loc = param_3;
       val_attributes = param_4;
       val_uid = param_5;
     } ->
  {
    val_type = copy_type_expr param_1;
    val_kind = copy_value_kind param_2;
    val_loc = param_3;
    val_attributes = Parsetree.copy_attributes param_4;
    val_uid = Uid.copy_t param_5;
  }

and copy_commutable : From.commutable -> To.commutable = function
  (* TODO: validate this function *)
  | Cok -> To.commu_ok
  | Cunknown -> assert false
  | Clink { contents = Cunknown } -> To.commu_var ()
  | Clink { contents } -> copy_commutable contents

and copy_field_kind : From.field_kind -> To.field_kind = function
  (* TODO: validate this function *)
  | Fvar { contents = None } -> To.field_private ()
  | Fvar { contents = Some field_kind } -> copy_field_kind field_kind
  | Fpresent -> To.field_public
  | Fabsent -> To.field_absent

and copy_abbrev_memo : From.abbrev_memo -> To.abbrev_memo = function
  | Mnil -> Mnil
  | Mcons (param_1, param_2, param_3, param_4, param_5) ->
      Mcons
        ( Asttypes.copy_private_flag param_1,
          param_2,
          copy_type_expr param_3,
          copy_type_expr param_4,
          copy_abbrev_memo param_5 )
  | Mlink param_1 -> Mlink (ref (copy_abbrev_memo !param_1))

and copy_row_field : From.row_field -> To.row_field = function
  | Rpresent param_1 -> To.rf_present (Option.map copy_type_expr param_1)
  | Reither (param_1, param_2, param_3, param_4) ->
      let param_4 = Option.map copy_row_field !param_4 in
      let param_2 = List.map copy_type_expr param_2 in
      To.rf_either ?use_ext_of:param_4 ~no_arg:param_1 param_2 ~matched:param_3
  | Rabsent -> To.rf_absent

and copy_fixed_explanation : From.fixed_explanation -> To.fixed_explanation =
  function
  | Univar param_1 -> Univar (copy_type_expr param_1)
  | Fixed_private -> Fixed_private
  | Reified param_1 -> Reified param_1
  | Rigid -> Rigid

and copy_row_desc : From.row_desc -> To.row_desc =
 fun {
       row_fields = param_1;
       row_more = param_2;
       row_bound = _param_3;
       row_closed = param_4;
       row_fixed = param_5;
       row_name = param_6;
     } ->
  let fields =
    List.map
      (fun (label, row_field) -> (label, copy_row_field row_field))
      param_1
  in
  let more = copy_type_expr param_2 in
  let fixed = Option.map copy_fixed_explanation param_5 in
  let name =
    Option.map (fun (path, typ) -> (path, List.map copy_type_expr typ)) param_6
  in
  To.create_row ~fields ~more ~closed:param_4 ~fixed ~name

and copy_type_desc : From.type_desc -> To.type_desc = function
  | Tvar param_1 -> Tvar param_1
  | Tarrow (param_1, param_2, param_3, param_4) ->
      Tarrow
        ( Asttypes.copy_arg_label param_1,
          copy_type_expr param_2,
          copy_type_expr param_3,
          copy_commutable param_4 )
  | Ttuple param_1 -> Ttuple (List.map copy_type_expr param_1)
  | Tconstr (param_1, param_2, param_3) ->
      Tconstr
        ( param_1,
          List.map copy_type_expr param_2,
          ref (copy_abbrev_memo !param_3) )
  | Tobject (param_1, param_2) ->
      Tobject
        ( copy_type_expr param_1,
          ref
            (Option.map
               (fun (path, typ) -> (path, List.map copy_type_expr typ))
               !param_2) )
  | Tfield (param_1, param_2, param_3, param_4) ->
      Tfield
        ( param_1,
          copy_field_kind param_2,
          copy_type_expr param_3,
          copy_type_expr param_4 )
  | Tnil -> Tnil
  | Tlink param_1 -> Tlink (copy_type_expr param_1)
  | Tsubst (param_1, param_2) ->
      Tsubst (copy_type_expr param_1, Option.map copy_type_expr param_2)
  | Tvariant param_1 -> Tvariant (copy_row_desc param_1)
  | Tunivar param_1 -> Tunivar param_1
  | Tpoly (param_1, param_2) ->
      Tpoly (copy_type_expr param_1, List.map copy_type_expr param_2)
  | Tpackage (param_1, param_2) ->
      Tpackage
        (param_1, List.map (fun (lid, ty) -> (lid, copy_type_expr ty)) param_2)

and copy_type_expr : From.type_expr -> To.type_expr =
 fun { desc = param_1; level; scope; id } ->
  (* TODO: is this okay, OCaml uses physical identity for interface types? *)
  To.create_expr ~level ~scope ~id (copy_type_desc param_1)

and copy_signature signature = List.map copy_signature_item signature
