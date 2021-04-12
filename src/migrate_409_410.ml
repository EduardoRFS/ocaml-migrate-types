module From = Types_409
module To = Types_410

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

module Concr = struct
  let copy_t : From.Concr.t -> To.Concr.t =
   fun param ->
    let module Unsafe = struct
      (* CHECK: check this every update *)
      external copy_t : From.Concr.t -> To.Concr.t = "%identity"
    end in
    (* this guarantees the elt is the same *)
    let v : From.Concr.elt list = [] in
    let _v : To.Concr.elt list = v in
    Unsafe.copy_t param
end

module Vars = struct
  let copy_t : 'a From.Vars.t -> 'a To.Vars.t =
   fun param ->
    let module Unsafe = struct
      (* CHECK: check this every update *)
      external copy_t : 'a From.Vars.t -> 'a To.Vars.t = "%identity"
    end in
    (* this guarantees the key is the same *)
    let v : From.Vars.key list = [] in
    let _v : To.Vars.key list = v in
    Unsafe.copy_t param
end

module Meths = struct
  let copy_t : 'a From.Meths.t -> 'a To.Meths.t =
   fun param ->
    let module Unsafe = struct
      (* CHECK: check this every update *)
      external copy_t : 'a From.Meths.t -> 'a To.Meths.t = "%identity"
    end in
    (* this guarantees the key is the same *)
    let v : From.Meths.key list = [] in
    let _v : To.Meths.key list = v in
    Unsafe.copy_t param
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
     } ->
  {
    lbl_name = param_1;
    lbl_res = copy_type_expr param_2;
    lbl_arg = copy_type_expr param_3;
    lbl_mut = param_4;
    lbl_pos = param_5;
    lbl_all = Array.map copy_label_description param_6;
    lbl_repres = copy_record_representation param_7;
    lbl_private = param_8;
    lbl_loc = param_9;
    lbl_attributes = param_10;
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
       cstr_normal = param_9;
       cstr_generalized = param_10;
       cstr_private = param_11;
       cstr_loc = param_12;
       cstr_attributes = param_13;
       cstr_inlined = param_14;
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
    cstr_normal = param_9;
    cstr_generalized = param_10;
    cstr_private = param_11;
    cstr_loc = param_12;
    cstr_attributes = param_13;
    cstr_inlined = Option.map copy_type_declaration param_14;
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
 fun { mtd_type = param_1; mtd_attributes = param_2; mtd_loc = param_3 } ->
  {
    mtd_type = Option.map copy_module_type param_1;
    mtd_attributes = param_2;
    mtd_loc = param_3;
  }

and copy_module_declaration : From.module_declaration -> To.module_declaration =
 fun { md_type = param_1; md_attributes = param_2; md_loc = param_3 } ->
  {
    md_type = copy_module_type param_1;
    md_attributes = param_2;
    md_loc = param_3;
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

and copy_module_type : From.module_type -> To.module_type = function
  | Mty_ident param_1 -> Mty_ident param_1
  | Mty_signature param_1 -> Mty_signature (copy_signature param_1)
  | Mty_functor (param_1, param_2, param_3) ->
      Mty_functor
        ( (match param_2 with
          | Some mty -> Named (Some param_1, copy_module_type mty)
          | None -> Unit),
          copy_module_type param_3 )
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
     } ->
  {
    clty_params = List.map copy_type_expr param_1;
    clty_type = copy_class_type param_2;
    clty_path = param_3;
    clty_variance = List.map Variance.copy_t param_4;
    clty_loc = param_5;
    clty_attributes = param_6;
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
     } ->
  {
    cty_params = List.map copy_type_expr param_1;
    cty_type = copy_class_type param_2;
    cty_path = param_3;
    cty_new = Option.map copy_type_expr param_4;
    cty_variance = List.map Variance.copy_t param_5;
    cty_loc = param_6;
    cty_attributes = param_7;
  }

and copy_class_signature : From.class_signature -> To.class_signature =
 fun {
       csig_self = param_1;
       csig_vars = param_2;
       csig_concr = param_3;
       csig_inher = param_4;
     } ->
  {
    csig_self = copy_type_expr param_1;
    csig_vars =
      Vars.copy_t
        (From.Vars.map
           (fun (mut, virt, typ) -> (mut, virt, copy_type_expr typ))
           param_2);
    csig_concr = Concr.copy_t param_3;
    csig_inher =
      List.map
        (fun (path, typs) -> (path, List.map copy_type_expr typs))
        param_4;
  }

and copy_class_type : From.class_type -> To.class_type = function
  | Cty_constr (param_1, param_2, param_3) ->
      Cty_constr
        (param_1, List.map copy_type_expr param_2, copy_class_type param_3)
  | Cty_signature param_1 -> Cty_signature (copy_class_signature param_1)
  | Cty_arrow (param_1, param_2, param_3) ->
      Cty_arrow (param_1, copy_type_expr param_2, copy_class_type param_3)

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
     } ->
  {
    ext_type_path = param_1;
    ext_type_params = List.map copy_type_expr param_2;
    ext_args = copy_constructor_arguments param_3;
    ext_ret_type = Option.map copy_type_expr param_4;
    ext_private = param_5;
    ext_loc = param_6;
    ext_attributes = param_7;
  }

and copy_unboxed_status : From.unboxed_status -> To.unboxed_status =
 fun param ->
  let module Unsafe = struct
    (* CHECK: check this every update *)
    external copy_unboxed_status : From.unboxed_status -> To.unboxed_status
      = "%identity"
  end in
  Unsafe.copy_unboxed_status param

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
     } ->
  {
    cd_id = param_1;
    cd_args = copy_constructor_arguments param_2;
    cd_res = Option.map copy_type_expr param_3;
    cd_loc = param_4;
    cd_attributes = param_5;
  }

and copy_label_declaration : From.label_declaration -> To.label_declaration =
 fun {
       ld_id = param_1;
       ld_mutable = param_2;
       ld_type = param_3;
       ld_loc = param_4;
       ld_attributes = param_5;
     } ->
  {
    ld_id = param_1;
    ld_mutable = param_2;
    ld_type = copy_type_expr param_3;
    ld_loc = param_4;
    ld_attributes = param_5;
  }

and copy_record_representation :
    From.record_representation -> To.record_representation = function
  | Record_regular -> Record_regular
  | Record_float -> Record_float
  | Record_unboxed param_1 -> Record_unboxed param_1
  | Record_inlined param_1 -> Record_inlined param_1
  | Record_extension param_1 -> Record_extension param_1

and copy_type_kind : From.type_kind -> To.type_kind = function
  | Type_abstract -> Type_abstract
  | Type_record (param_1, param_2) ->
      Type_record
        ( List.map copy_label_declaration param_1,
          copy_record_representation param_2 )
  | Type_variant param_1 ->
      Type_variant (List.map copy_constructor_declaration param_1)
  | Type_open -> Type_open

and copy_type_declaration : From.type_declaration -> To.type_declaration =
 fun {
       type_params = param_1;
       type_arity = param_2;
       type_kind = param_3;
       type_private = param_4;
       type_manifest = param_5;
       type_variance = param_6;
       type_is_newtype = param_7;
       type_expansion_scope = param_8;
       type_loc = param_9;
       type_attributes = param_10;
       type_immediate = param_11;
       type_unboxed = param_12;
     } ->
  {
    type_params = List.map copy_type_expr param_1;
    type_arity = param_2;
    type_kind = copy_type_kind param_3;
    type_private = param_4;
    type_manifest = Option.map copy_type_expr param_5;
    type_variance = List.map Variance.copy_t param_6;
    type_is_newtype = param_7;
    type_expansion_scope = param_8;
    type_loc = param_9;
    type_attributes = param_10;
    (* TODO: check this, it makes sense to me, but who knows *)
    type_immediate = (if param_11 then Type_immediacy_410.Always else Unknown);
    type_unboxed = copy_unboxed_status param_12;
  }

and copy_value_kind : From.value_kind -> To.value_kind = function
  | Val_reg -> Val_reg
  | Val_prim param_1 -> Val_prim param_1
  | Val_ivar (param_1, param_2) -> Val_ivar (param_1, param_2)
  | Val_self (param_1, param_2, param_3, param_4) ->
      Val_self
        ( ref
            (Meths.copy_t
               (From.Meths.map
                  (fun (ident, typ) -> (ident, copy_type_expr typ))
                  !param_1)),
          ref
            (Vars.copy_t
               (From.Vars.map
                  (fun (ident, mut, virt, typ) ->
                    (ident, mut, virt, copy_type_expr typ))
                  !param_2)),
          param_3,
          copy_type_expr param_4 )
  | Val_anc (param_1, param_2) -> Val_anc (param_1, param_2)
  (* TODO: I think this never happens, but I'm not sure on it *)
  | Val_unbound _param_1 -> assert false

and copy_value_description : From.value_description -> To.value_description =
 fun {
       val_type = param_1;
       val_kind = param_2;
       val_loc = param_3;
       val_attributes = param_4;
     } ->
  {
    val_type = copy_type_expr param_1;
    val_kind = copy_value_kind param_2;
    val_loc = param_3;
    val_attributes = param_4;
  }

and copy_commutable : From.commutable -> To.commutable = function
  | Cok -> Cok
  | Cunknown -> Cunknown
  | Clink param_1 -> Clink (ref (copy_commutable !param_1))

and copy_field_kind : From.field_kind -> To.field_kind = function
  | Fvar param_1 -> Fvar (ref (Option.map copy_field_kind !param_1))
  | Fpresent -> Fpresent
  | Fabsent -> Fabsent

and copy_abbrev_memo : From.abbrev_memo -> To.abbrev_memo = function
  | Mnil -> Mnil
  | Mcons (param_1, param_2, param_3, param_4, param_5) ->
      Mcons
        ( param_1,
          param_2,
          copy_type_expr param_3,
          copy_type_expr param_4,
          copy_abbrev_memo param_5 )
  | Mlink param_1 -> Mlink (ref (copy_abbrev_memo !param_1))

and copy_row_field : From.row_field -> To.row_field = function
  | Rpresent param_1 -> Rpresent (Option.map copy_type_expr param_1)
  | Reither (param_1, param_2, param_3, param_4) ->
      Reither
        ( param_1,
          List.map copy_type_expr param_2,
          param_3,
          ref (Option.map copy_row_field !param_4) )
  | Rabsent -> Rabsent

and copy_row_desc : From.row_desc -> To.row_desc =
 fun {
       row_fields = param_1;
       row_more = param_2;
       row_bound = param_3;
       row_closed = param_4;
       row_fixed = param_5;
       row_name = param_6;
     } ->
  {
    row_fields =
      List.map
        (fun (label, row_field) -> (label, copy_row_field row_field))
        param_1;
    row_more = copy_type_expr param_2;
    row_bound = param_3;
    row_closed = param_4;
    (* TODO: check if this is okay, seems like explanation is only for error message *)
    row_fixed = (if param_5 then Some Rigid else None);
    row_name =
      Option.map
        (fun (path, typ) -> (path, List.map copy_type_expr typ))
        param_6;
  }

and copy_type_desc : From.type_desc -> To.type_desc = function
  | Tvar param_1 -> Tvar param_1
  | Tarrow (param_1, param_2, param_3, param_4) ->
      Tarrow
        ( param_1,
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
  | Tsubst param_1 -> Tsubst (copy_type_expr param_1)
  | Tvariant param_1 -> Tvariant (copy_row_desc param_1)
  | Tunivar param_1 -> Tunivar param_1
  | Tpoly (param_1, param_2) ->
      Tpoly (copy_type_expr param_1, List.map copy_type_expr param_2)
  | Tpackage (param_1, param_2, param_3) ->
      Tpackage (param_1, param_2, List.map copy_type_expr param_3)

and copy_type_expr : From.type_expr -> To.type_expr =
 fun { desc = param_1; level = param_2; scope = param_3; id = param_4 } ->
  {
    desc = copy_type_desc param_1;
    level = param_2;
    scope = param_3;
    id = param_4;
  }

and copy_signature signature = List.map copy_signature_item signature
