module Location = struct
  include Location

  let pp = print_loc
end

module Longident = struct
  include Longident

  type t = [%import: Longident.t] [@@deriving show { with_path = false }]
end

module Asttypes = struct
  type 'a loc = [%import: 'a Asttypes.loc]
  and label = [%import: Asttypes.label]
  and closed_flag = [%import: Asttypes.closed_flag]
  and arg_label = [%import: Asttypes.arg_label]
  and direction_flag = [%import: Asttypes.direction_flag]
  and rec_flag = [%import: Asttypes.rec_flag]
  and private_flag = [%import: Asttypes.private_flag]
  and injectivity = [%import: Asttypes.injectivity]
  and variance = [%import: Asttypes.variance]
  and mutable_flag = [%import: Asttypes.mutable_flag]
  and virtual_flag = [%import: Asttypes.virtual_flag]

  and override_flag = [%import: Asttypes.override_flag]
  [@@deriving show { with_path = false }]
end

module Parsetree = struct
  type attributes = [%import: Parsetree.attributes]
  and attribute = [%import: Parsetree.attribute]
  and extension = [%import: Parsetree.extension]
  and payload = [%import: Parsetree.payload]
  and core_type = [%import: Parsetree.core_type]
  and core_type_desc = [%import: Parsetree.core_type_desc]
  and package_type = [%import: Parsetree.package_type]
  and row_field = [%import: Parsetree.row_field]
  and row_field_desc = [%import: Parsetree.row_field_desc]
  and object_field = [%import: Parsetree.object_field]
  and object_field_desc = [%import: Parsetree.object_field_desc]
  and pattern = [%import: Parsetree.pattern]
  and pattern_desc = [%import: Parsetree.pattern_desc]
  and expression = [%import: Parsetree.expression]
  and expression_desc = [%import: Parsetree.expression_desc]
  and case = [%import: Parsetree.case]
  and letop = [%import: Parsetree.letop]
  and binding_op = [%import: Parsetree.binding_op]
  and value_description = [%import: Parsetree.value_description]
  and type_declaration = [%import: Parsetree.type_declaration]
  and type_kind = [%import: Parsetree.type_kind]
  and label_declaration = [%import: Parsetree.label_declaration]
  and constructor_declaration = [%import: Parsetree.constructor_declaration]
  and constructor_arguments = [%import: Parsetree.constructor_arguments]
  and type_extension = [%import: Parsetree.type_extension]
  and extension_constructor = [%import: Parsetree.extension_constructor]
  and type_exception = [%import: Parsetree.type_exception]
  and constant = [%import: Parsetree.constant]

  and extension_constructor_kind =
    [%import: Parsetree.extension_constructor_kind]

  and class_type = [%import: Parsetree.class_type]
  and class_type_desc = [%import: Parsetree.class_type_desc]
  and class_signature = [%import: Parsetree.class_signature]
  and class_type_field = [%import: Parsetree.class_type_field]
  and class_type_field_desc = [%import: Parsetree.class_type_field_desc]
  and class_description = [%import: Parsetree.class_description]
  and class_type_declaration = [%import: Parsetree.class_type_declaration]
  and class_expr = [%import: Parsetree.class_expr]
  and class_expr_desc = [%import: Parsetree.class_expr_desc]
  and class_structure = [%import: Parsetree.class_structure]
  and class_field = [%import: Parsetree.class_field]
  and class_field_desc = [%import: Parsetree.class_field_desc]
  and class_field_kind = [%import: Parsetree.class_field_kind]
  and class_declaration = [%import: Parsetree.class_declaration]
  and module_type = [%import: Parsetree.module_type]
  and module_type_desc = [%import: Parsetree.module_type_desc]
  and functor_parameter = [%import: Parsetree.functor_parameter]
  and signature = [%import: Parsetree.signature]
  and signature_item = [%import: Parsetree.signature_item]
  and signature_item_desc = [%import: Parsetree.signature_item_desc]
  and module_declaration = [%import: Parsetree.module_declaration]
  and module_substitution = [%import: Parsetree.module_substitution]
  and module_type_declaration = [%import: Parsetree.module_type_declaration]
  and open_description = [%import: Parsetree.open_description]
  and open_declaration = [%import: Parsetree.open_declaration]
  and include_description = [%import: Parsetree.include_description]
  and include_declaration = [%import: Parsetree.include_declaration]
  and with_constraint = [%import: Parsetree.with_constraint]
  and module_expr = [%import: Parsetree.module_expr]
  and module_expr_desc = [%import: Parsetree.module_expr_desc]
  and structure = [%import: Parsetree.structure]
  and structure_item = [%import: Parsetree.structure_item]
  and structure_item_desc = [%import: Parsetree.structure_item_desc]
  and value_binding = [%import: Parsetree.value_binding]
  and module_binding = [%import: Parsetree.module_binding]
  and toplevel_directive = [%import: Parsetree.toplevel_directive]
  and directive_argument = [%import: Parsetree.directive_argument]
  and directive_argument_desc = [%import: Parsetree.directive_argument_desc]
  and location_stack = [%import: Parsetree.location_stack]
  and 'a class_infos = [%import: 'a Parsetree.class_infos]
  and 'a open_infos = [%import: 'a Parsetree.open_infos]

  and 'a include_infos = [%import: 'a Parsetree.include_infos]
  [@@deriving show { with_path = false }]
end

module Primitive = struct
  include Primitive

  type description = [%import: Primitive.description]
  and native_repr = [%import: Primitive.native_repr]

  and boxed_integer = [%import: Primitive.boxed_integer]
  [@@deriving show { with_path = false }]
end

module Types = struct
  module Types = struct
    include Types

    module Variance = struct
      include Variance

      let pp fmt _t = Format.fprintf fmt "<variance>"
    end

    module Path = struct
      include Path

      let pp = print
    end

    module Ident = struct
      include Ident

      let pp = print_with_scope
    end

    module Uid = struct
      include Uid

      let pp = print
    end

    module Meths = struct
      include Meths

      type 'v print_t = (string * 'v) list [@@deriving show]
      let pp pp_v fmt t = Format.fprintf fmt "%a" (pp_print_t pp_v) (bindings t)
    end

    module Vars = struct
      include Vars

      type 'v print_t = (string * 'v) list [@@deriving show]
      let pp pp_v fmt t = Format.fprintf fmt "%a" (pp_print_t pp_v) (bindings t)
    end

    module Type_immediacy = struct
      include Type_immediacy

      type t = [%import: Type_immediacy.t]
      [@@deriving show { with_path = false }]
    end

    module Separability = struct
      include Separability

      type t = [%import: Types.Separability.t]
      [@@deriving show { with_path = false }]
    end

    type field_kind_view = [%import: Types.field_kind_view]
    [@@deriving show { with_path = false }]

    let pp_field_kind fmt t = pp_field_kind_view fmt (field_kind_repr t)
    let pp_row_desc fmt _t = Format.fprintf fmt "<row_desc>"
    let pp_commutable fmt _t = Format.fprintf fmt "<commutable>"
  end

  include Types

  let pp_type_expr_ref = ref (fun _ -> assert false)
  let pp_type_expr fmt t = !pp_type_expr_ref fmt t

  type signature_item = [%import: Types.signature_item]
  and signature = [%import: Types.signature]
  and visibility = [%import: Types.visibility]
  and rec_status = [%import: Types.rec_status]
  and class_type_declaration = [%import: Types.class_type_declaration]
  and class_declaration = [%import: Types.class_declaration]
  and fixed_explanation = [%import: Types.fixed_explanation]
  and abbrev_memo = [%import: Types.abbrev_memo]
  and field_kind = [%import: Types.field_kind]
  and ('a, 'b) type_kind = [%import: ('a, 'b) Types.type_kind]
  and value_kind = [%import: Types.value_kind]
  and class_signature = [%import: Types.class_signature]
  and self_meths = [%import: Types.self_meths]
  and module_type = [%import: Types.module_type]
  and method_privacy = [%import: Types.method_privacy]
  and type_decl_kind = [%import: Types.type_decl_kind]
  and record_representation = [%import: Types.record_representation]
  and variant_representation = [%import: Types.variant_representation]
  and label_declaration = [%import: Types.label_declaration]
  and constructor_declaration = [%import: Types.constructor_declaration]
  and constructor_arguments = [%import: Types.constructor_arguments]
  and type_transparence = [%import: Types.type_transparence]
  and functor_parameter = [%import: Types.functor_parameter]
  and module_presence = [%import: Types.module_presence]
  and module_declaration = [%import: Types.module_declaration]
  and modtype_declaration = [%import: Types.modtype_declaration]
  and ext_status = [%import: Types.ext_status]
  and constructor_tag = [%import: Types.constructor_tag]
  and extension_constructor = [%import: Types.extension_constructor]
  and type_declaration = [%import: Types.type_declaration]
  and value_description = [%import: Types.value_description]
  and class_type = [%import: Types.class_type]
  and transient_expr = [%import: Types.transient_expr]

  and type_desc = [%import: Types.type_desc]
  [@@deriving show { with_path = false }]

  let () =
    pp_type_expr_ref :=
      fun fmt t -> pp_transient_expr fmt (Types.Transient_expr.repr t)
end
