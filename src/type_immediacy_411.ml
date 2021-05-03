(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[%%if ocaml_version = (4, 11, 0)]
include Type_immediacy
[%%else];;

(** Immediacy status of a type *)

type t =
  | Unknown 
      (** We don't know anything *)
  | Always
      (** We know for sure that values of this type are always immediate *)
  | Always_on_64bits
      (** We know for sure that values of this type are always immediate
      on 64 bit platforms. For other platforms, we know nothing. *)

module Violation = struct
  type t = Type_immediacy_410.Violation.t =
    | Not_always_immediate
    | Not_always_immediate_on_64bits
end
[%%endif]
