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

[%%if ocaml_version >= (4, 13, 0) && ocaml_version < (4, 14, 0)]
include Type_immediacy
[%%else];;
  
type t =
  | Unknown
  | Always
  | Always_on_64bits

module Violation = struct
  type t =
    | Not_always_immediate
    | Not_always_immediate_on_64bits
end
[%%endif]
