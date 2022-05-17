open Parsetree

let loc = Location.none

let code =
  [%str
    class a =
      object
        method x = 1
      end

    class b =
      object
        inherit a
        method y = 2
      end

    class c =
      object (self)
        inherit b
        method! x = self#y
      end
    class virtual x =
      object (self : < y : int ; .. >)
        method x = (self#y [@ocaml.warning "-17"])
      end]

let env =
  Compmisc.init_path ();
  Compmisc.initial_env ()

let tcode, tsig, tsig_names, tshape, env = Typemod.type_structure env code
let () = Format.printf "%a\n%!" Printtyp.signature tsig
let () = Format.printf "%a\n%!" Print_types.Types.pp_signature tsig
