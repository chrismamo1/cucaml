open Ocamlbuild_plugin
open Command

let () =
  dispatch begin function
  | After_rules ->
      flag ["needs_unix"; "ocaml"; "link"] (A "unix.cmxa");
      (*flag ["needs_camlcuda"; "ocaml"] (A "camlCuda.o");*)
      dep ["needs_camlcuda"; "ocaml"; "link"] ["camlCuda.o"];
      flag ["reason_file"] (A "-pp refmt -intf-suffix rei")
  | _ -> ()
  end