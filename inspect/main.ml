open Tptp_ast

exception Cannot_handle of string
let cannot_handle s = raise (Cannot_handle s)

(** parse parses Tptp_ast.tptp_input *)
let parse : Tptp_ast.tptp_input -> unit = function
| Fof_anno _ -> cannot_handle "fof annotation found"
| Cnf_anno _ -> ()
| Include _ -> cannot_handle "include found"
| Comment _ -> ()

(** inspect inspects parses given filename and inspect it *)
let inspect filename =
  try
    let tptp_inputs = Tptp.File.read filename in
    List.iter parse tptp_inputs;
    Printf.printf "YES,\t-\n"
  with
  | Cannot_handle s -> Printf.printf "NO,\t%s\n" s
  | Tptp.Parse_error _ -> Printf.printf "NO,\tparse error\n"

let () =
  let filename = try Sys.argv.(1) with Invalid_argument _ -> "left_unit.p" in
  inspect filename