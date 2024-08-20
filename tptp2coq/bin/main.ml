open Tptp2coqp.Lib

(*
  tptp2coqp file.p
  tptp2copq file.p l (* lpo_rewrite *)
  tptp2coqp file.p h (* use hammer *)
 *)
let () =
  if Array.length Sys.argv < 2 then
    print_endline
      (String.concat "\n"
         [
           "Usage:";
           "tptp2coqp file.p";
           "tptp2coqp file.p l // use lpo_autorewrite.";
           "tptp2coqp file.p h // use hammer.";
           "tptp2coqp file.p s // use smt.";
         ])
  else
    let filename = Sys.argv.(1) in
    (* Tptp.File.read "left_unit.p"
       |> List.iter parse_input; *)
    let problem = parse (Tptp.File.read filename) in
    if Array.length Sys.argv == 2 then
      gen_coq_p_with_completion problem false false false
    else if Sys.argv.(2) = "l" then
      gen_coq_p_with_completion problem true false false
    else if Sys.argv.(2) = "h" then
      gen_coq_p_with_completion problem false true false
    else if Sys.argv.(2) = "s" then
      gen_coq_p_with_completion problem false false true
    else
      print_endline
        (String.concat "\n"
           [
             "Usage:";
             "tptp2coqp file.p";
             "tptp2coqp file.p l // use lpo_autorewrite";
             "tptp2coqp file.p h // use hammer.";
             "tptp2coqp file.p s // use smt.";
           ])
