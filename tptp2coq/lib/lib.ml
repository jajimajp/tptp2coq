open Tptp_ast

let print_atomic_word = function
  | Plain_word s -> print_endline (Tptp_printer.show_plain_word s)
  | _ -> failwith "print_atomic_word: not implemented"

let rec print_term = function
  | Var v -> print_endline (Tptp_printer.show_var v)
  | Func (f, args) ->
      print_endline "Func <";
      print_atomic_word f;
      print_endline "> (";
      List.iter print_term args;
      print_endline ")"
  | Number _ -> failwith "Number not implemented"
  | String _ -> failwith "String not implemented"

let print_atom = function
  | Equals (t1, t2) ->
      print_endline "Equals (";
      print_term t1;
      print_term t2;
      print_endline ")"
  | Pred _ -> failwith "Pred not implemented"

let print_literal = function
  | Lit (Pos, a) -> print_atom a
  | Lit (Neg, a) ->
      print_string "Neg ";
      print_atom a

let print_cnf_formula = function
  | Clause lits -> (
      match lits with
      | [ x ] -> print_literal x
      | _ ->
          failwith
            "cnf formula with zero or more than one literal not implemented")

let print_formula_name = function
  | N_word s -> print_endline (Tptp_printer.show_plain_word s)
  | N_integer _ -> failwith "Not implemented"

let parse_annotated_formula af =
  print_formula_name af.af_name;
  print_cnf_formula af.af_formula

let parse_input_r = function
  | Fof_anno _ ->
      print_endline "Fof_anno";
      failwith "fof_anno not implemented"
  | Cnf_anno af ->
      print_endline "Cnf_anno";
      parse_annotated_formula af;
      () (* failwith "Not implemented" *)
  | Comment _ ->
      print_endline "Comment";
      ()
  | Include _ ->
      print_endline "Include";
      ()

let parse_input i =
  parse_input_r i;
  i |> Tptp_printer.print_tptp_input |> PPrint.ToChannel.compact stdout

type t = {
  axioms : Tptp_ast.cnf_formula Tptp_ast.annotated_formula list;
  constants : (string * int) list (* (name, num_of_args) list *);
  problem : Tptp_ast.cnf_formula;
}

(** [is_equality input] returns true if [input] term is <... = ...>.
    returns false otherwise, especially when the term is <... != ...>. *)
let is_equality = function
  | Cnf_anno af -> (
      match af.af_formula with
      | Clause [ Lit (Pos, Equals _) ] -> true
      | _ -> false)
  | _ -> false

let is_axiom = function
  | Cnf_anno af as t -> (
      match af.af_role with
      | R_axiom -> true
      | R_hypothesis -> true
      | R_definition -> true
      | R_assumption -> true
      | R_lemma -> true (* TODO: is this ok? *)
      | R_theorem -> true (* TODO: is this ok? *)
      | R_negated_conjecture ->
          if is_equality t then true (* TODO: is this ok? *) else false
      | _ -> false)
  | _ -> false

let is_constant i =
  (* 小文字で始まる識別子は定数 *)
  if String.length i = 0 then false
  else match i.[0] with 'a' .. 'z' -> true | _ -> false

let rec identifiers_in_term = function
  | Var v -> [ Tptp_printer.show_var v ]
  | Func (f, args) ->
      let f =
        match f with
        | Plain_word s -> Tptp_printer.show_plain_word s
        | _ -> failwith "identifiers_in_term: not implemented"
      in
      f :: List.concat (List.map identifiers_in_term args)
  | _ -> failwith "identifiers_in_term: not implemented"

let rec identifiers_with_rank_in_term = function
  | Var v -> [ (Tptp_printer.show_var v, 0) ]
  | Func (f, args) ->
      let f =
        match f with
        | Plain_word s -> Tptp_printer.show_plain_word s
        | _ -> failwith "identifiers_in_term: not implemented"
      in
      let arglen = List.length args in
      let args =
        List.concat (List.map (fun t -> identifiers_with_rank_in_term t) args)
      in
      (f, arglen) :: args
  | _ -> failwith "identifiers_in_term: not implemented"

let rec identifiers_in_atom = function
  | Equals (t1, t2) ->
      List.concat [ identifiers_in_term t1; identifiers_in_term t2 ]
  | _ -> failwith "identifiers_in_atom: not implemented"

let identifiers_with_rank_in_atom = function
  | Equals (t1, t2) ->
      List.concat
        [ identifiers_with_rank_in_term t1; identifiers_with_rank_in_term t2 ]
  | _ -> failwith "identifiers_in_atom: not implemented"

let axioms (inputs : tptp_input list) =
  let rec aux axs = function
    | [] -> axs
    | h :: t ->
        if is_axiom h then
          match h with
          | Cnf_anno af -> aux (af :: axs) t
          | _ -> failwith "axioms: not implemented"
        else aux axs t
  in
  aux [] inputs

let find_problem inputs =
  let rec aux = function
    | [] -> failwith "find_problem: not found"
    | h :: t -> (
        match h with
        | Cnf_anno af -> (
            match af.af_role with
            | R_axiom | R_hypothesis | R_definition | R_assumption | R_lemma
            | R_theorem ->
                aux t
            | R_negated_conjecture ->
                if is_equality h then aux t else af.af_formula
            | _ -> aux t)
        | _ -> aux t)
  in
  aux inputs

let string_of_lit = function
  | Lit (Pos, _) -> "Lit (Pos, _)"
  | Lit (Neg, _) -> "Lit (Neg, _)"

let parse t =
  let axioms = axioms t in
  let compare_pair (i1, _) (i2, _) = compare i1 i2 in
  let constants =
    List.concat
      (List.map
         (fun formula ->
           match formula with
           | Clause lits ->
               List.concat
                 (List.filter_map
                    (fun lit ->
                      match lit with
                      | Lit (_, a) ->
                          Some (identifiers_with_rank_in_atom a)
                          (* parse as Pos for Pos & Neg *)
                      | _ ->
                          Printf.eprintf
                            "Encountered not implemented literal. Some \
                             constants may be ignored.";
                          Printf.eprintf "%s\n" (string_of_lit lit);
                          None)
                    lits))
         (List.filter_map
            (function Cnf_anno af -> Some af.af_formula | _ -> None)
            t))
    |> List.filter (fun (i, _) -> is_constant i)
    |> List.sort_uniq compare_pair
  in
  let problem = find_problem t in
  { axioms; constants; problem }

let rec string_of_atom = function
  | Equals (t1, t2) -> string_of_term t1 ^ " = " ^ string_of_term t2
  | _ -> failwith "string_of_atom: not implemented"


(** [string_of_atom_s atom] returns string expression for SMTCoq.
    This uses "=?" as equal. *)
and string_of_atom_s = function
  | Equals (t1, t2) -> string_of_term t1 ^ " =? " ^ string_of_term t2
  | _ -> failwith "string_of_atom: not implemented"

and string_of_term = function
  | Var v -> Tptp_printer.show_var v
  | Func (f, args) -> (
      let f =
        match f with
        | Plain_word s -> Tptp_printer.show_plain_word s
        | _ -> failwith "string_of_term: not implemented"
      in
      match List.length args with
      | 0 -> f
      | _ ->
          "(" ^ f ^ " " ^ String.concat " " (List.map string_of_term args) ^ ")"
      )
  | _ -> failwith "string_of_term: not implemented"

let string_of_formula problem f =
  let vars =
    match f with
    | Lit (_, a) ->
        identifiers_in_atom a |> List.sort_uniq compare
        |> List.filter (fun i -> not (is_constant i))
  in
  (if List.length vars = 0 then ""
   else "forall " ^ String.concat " " vars ^ " : G, ")
  ^ string_of_atom (match f with Lit (_, a) -> a)

let print_formula problem f =
  let vars =
    match f with
    | Lit (_, a) ->
        identifiers_in_atom a |> List.sort_uniq compare
        |> List.filter (fun i -> not (is_constant i))
  in
  print_string
    ((if List.length vars = 0 then ""
      else "forall " ^ String.concat " " vars ^ " : G, ")
    ^ string_of_atom (match f with Lit (_, a) -> a))

let print_axioms_in_p p =
  List.iter
    (fun af ->
      match af.af_formula with
      | Clause lits ->
          let name =
            match af.af_name with
            | N_word s -> Tptp_printer.show_plain_word s
            | _ -> failwith "print_axioms_in_p: not implemented"
          in
          let lit = List.hd lits in
          print_string ("Axiom ax_" ^ name ^ " : ");
          print_formula p lit;
          print_endline ".")
    p.axioms

(** [string_of_formula_s f] returns string expression of formula [f].
    _s for "SMTCoq version". This uses "Z" as the set. *)
let string_of_formula_s f =
  let vars =
    match f with
    | Lit (_, a) ->
        identifiers_in_atom a |> List.sort_uniq compare
        |> List.filter (fun i -> not (is_constant i))
  in
  (if List.length vars = 0 then ""
   else "forall " ^ String.concat " " vars ^ " : Z, ")
  ^ string_of_atom_s (match f with Lit (_, a) -> a)

(** [print_axioms_in_p_s p] prints axioms.
    This is "SMTCoq version", this prints "Z" as the set. *)
let print_axioms_in_p_s p =
  let print_formula_s problem f = (** _s for "SMTCoq version". *)
    let vars = match f with
    | Lit (_, a) ->
      identifiers_in_atom a |> List.sort_uniq compare
      |> List.filter (fun i -> not (is_constant i)) in
    if List.length vars = 0 then ()
    else print_string ("forall " ^ String.concat " " vars ^ " : Z, ");
    print_string @@ string_of_atom_s (match f with Lit (_, a) -> a) in

  let print_axiom axiom = match axiom.af_formula with (** Prints single axiom. *)
  | Clause lits ->
    let name = match axiom.af_name with
    | N_word s -> Tptp_printer.show_plain_word s
    | _ -> failwith "print_axioms_in_p_s: not implemented" in
    let lit = List.hd lits in (* Only accepts one clause. *)
    print_string ("Axiom ax_" ^ name ^ " : "); (* Differenciate axioms from constants. *)
    print_formula_s p lit;
    print_endline "." in

  List.iter print_axiom p.axioms

let rec gen_coq_p_with_completion p use_lpo use_hammer use_smt =
  if use_hammer then gen_coq_p_with_completion_ham p
  else if use_smt then gen_coq_p_with_smt p
  else (
    (* prologue *)
    print_endline
      (String.concat "\n"
         [
           "(* Generated by tptp2coqp *)";
           "Require Import Setoid.";
           "From Completion Require Import Plugin.";
           "";
           "(* axioms *)";
           "Parameter G : Set.";
         ]);
    List.iter
      (fun (name, num_of_args) ->
        print_endline
          ("Parameter " ^ name ^ " : "
          ^ String.concat " -> " (List.init (num_of_args + 1) (fun _ -> "G"))
          ^ "."))
      p.constants;
    print_axioms_in_p p;
    print_endline "";
    let axiom_names =
      List.map
        (fun af ->
          match af.af_name with
          | N_word s -> Tptp_printer.show_plain_word s
          | _ -> failwith "gen_coq_p_with_completion: not implemented")
        p.axioms
    in
    let axiom_names = List.map (fun n -> "ax_" ^ n) axiom_names in
    let constants = List.map (fun (name, _) -> name) p.constants in
    print_endline
      ("Complete "
      ^ String.concat " " axiom_names
      ^ " : "
      ^ String.concat " " constants
      ^ " : hint");
    let problem_lit = match p.problem with Clause lits -> List.hd lits in
    print_endline ("  for (" ^ string_of_formula p problem_lit ^ ").");
    print_endline "";
    let goal_name = "check" in
    (* TODO *)
    print_endline
      (String.concat "\n"
         [
           "(* Goal *)";
           "Theorem " ^ goal_name ^ " : "
           ^ string_of_formula p problem_lit
           ^ ".";
           "Proof.";
           (if use_lpo then "  lpo_autorewrite with hint."
            else "  autorewrite with hint.");
           "  reflexivity.";
           "Qed.";
           "";
           "Check " ^ goal_name ^ ".";
           "";
         ]))

and gen_coq_p_with_completion_ham p =
  (* prologue *)
  print_endline
    (String.concat "\n"
       [
         "(* Generated by tptp2coqp *)";
         "Require Import Setoid.";
         "From Hammer Require Import Hammer.";
         "";
         "(* axioms *)";
         "Parameter G : Set.";
       ]);
  List.iter
    (fun (name, num_of_args) ->
      print_endline
        ("Parameter " ^ name ^ " : "
        ^ String.concat " -> " (List.init (num_of_args + 1) (fun _ -> "G"))
        ^ "."))
    p.constants;
  print_axioms_in_p p;
  print_endline "";
  let problem_lit = match p.problem with Clause lits -> List.hd lits in
  print_endline "";
  let goal_name = "check" in
  (* TODO *)
  print_endline
    (String.concat "\n"
       [
         "(* Goal *)";
         "Theorem " ^ goal_name ^ " : " ^ string_of_formula p problem_lit ^ ".";
         "Proof.";
         "  hammer.";
         "Qed.";
         "";
         "Check " ^ goal_name ^ ".";
         "";
       ])

and gen_coq_p_with_smt p =
  (* prologue *)
  print_endline
    (String.concat "\n"
       [
         "(* Generated by tptp2coqp *)";
         "Require Import SMTCoq.SMTCoq ZArith.";
         "Local Open Scope Z_scope.";
         "";
         "(* axioms *)";
       ]);
  List.iter
    (fun (name, num_of_args) ->
      print_endline
        ("Variable " ^ name ^ " : "
        ^ String.concat " -> " (List.init (num_of_args + 1) (fun _ -> "Z"))
        ^ "."))
    p.constants;
  print_axioms_in_p_s p;
  print_endline "";
  let axiom_names =
    List.map
      (fun af ->
        match af.af_name with
        | N_word s -> Tptp_printer.show_plain_word s
        | _ -> failwith "gen_coq_p_with_smt: not implemented")
      p.axioms
  in
  let axiom_names = List.map (fun n -> "ax_" ^ n) axiom_names in
  print_endline ("Add_lemmas " ^ String.concat " " axiom_names ^ ".");
  let problem_lit = match p.problem with Clause lits -> List.hd lits in
  print_endline "";
  let goal_name = "check" in
  print_endline
    (String.concat "\n"
       [
         "(* Goal *)";
         "Theorem " ^ goal_name ^ " : " ^ string_of_formula_s problem_lit ^ ".";
         "Proof.";
         "  smt.";
         "Qed.";
         "";
         "Check " ^ goal_name ^ ".";
         "";
       ])
