(* Generated by tptp2coqp *)
Require Import SMTCoq.SMTCoq ZArith.
Local Open Scope Z_scope.

(* axioms *)
Variable divide : Z -> Z -> Z.
Variable inverse : Z -> Z.
Variable multiply : Z -> Z -> Z.
Axiom inverse : forall A B : Z, (inverse A) = (divide (divide B B) A).
Axiom multiply : forall A B C : Z, (multiply A B) = (divide A (divide (divide C C) B)).
Axiom single_axiom : forall A B C : Z, (divide (divide A (divide B C)) (divide A B)) = C.

Add_lemmas inverse multiply single_axiom.

(* Goal *)
Theorem check : (multiply (inverse a1) a1) = (multiply (inverse b1) b1).
Proof.
  smt.
Qed.

Check check.

