(* Generated by tptp2coqp *)
Require Import SMTCoq.SMTCoq ZArith.
Local Open Scope Z_scope.

(* axioms *)
Variable double_divide : Z -> Z -> Z.
Variable inverse : Z -> Z.
Variable multiply : Z -> Z -> Z.
Axiom multiply : forall A B : Z, (multiply A B) = (inverse (double_divide B A)).
Axiom single_axiom : forall A B C : Z, (inverse (double_divide (inverse (double_divide (inverse (double_divide A B)) C)) (double_divide A C))) = B.

Add_lemmas multiply single_axiom.

(* Goal *)
Theorem check : (multiply (multiply a3 b3) c3) = (multiply a3 (multiply b3 c3)).
Proof.
  smt.
Qed.

Check check.

