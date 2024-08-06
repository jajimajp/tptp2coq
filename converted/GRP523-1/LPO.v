(* Generated by tptp2coqp *)
Require Import Setoid.
From Completion Require Import Plugin.

(* axioms *)
Parameter G : Set.
Parameter divide : G -> G -> G.
Parameter inverse : G -> G.
Parameter multiply : G -> G -> G.
Axiom inverse : forall A B : G, (inverse A) = (divide (divide B B) A).
Axiom multiply : forall A B C : G, (multiply A B) = (divide A (divide (divide C C) B)).
Axiom single_axiom : forall A B C : G, (divide A (divide B (divide C (divide A B)))) = C.

Complete inverse multiply single_axiom : divide inverse multiply : hint
  for ((multiply (multiply a3 b3) c3) = (multiply a3 (multiply b3 c3))).

(* Goal *)
Theorem check : (multiply (multiply a3 b3) c3) = (multiply a3 (multiply b3 c3)).
Proof.
  lpo_autorewrite with hint.
  reflexivity.
Qed.

Check check.

