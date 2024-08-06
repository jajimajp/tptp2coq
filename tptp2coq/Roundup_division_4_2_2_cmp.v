Require Import Setoid.
From Completion Require Import Plugin.

(* axioms *)
Parameter G : Set.
Parameter z : G.
Parameter s : G -> G.
Parameter minus : G -> G -> G.
Parameter div : G -> G -> G.
Axiom minus_z_left : forall y, minus z y = z. (* ? *)
Axiom minus_z_right : forall x, minus x z = x.
Axiom minus_s : forall x y, minus (s x) (s y) = minus x y.
Axiom div_z : forall y, div z (s y) = z.
Axiom div_s : forall x y, div (s x) (s y) = s (div (minus x y) (s y)).

Complete minus_z_left minus_z_right minus_s div_z div_s : z s minus div : hint
  for (div (s (s (s (s z)))) (s (s z)) = s (s z)).

Theorem check : div (s (s (s (s z)))) (s (s z)) = s (s z).
Proof.
  lpo_autorewrite with hint.
  reflexivity.
Qed.

Check check.
