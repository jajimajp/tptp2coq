(* Generated by tptp2coqp *)
Require Import Setoid.
From Completion Require Import Plugin.

(* axioms *)
Parameter G : Set.
Parameter asoc : G -> G -> G -> G.
Parameter ld : G -> G -> G.
Parameter mult : G -> G -> G.
Parameter rd : G -> G -> G.
Parameter unit : G.
Axiom c09 : forall A B C : G, (asoc A B C) = (ld (mult A (mult B C)) (mult (mult A B) C)).
Axiom c08 : forall A B C : G, (mult (mult A B) C) = (mult (mult A C) (ld C (mult B C))).
Axiom c07 : forall A B C : G, (mult A (mult B C)) = (mult (rd (mult A B) A) (mult A C)).
Axiom c06 : forall A : G, (mult unit A) = A.
Axiom c05 : forall A : G, (mult A unit) = A.
Axiom c04 : forall A B : G, (rd (mult A B) B) = A.
Axiom c03 : forall A B : G, (mult (rd A B) B) = A.
Axiom c02 : forall A B : G, (ld A (mult A B)) = B.
Axiom c01 : forall A B : G, (mult A (ld A B)) = B.

Complete c09 c08 c07 c06 c05 c04 c03 c02 c01 : asoc ld mult rd unit : hint
  for ((mult (asoc a b c) (mult d e)) = (mult (mult (asoc a b c) d) e)).

(* Goal *)
Theorem check : (mult (asoc a b c) (mult d e)) = (mult (mult (asoc a b c) d) e).
Proof.
  lpo_autorewrite with hint.
  reflexivity.
Qed.

Check check.

