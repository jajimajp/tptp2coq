(* Generated by tptp2coqp *)
Require Import Setoid.
From Completion Require Import Plugin.

(* axioms *)
Parameter G : Set.
Parameter f : G -> G -> G.
Parameter ld : G -> G -> G.
Parameter mult : G -> G -> G.
Parameter unit : G.
Axiom c08 : forall A B : G, (mult (mult A A) (mult B B)) = (mult (f A B) (f A B)).
Axiom c07 : forall A B C : G, (ld A (mult (mult B C) A)) = (mult (ld A (mult B A)) (ld A (mult C A))).
Axiom c06 : forall A B C D : G, (ld (mult A B) (mult A (mult B (mult C D)))) = (mult (ld (mult A B) (mult A (mult B C))) (ld (mult A B) (mult A (mult B D)))).
Axiom c05 : forall A B : G, (mult A B) = (mult B A).
Axiom c04 : forall A B : G, (ld A (mult A B)) = B.
Axiom c03 : forall A B : G, (mult A (ld A B)) = B.
Axiom c02 : forall A : G, (mult unit A) = A.
Axiom c01 : forall A : G, (mult A unit) = A.

Complete c08 c07 c06 c05 c04 c03 c02 c01 : f ld mult unit : hint
  for ((f a b) = (f b a)).

(* Goal *)
Theorem check : (f a b) = (f b a).
Proof.
  lpo_autorewrite with hint.
  reflexivity.
Qed.

Check check.

