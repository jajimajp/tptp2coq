(* Generated by tptp2coqp *)
Require Import Setoid.
From Completion Require Import Plugin.

(* axioms *)
Parameter G : Set.
Parameter ld : G -> G -> G.
Parameter mult : G -> G -> G.
Parameter rd : G -> G -> G.
Parameter unit : G.
Axiom c08 : forall A B : G, (mult (mult A B) A) = (mult A (mult B A)).
Axiom c07 : forall A B C : G, (mult A (mult B (mult B C))) = (mult (mult (mult A B) B) C).
Axiom c06 : forall A : G, (mult unit A) = A.
Axiom c05 : forall A : G, (mult A unit) = A.
Axiom c04 : forall A B : G, (rd (mult A B) B) = A.
Axiom c03 : forall A B : G, (mult (rd A B) B) = A.
Axiom c02 : forall A B : G, (ld A (mult A B)) = B.
Axiom c01 : forall A B : G, (mult A (ld A B)) = B.

Complete c08 c07 c06 c05 c04 c03 c02 c01 : ld mult rd unit : hint
  for ((mult (mult a b) (mult (mult c b) c)) = (mult (mult a (mult (mult b c) b)) c)).

(* Goal *)
Theorem check : (mult (mult a b) (mult (mult c b) c)) = (mult (mult a (mult (mult b c) b)) c).
Proof.
  lpo_autorewrite with hint.
  reflexivity.
Qed.

Check check.

