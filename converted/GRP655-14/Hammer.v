(* Generated by tptp2coqp *)
Require Import Setoid.
From Hammer Require Import Hammer.

(* axioms *)
Parameter G : Set.
Parameter ld : G -> G -> G.
Parameter mult : G -> G -> G.
Parameter rd : G -> G -> G.
Axiom f05 : forall A B C : G, (mult A (mult B (mult C B))) = (mult (mult (mult A B) C) B).
Axiom f04 : forall A B : G, (rd (mult A B) B) = A.
Axiom f03 : forall A B : G, (mult (rd A B) B) = A.
Axiom f02 : forall A B : G, (ld A (mult A B)) = B.
Axiom f01 : forall A B : G, (mult A (ld A B)) = B.


(* Goal *)
Theorem check : (mult (ld x1 x1) x0) = x0.
Proof.
  hammer.
Qed.

Check check.

