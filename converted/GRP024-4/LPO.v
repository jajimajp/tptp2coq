(* Generated by tptp2coqp *)
Require Import Setoid.
From Completion Require Import Plugin.

(* axioms *)
Parameter G : Set.
Parameter commutator : G -> G -> G.
Parameter identity : G.
Parameter inverse : G -> G.
Parameter multiply : G -> G -> G.
Axiom commutator : forall X Y : G, (commutator X Y) = (multiply X (multiply Y (multiply (inverse X) (inverse Y)))).
Axiom right_inverse : forall X : G, (multiply X (inverse X)) = identity.
Axiom right_identity : forall X : G, (multiply X identity) = X.
Axiom associativity : forall X Y Z : G, (multiply (multiply X Y) Z) = (multiply X (multiply Y Z)).
Axiom left_inverse : forall X : G, (multiply (inverse X) X) = identity.
Axiom left_identity : forall X : G, (multiply identity X) = X.

Complete commutator right_inverse right_identity associativity left_inverse left_identity : commutator identity inverse multiply : hint
  for ((commutator (commutator a b) c) = (commutator a (commutator b c))).

(* Goal *)
Theorem check : (commutator (commutator a b) c) = (commutator a (commutator b c)).
Proof.
  lpo_autorewrite with hint.
  reflexivity.
Qed.

Check check.

