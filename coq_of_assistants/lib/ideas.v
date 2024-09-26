(*
  Unimath of OpenAI
    Copyright (C) 2     024  James Michael DuPont

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

 *)


Require Export Preamble.
Definition String : UU := UU.
Definition R : UU := UU.
                               
Definition Request : UU := total2 (
    fun prompt : String => total2
   (fun tokens : nat => total2
   (fun temp : R => total2
   (fun top_k : nat => total2
   (fun top_n : nat => total2 (fun output : String => unit)))))).


Definition Forms := total2 (
                        (fun monsters : Set => total2
                                                 (fun treasure : Set => total2
                                                                          (fun doors: Set => total2
                                                                                               (fun hallways: Set => total2
                                                                                                                       (fun rooms: nat => total2
                                                                                                                                            (fun traps: nat => total2
                                                                                                                                                                 (fun output : String => unit)
                                                                                                                                                 
                      ))))))).


Definition Dungeon : UU := total2 (
                               fun entrance : nat => total2
                                                       (fun exits : R => unit)
                             ).

(*
room => Map => space in higher dimension = UU = type
(paths a b ) equivalent.
(path json-rep python-rep ) is saying these two types are equivalent.
total2 - connects two different types.
dirprod_paths = path down the tunnel directed corridor.

  path and rooms
  connect two rooms via path (
assoc

proposal : hlevels = levels of dungeon
hlevels = levels of types, rooms are types.

 *)        
Definition TowerDefenseGame : UU := total2 (
                                        fun layers_deep : nat => total2
                                                                   (fun temp : R => total2
                                                                                      (fun forms : Forms => total2             
                                                                                                              (fun pictures: nat => total2                     (fun traces: nat => total2
                                                                                                                                                                                     (fun styles: nat => total2
                                                                                                                                                                                                           (fun output : Dungeon => unit))))))).

Definition Game : UU := total2 (
                            fun play : nat => unit
                          ).
Definition WriteMeATowerDefenseGameQ : UU := total2 (
                                                 fun write : nat => total2 (
                                                                        fun TowerDefence :  TowerDefenseGame => total2 (
                                                                                                                    fun output : Game => unit))).
                                                                                                                                                       
Notation "Write me a tower defence game! " :=                            WriteMeATowerDefenseGameQ (at level 100).
Definition LLM : UU := UU.
Definition Tree: UU := UU.
Definition ProveGood : UU := total2 (
                                 fun response: LLM => total2(fun request: nat => total2(fun connected_spanning_tree : Tree => total2(fun min_complexity : nat => total2	(fun max_complexity : nat => total2
                                                                                                                                                                                                       (fun video_snippets : nat => total2
                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                      (fun output : Game => unit))))))).

Definition SendToLLM := UU.
  (*
    
   Chunk in 4000 tokens                     Generate serialization, traverse KG in the right order following the proof schema that gives us the attention graph,
  Reduce size abstracting details and rewriting, fold the data together            bottom up, starting with bits. abstracting the representations to tokens, 
  UU -> term -> universe of universes                                                take jsonserde from rust,
  lift and shift to coq using coq of rust
  lift json into unimath                   lift the asts into sexprs into json,

prove the connection between the json and the unimath.


follow the code of serlib which serializes coq in ocaml into sexpressions.
 lift that sexpression into json.                                                      serve the open ai assistent api endpoint using the dungeon game example in coq
as an assistent for ai and prove that the generated code is of quality.
write requirements
write specification
precondition post conditions
   *)                  
                  
  (* Definition JustDoit := ProveGood SendToLLM   WriteMeATowerDefenseGameQ            . *)
                 

(* sketches subset of pictures*)
       
(* Definition VernacDefinition : UU := total2 ( *)
(*        (pre : myFuncts)(name : myFuncts)(def : myFuncts) *)
                   
Definition RequestLLM : UU := total2 (
    fun prompt : String => total2
   (fun tokens : nat => total2
   (fun temp : R => total2
   (fun top_k : nat => total2
   (fun top_n : nat => total2 (fun output : String => unit)))))).


(* WriteMeATowerDefenseGame -> jsonserde *)

(* in coq
Define two sets of natural numbers from 1 to 10 *)
Definition t2 := nat.
Inductive foo1: Type :=
| nope
| yup (n:UU) (l:foo1).
Definition A := (yup t2 nope ).
Definition B := (yup  t2 A ).


(* Define a function that takes two sets A and B and returns a set of pairs  *)

(* Definition pair_sets (X Y : Set) : Set := *)
(*   { p : X * Y | fst p ∈ X /\ snd p ∈ Y }. *)

(* Define a function that prints a pair of natural numbers *)
(* Definition print_pair (p : nat * nat) : string := *)
(*   "(" ++ string_of_nat (fst p) ++ ", " ++ string_of_nat (snd p) ++ ")". *)

(* (* Define a function that prints a set of pairs of natural numbers *) *)
(* Definition print_set (S : Set) : string := *)
(*   match S with *)
(*   | {} => "∅" *)
(*   | {x} => print_pair x *)
(*   | _ => "{" ++ fold_right (fun x s => print_pair x ++ ", " ++ s) "" S ++ "}" *)
(*   end. *)

(* (* Combine A and B in pairs and print the result *) *)
(* Eval compute in print_set (pair_sets A B). *)
