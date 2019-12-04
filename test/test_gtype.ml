open! Base
open Nuscrlib
open Syntax
open Gtype

(*
 * let%test "Flatten Example" =
 *   let nos = Name.of_string in
 *   let mkMsg str = Message {name= nos str; payload= []} in
 *   let m1 = mkMsg "m1" in
 *   let m2 = mkMsg "m2" in
 *   let m3 = mkMsg "m3" in
 *   let mkMG m = MessageG (m, nos "A", nos "B", EndG) in
 *   let before =
 *     ChoiceG (nos "A", [ChoiceG (nos "A", [mkMG m1; mkMG m2]); mkMG m3])
 *   in
 *   let after = ChoiceG (nos "A", [mkMG m1; mkMG m2; mkMG m3]) in
 *   Poly.equal (flatten before) after
 *)

let%test "Normal Form Example" =
  let nos = Name.of_string in
  let mkMsg name = Message {name= nos name; payload= []} in
  let m1 = mkMsg "m1" in
  let m2 = mkMsg "m2" in
  let m3 = mkMsg "m3" in
  let mkMG m c = MessageG (m, nos "A", nos "B", c) in
  let before =
    ChoiceG
      ( nos "A"
      , [ MuG
            ( nos "Loop"
            , ChoiceG (nos "A", [mkMG m1 (TVarG (nos "Loop")); mkMG m2 EndG])
            )
        ; mkMG m3 EndG ] )
  in
  let after =
    ChoiceG
      ( nos "A"
      , [ mkMG m1
            (MuG
               ( nos "Loop"
               , ChoiceG
                   (nos "A", [mkMG m1 (TVarG (nos "Loop")); mkMG m2 EndG]) ))
        ; mkMG m2 EndG
        ; mkMG m3 EndG ] )
  in
  Poly.equal (normalise before) after
