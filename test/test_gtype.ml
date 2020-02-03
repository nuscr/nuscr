open! Base
open Nuscrlib
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

module Name = Name.Name_M
open Names

let%test "Normal Form Example" =
  let rnos = RoleName.of_string in
  let tvnos = TypeVariableName.of_string in
  let mkMsg name = {label= LabelName.of_string name; payload= []} in
  let m1 = mkMsg "m1" in
  let m2 = mkMsg "m2" in
  let m3 = mkMsg "m3" in
  let a = rnos "A" in
  let b = rnos "B" in
  let loop = tvnos "Loop" in
  let mkMG m c = MessageG (m, a, b, c) in
  let before =
    ChoiceG
      ( a
      , [ MuG (loop, ChoiceG (a, [mkMG m1 (TVarG loop); mkMG m2 EndG]))
        ; mkMG m3 EndG ] )
  in
  let after =
    ChoiceG
      ( a
      , [ mkMG m1
            (MuG (loop, ChoiceG (a, [mkMG m1 (TVarG loop); mkMG m2 EndG])))
        ; mkMG m2 EndG
        ; mkMG m3 EndG ] )
  in
  Poly.equal (normalise before) after
