open Belt
open Js

let getGroupInputs = fIn => fIn->Js.String2.split("\n\n")
let getPersonInputs = grpIn => grpIn->Js.String2.split("\n")
let getAnswers = psIn =>
  psIn->String2.castToArrayLike->Js.Array.fromMap(x => x)->Belt.Set.String.fromArray

let getGrpAnswers = (fn, grpIn) => {
  let answers = grpIn->getPersonInputs->Belt.Array.map(getAnswers)
  let first = answers->Belt_Array.getExn(0)
  answers->Belt.Array.reduce(first, fn)
}

let grpInputs = Node_fs.readFileAsUtf8Sync("res/day6.txt")->getGroupInputs

let totalAnswer = answers =>
  answers->Belt.Array.map(Set.String.size)->Belt.Array.reduce(0, (x, y) => x + y)

let solveBy = fn => {
  grpInputs->Belt.Array.map(getGrpAnswers(fn))->totalAnswer->Js.log
}

let solutionP1 = solveBy(Belt.Set.String.union)

let solutionP2 = solveBy(Belt.Set.String.intersect)
