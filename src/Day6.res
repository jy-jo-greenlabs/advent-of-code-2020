open Belt
open Js

let getGroupInputs = fIn => fIn->Js.String2.split("\n\n")
let getPersonInputs = grpIn => grpIn->Js.String2.split("\n")
let getAnswers = psIn => psIn->String2.castToArrayLike->Js.Array.fromMap(x => x)->Belt.Set.String.fromArray

let getGrpAnswers = grpIn => grpIn
    ->getPersonInputs
    ->Belt.Array.map(getAnswers)
    ->Belt.Array.reduce(Belt.Set.String.empty, Belt.Set.String.union)

let solutionP1 = Node_fs.readFileAsUtf8Sync("res/day6.txt")
                ->getGroupInputs
                ->Belt.Array.map(getGrpAnswers)
                ->Belt.Array.map(Set.String.size)
                ->Belt.Array.reduce(0, (x,y) => x+y)

Js.log(solutionP1)

let getGrpAnswersP2 = grpIn => {
    let answers = grpIn
        ->getPersonInputs
        ->Belt.Array.map(getAnswers);
    let first = answers->Belt_Array.getExn(0)
    answers->Belt.Array.reduce(first, Belt.Set.String.intersect)}

let solutionP2 = Node_fs.readFileAsUtf8Sync("res/day6.txt")
                ->getGroupInputs
                ->Belt.Array.map(getGrpAnswersP2)
                ->Belt.Array.map(Set.String.size)
                ->Belt.Array.reduce(0, (x,y) => x+y)
                ->Js.log





