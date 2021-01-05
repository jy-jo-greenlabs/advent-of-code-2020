open Belt

let getGroupInputs = fIn => fIn->Js.String2.split("\n\n")
let getPersonInputs = grpIn => grpIn->Js.String2.split("\n")
let getAnswers = psIn =>
  psIn->Js.String2.castToArrayLike->Js.Array.fromMap(x => x)->Set.String.fromArray

let getGrpAnswers = (fn, grpIn) => {
  let answers = grpIn->getPersonInputs->Array.map(getAnswers)
  let first = answers->Belt_Array.getExn(0)
  answers->Array.reduce(first, fn)
}

let grpInputs = Node_fs.readFileAsUtf8Sync("res/day6.txt")->getGroupInputs

let totalAnswer = answers => answers->Array.map(Set.String.size)->Array.reduce(0, (x, y) => x + y)

let solveBy = fn => {
  grpInputs->Array.map(getGrpAnswers(fn))->totalAnswer->Js.log
}

let solutionP1 = solveBy(Set.String.union)

let solutionP2 = solveBy(Set.String.intersect)
