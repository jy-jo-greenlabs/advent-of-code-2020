let inputStr = Node_fs.readFileAsUtf8Sync("res/day5.txt")
let seatStrs = inputStr->Js.String2.split("\n")
let strSample = "BBFFBBFRLL"
let makeArray = str => str->Js_string2.castToArrayLike->Js_array.fromMap(x => x)

let calcRowCol = strArr => {
  strArr
  ->Belt_Array.reduce((0, 127, 0, 7), ((minRow, maxRow, minCol, maxCol), char) => {
    switch char {
    | "F" => (minRow, maxRow - (maxRow - minRow + 1) / 2, minCol, maxCol)
    | "B" => (minRow + (maxRow - minRow + 1) / 2, maxRow, minCol, maxCol)
    | "L" => (minRow, maxRow, minCol, maxCol - (maxCol - minCol + 1) / 2)
    | "R" => (minRow, maxRow, minCol + (maxCol - minCol + 1) / 2, maxCol)
    | _ => Js.Exn.raiseError("Unexpected input.")
    }
  })
  ->(((r, _, c, _)) => (r, c))
}
let getSeatID = ((row, col)) => {
  row * 8 + col
}

let strToSeatID = str => str->makeArray->calcRowCol->getSeatID
let seatIDSet = seatStrs->Belt_Array.map(strToSeatID)->Belt_SetInt.fromArray

let solutionP1 = seatIDSet->Belt_SetInt.maximum->Js.log

let isNearByMySeatID = n => {
  seatIDSet->Belt_SetInt.some(x => x - 2 == n) && seatIDSet->Belt_SetInt.every(x => x - 1 != n)
}

let solutionP2 =
  seatIDSet
  ->Belt_SetInt.keep(isNearByMySeatID)
  ->Belt_SetInt.toArray
  ->Belt_Array.getExn(0)
  ->(x => x + 1)
Js.log(solutionP2)

/**
l->Belt_set.keepMap(a =>
{let existPlus2 = l->Belt_Array.keep(c => a + 2 == c)
                ->Belt_Array.length == 1;
 let notExistID = l->}))

Belt.List.keepMap(l, x => Belt.List.)
    ->Belt_List.flatten->Belt_List.flatten
    ->Belt_List.toArray
    ->Js.log**/
