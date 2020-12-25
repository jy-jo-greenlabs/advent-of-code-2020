type objType = OpenSquare | Tree | None

type position = {x: int, y: int}
type areaMap = {
  map: array<array<objType>>,
  width: int,
  height: int,
}

let p1Offset = {x: 3, y: 1}

let parsedList = Node_fs.readFileAsUtf8Sync("res/day3.txt")
let strings = parsedList->Js.String2.split("\n")

let getType = ch =>
  switch ch {
  | "." => OpenSquare
  | "#" => Tree
  | _ => None
  }

let parseRow = (str: Js.String2.t) => str->Js.String2.castToArrayLike->Js_array2.fromMap(getType)
let m = strings->Belt_Array.map(parseRow)
let area = {
  map: m,
  width: m->Belt_Array.getExn(0)->Belt_Array.length,
  height: m->Belt_Array.length,
}
let nextCol = curr => {
  let nextX = curr + p1Offset.x
  if area.width <= nextX {
    nextX - area.width
  } else {
    nextX
  }
}
type explorePos = {count: int, col: int}
let nextStep = (curr, row) => {
  count: switch row->Belt_Array.get(curr.col) {
  | Some(Tree) => curr.count + 1
  | _ => curr.count
  },
  col: nextCol(curr.col),
}

let exploreRes = area.map->Belt_Array.reduce({count: 0, col: 0}, nextStep)

Js.log(exploreRes)
