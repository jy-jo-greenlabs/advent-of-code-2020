type objType = OpenSquare | Tree | None
type position = {x: int, y: int}
type spot = {obj: objType, x: int, y: int}
type areaMap = {
  map: array<array<spot>>,
  width: int,
  height: int,
}

let getType = ch =>
  switch ch {
  | "." => OpenSquare
  | "#" => Tree
  | _ => None
  }

let getArea = m => {
  map: m,
  width: m->Belt_Array.getExn(0)->Belt_Array.length,
  height: m->Belt_Array.length,
}

let makeMap = strings =>
  strings->Belt_Array.mapWithIndex((y, row) =>
    row
    ->Js_string2.castToArrayLike
    ->Js_array.fromMap(x => x)
    ->Belt_Array.mapWithIndex((x, ch) => {x: x, y: y, obj: getType(ch)})
  )

type explorePos = {
  count: int,
  col: int,
  row: int,
  slope: position,
}
let parsedList = Node_fs.readFileAsUtf8Sync("res/day3.txt")
let strings = parsedList->Js.String2.split("\n")->makeMap
let area = getArea(strings)

let explore = (area, {count, col, row, slope}, mapRow) => {
  let cell = mapRow->Belt_Array.getExn(col)
  {
    col: if row == cell.y {
      mod(col + slope.x, area.width)
    } else {
      col
    },
    row: if row == cell.y {
      row + slope.y
    } else {
      row
    },
    count: if row == cell.y && cell.obj == Tree {
      count + 1
    } else {
      count
    },
    slope: slope,
  }
}

let slopeP1 = {x: 3, y: 1}
let solutionP1 = area.map->Belt_Array.reduce(
  {
    count: 0,
    col: 0,
    row: 0,
    slope: slopeP1,
  },
  explore(area),
)

Js.log(solutionP1.count)

let p2Slopes = [{x: 1, y: 1}, slopeP1, {x: 5, y: 1}, {x: 7, y: 1}, {x: 1, y: 2}]

let p2Init = p2Slopes->Belt_Array.map(s => {count: 0, col: 0, row: 0, slope: s})
let p2Results =
  p2Init
  ->Belt_Array.map(i => area.map->Belt_Array.reduce(i, explore(area)))
  ->Belt_Array.map(i => float(i.count))
Js.log(p2Results)

let p2Counts = p2Results->Belt_Array.reduce(1., (x, y) => x *. y)
Js.log(p2Counts)
