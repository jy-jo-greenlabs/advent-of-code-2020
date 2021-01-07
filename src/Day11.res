open Belt

let mapInputs = Node_fs.readFileAsUtf8Sync("res/day11.txt")->Js.String2.split("\n")

let width = mapInputs->Belt.Array.get(0)->Option.mapWithDefault(0, w => w->String.length)
let height = mapInputs->Belt.Array.length

type point = {x: int, y: int}

module PointCmp = Belt.Id.MakeComparable({
  type t = point
  let seatId = p => p.x * width + p.y
  let cmp = (a, b) => a->seatId - b->seatId
})

type object = Occupied | Empty | Floor

let charToObj = char =>
  switch char {
  | "L" => Some(Empty)
  | "." => Some(Floor)
  | "#" => Some(Occupied)
  | _ => None
  }

let objToChar = obj =>
  switch obj {
  | Some(Empty) => "L"
  | Some(Floor) => "."
  | Some(Occupied) => "#"
  | _ => ""
  }

let initialize = () => {
  let initialMap = Belt.Map.make(~id=module(PointCmp))
  let addElement = (map, p, obj) => map->Map.set(p, obj)
  mapInputs->Array.reduceWithIndex(initialMap, (m, row, yi) =>
    row
    ->Js.String2.castToArrayLike
    ->Js.Array.fromMap(charToObj)
    ->Array.reduceWithIndex(m, (m, cell, xi) => m->addElement({x: xi, y: yi}, cell))
  )
}

let adjacentSeats = (c, seatingMap) => {
  seatingMap->Map.keep((p, _) =>
    Js.Math.abs_int(p.x - c.x) <= 1 && Js.Math.abs_int(p.y - c.y) <= 1 && p != c
  )
}

let nextObjStatus = (obj, adjacents) => {
  open Map
  let notOccupied = (_, obj) => obj != Some(Occupied)
  let occupiedSeatsCount = a => a->keep((_, obj) => obj == Some(Occupied))->size

  switch obj {
  | Some(Empty) when adjacents->every(notOccupied) => Some(Occupied)
  | Some(Occupied) when 4 <= adjacents->occupiedSeatsCount => Some(Empty)
  | _ => obj
  }
}

let nextMapStatus = seatingMap => {
  seatingMap->Map.mapWithKey((current, obj) => {
    obj->nextObjStatus(current->adjacentSeats(seatingMap))
  })
}

let rec findNoChangeState = seatingMap => {
  let next = seatingMap->nextMapStatus
  next == seatingMap ? seatingMap : next->findNoChangeState
}

let countOccupiedSeats = seatingMap => {
  open Map
  let occupied = (_, v) => v == Some(Occupied)
  seatingMap->keep(occupied)->size
}

let printMap = seatingMap => {
  let prnStr = ref("")
  let append = char => {
    prnStr.contents = prnStr.contents ++ char
  }

  Range.forEach(0, height - 1, y => {
    Range.forEach(0, width - 1, x =>
      seatingMap->Map.get({x: x, y: y})->Option.getExn->objToChar->append
    )
    append("\n")
  })
  prnStr.contents->Js.log
}

let validate = seatingMap => {
    open Map;
    let occupied = (_, obj) => obj == Some(Occupied)
    let occupiedSeatsCount = a => a->keep((_, obj) => obj == Some(Occupied))->size

    seatingMap->every((k, v) => {
        let adj = k->adjacentSeats(seatingMap)
        switch v {
        | Some(Empty) => adj
            ->some(occupied)
            ->b => {!b ? {Js.log("Validate fail at empty"); 
                        Js.log(k)}: b->ignore; b;}  
        | Some(Occupied) => 
        (adj->occupiedSeatsCount < 4) 
        -> b => {!b ? {Js.log("Valid fail at occupied.");
                        Js.log(k)}: b->ignore; b;}  
        | _ => true
        }
    })
}

Range.forEach(0, 3, Js.log)

let seatingMap = initialize()->findNoChangeState

Js.log("validate")
seatingMap->validate->Js.log

Js.log(seatingMap->countOccupiedSeats)
