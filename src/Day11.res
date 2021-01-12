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
  | Empty => "L"
  | Floor => "."
  | Occupied => "#"

  }

let initialize = () => {
  let initialMap = Belt.Map.make(~id=module(PointCmp))
  let addElement = (map, p, obj) => map->Map.set(p, obj)
  mapInputs->Array.reduceWithIndex(initialMap, (m, row, yi) =>
    row
    ->Js.String2.castToArrayLike
    ->Js.Array.fromMap(x => x)
    ->Array.keepMap(x => x->charToObj)
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
  let notOccupied = (_, obj) => obj != Occupied
  let occupiedSeatsCount = a => a->keep((_, obj) => obj == Occupied)->size

  switch obj {
  | Empty when adjacents->every(notOccupied) => Occupied
  | Occupied when 4 <= adjacents->occupiedSeatsCount => Empty
  | _ => obj
  }
}

let nextMapStatus = seatingMap => {
  seatingMap->Map.mapWithKey((current, obj) => {
    obj->nextObjStatus(current->adjacentSeats(seatingMap))
  })
}

let rec findNoChangeState = (seatingMap, nextMapStatus) => {
  let next = seatingMap->nextMapStatus
  next == seatingMap ? seatingMap : next->findNoChangeState(nextMapStatus)
}

let countOccupiedSeats = seatingMap => {
  open Map
  let occupied = (_, v) => v == Occupied
  seatingMap->keep(occupied)->size
}

let printMap = seatingMap => {
  let prnStr = ref("")
  let append = char => {
    prnStr.contents = prnStr.contents ++ char
  }

  Range.forEach(0, height - 1, y => {
    Range.forEach(0, width - 1, x =>
      seatingMap->Map.get({x: x, y: y})->Option.getWithDefault(Floor)->objToChar->append
    )
    append("\n")
  })
  prnStr.contents->Js.log
}

let validate = seatingMap => {
  open Map
  let occupied = (_, obj) => obj == Some(Occupied)
  let occupiedSeatsCount = a => a->keep((_, obj) => obj == Some(Occupied))->size

  seatingMap->every((k, v) => {
    let adj = k->adjacentSeats(seatingMap)
    switch v {
    | Some(Empty) => adj->some(occupied)
    | Some(Occupied) => adj->occupiedSeatsCount < 4
    | _ => true
    }
  })
}

Range.forEach(0, 3, Js.log)

let add = (a: point, b: point) => {x: a.x + b.x, y: a.y + b.y}

let direcs = [
    {x: -1, y: -1},
    {x: -1, y: 0},
    {x: -1, y: 1},
    {x: 0, y: -1},
    {x: 0, y: 1},
    {x: 1, y: -1},
    {x: 1, y: 0},
    {x: 1, y: 1},
  ]
  let isValidLoc = p => 0 <= p.x && p.x < width && 0 <= p.y && p.y < height
  let nextStep = (current, dir) => {
    let next = add(current, dir)
    if next->isValidLoc {
      Some(next, dir)
    } else {
      None
    }
  }

let occupiedCountFromAdjacent = (smap, curr:point) => {
  open Array

  direcs
  ->keepMap(d => curr->nextStep(d))
  ->keepMap(((p, _)) => smap->Map.get(p))
  ->keep(o => o == Occupied)
  ->length
}


let occupiedCountFromSight = (smap, curr: point) => {
  open Array

  let rec countOccupied = (current: point, dir: point) => {
    let currObj = smap->Map.get(current)
    let nextL = nextStep(current, dir)
    
    switch (currObj, nextL) {
    | (Some(Occupied), _) => 1
    | (Some(Empty), _) => 0
    | (Some(Floor), Some((n,_))) => countOccupied(n, dir)
    | _ => 0
    }
  }

  direcs
  ->keepMap(d => nextStep(curr, d))
  ->map(((c, d)) => countOccupied(c, d))
  ->reduce(0, (a, b) => a + b)
}

let nextObjStatus2 = (smap, curr, obj) => {
  let countSight = smap->occupiedCountFromSight(curr)
  let countAdjacent = smap->occupiedCountFromAdjacent(curr)
  if(curr.x == 2 && curr.y == 0)
  {
    Js.log4("(2,0)",obj->objToChar, countSight, countAdjacent)
  }
  
  switch obj {
  | Empty when countSight == 0 => Occupied
  | Occupied when 5 <= countSight => Empty
  | _ => obj
  }
}

let nextMapStatus2 = sMap => {
  let nextMap = sMap->Map.mapWithKey((current, obj) => {
    sMap->nextObjStatus2(current, obj)
  })
  nextMap
}
let rec findNoChangeState2 = (sMap, nextMapStatus) => {
  let next = sMap->nextMapStatus
  next->printMap
  next == sMap ? sMap : next->findNoChangeState2(nextMapStatus)
}

let countOccupiedSeats2 = sMap => {
  open Map
  let isOccupied = (_, v) => v == Occupied
  sMap->keep(isOccupied)->size}

let printMap2 = seatingMap => {
  let prnStr = ref("")
  let append = char => {
    prnStr.contents = prnStr.contents ++ char
  }

  Range.forEach(0, height - 1, y => {
    Range.forEach(0, width - 1, x =>
      seatingMap->Map.get({x: x, y: y})->Option.getWithDefault(Floor)->objToChar->append
    )
    append("\n")
  })
  prnStr.contents->Js.log

  seatingMap
}
Js.log("Height : ")
Js.log(height)
Js.log(width)
let initMap = initialize()

initMap->findNoChangeState2(nextMapStatus2)->countOccupiedSeats2->Js.log
/**
let result = initMap->findNoChangeState2(nextMapStatus2)

result->printMap2
result->countOccupiedSeats2->Js.log
**/