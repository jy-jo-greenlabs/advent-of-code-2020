open Belt

type adapter = int

type adapters = array<int>

type choice = {
  adapter: adapter,
  diff: int,
}

let findChoicesForAllAdapter = adapters => {
  let rec findNextChoice = (choices, remain) =>
    switch remain {
    | [] => choices
    | _ => {
        let min = remain->Js.Math.minMany_int
        let remaining = remain->Array.keep(x => x != min)
        let nextMin = remaining->Js.Math.minMany_int
        let nextChoices = choices->List.add({adapter: min, diff: nextMin - min})
        findNextChoice(nextChoices, remaining)
      }
    }
  findNextChoice(list{}, adapters)
}

let addForChargingOutlet = adapters => {
  adapters->Array.concat([0])
}

let addBuiltinAdapter = adapters => {
  let max = adapters->Js.Math.maxMany_int
  adapters->Array.concat([max + 3])
}

let countDiff = c => {
  let findLen = cond => c->List.keep(cond)->List.length
  let a = findLen(({diff}) => diff == 1)
  let b = findLen(({diff}) => diff == 3)
  a * b
}
let adapters =
  Node_fs.readFileAsUtf8Sync("res/day10-sample-1.txt")
  ->Js.String2.split("\n")
  ->Array.keepMap(Int.fromString)

//adapters->addForChargingOutlet->addBuiltinAdapter->findChoicesForAllAdapter->List.toArray->Js.log

let adapters2 =
  Node_fs.readFileAsUtf8Sync("res/day10-sample-2.txt")
  ->Js.String2.split("\n")
  ->Array.keepMap(Int.fromString)

//adapters2->addForChargingOutlet->addBuiltinAdapter->findChoicesForAllAdapter->countDiff->Js.log

let p1 =
  Node_fs.readFileAsUtf8Sync("res/day10.txt")
  ->Js.String2.split("\n")
  ->Array.keepMap(Int.fromString)
  ->addForChargingOutlet
  ->addBuiltinAdapter
  ->findChoicesForAllAdapter
  ->countDiff
  ->Js.log

let combine = (a, b, condition) =>
  a->Array.reduce(list{}, (l, a0) => {
    b->Array.reduce(l, (l, b0) =>
      if condition(a0, b0) {
        l->List.add((a0, b0))
      } else {
        l
      }
    )
  })

//combine([1,2,3,4], [1,2,3,4], (a, b) => a < b)->List.toArray->Js.log
//[ [ 3, 4 ], [ 2, 4 ], [ 2, 3 ], [ 1, 4 ], [ 1, 3 ], [ 1, 2 ] ]
//0 1 2 3

//[0, 1, 2, 3, 4]->Array.slice(~offset=0, ~len=2)->Js.log//[ 0, 1 ]
//[0, 1, 2, 3, 4]->Array.sliceToEnd(2)->Js.log//[ 2, 3, 4 ]

let printList = l => {
  l->List.toArray->Array.map(List.toArray)->Js.log;
  l
}

let spliceBy3 = adapters => {
  open List
  let rec diff3 = (res,curr, l) => {
    switch l {
      |list{a, b, ...c} when b - a == 3 => diff3(res->add(curr->add(a)), list{}, list{b, ...c})
      |list{a, ...b} when b == list{} => Some(res->add(curr->add(a)))
      |list{a, ...b} => diff3(res, curr->add(a), b)
      |_=> None
    }
  }
  diff3(list{}, list{}, adapters)
  ->Option.getExn
  ->map(reverse)
}

let countArrangement = adapterList => {
  open List
  let rec countNext = remain => {
    switch remain {
      |list{a, b, c, d, ...e} when d - a == 3 =>
      countNext(list{d, ...e}) +.
      countNext(list{c, d, ...e}) +.
      countNext(list{b, c,d, ...e})
      |list{a, b, c, ...d} when c - a <= 3 =>
      countNext(list{c, ...d}) +.
      countNext(list{b, c, ...d})
      |list{a, b, ...c} when b - a <= 2 =>
      countNext(list{b, ...c})
      |_ => 1.
    }
  }
  Js.log(list{4,5,6,7}->countNext)
  adapterList
  ->spliceBy3
  ->printList
  ->map(countNext)
  ->x => {Js.log(x->List.toArray); x}
  ->reduce(1., (a, b) => a *. b)
}




Node_fs.readFileAsUtf8Sync("res/day10.txt")
->Js.String2.split("\n")
->Array.keepMap(Int.fromString)
->addForChargingOutlet
->addBuiltinAdapter
->Js.Array2.sortInPlaceWith((n1, n2) => n1 - n2)
->List.fromArray
->countArrangement
->Js.log;