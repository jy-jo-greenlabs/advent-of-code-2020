open Belt

type adapter = float

type adapters = array<float>

type choice = {
  adapter: adapter,
  diff: float,
}

let findChoices = adapters => {
  let rec findNextChoice = (choices, remain) =>
    switch remain {
    | [] => choices
    | _ => {
        let min = remain->Js.Math.minMany_float
        let remaining = remain->Array.keep(x => x != min)
        let nextMin = remaining->Js.Math.minMany_float
        let nextChoices = choices->List.add({adapter:min, diff:nextMin -. min})
        findNextChoice(nextChoices,remaining)
      }
    }
    findNextChoice(list{}, adapters)
}

let addForChargingOutlet = adapters => {
    adapters->Array.concat([0.])
}

let addBuiltinAdapter = adapters => {
    let max = adapters->Js.Math.maxMany_float
    adapters->Array.concat([max +. 3.])
}

let countDiff = c => {
    let findLen = cond => c->List.keep(cond)->List.length
    let a = findLen(({diff}) => diff == 1.)
    let b = findLen(({diff}) => diff == 3.)
    a * b
}
let adapters =
  Node_fs.readFileAsUtf8Sync("res/day10-sample-1.txt")
  ->Js.String2.split("\n")
  ->Array.keepMap(Float.fromString)

adapters
->addForChargingOutlet
->addBuiltinAdapter
->findChoices->List.toArray->Js.log

let adapters2 =
  Node_fs.readFileAsUtf8Sync("res/day10-sample-2.txt")
  ->Js.String2.split("\n")
  ->Array.keepMap(Float.fromString)

  adapters2
->addForChargingOutlet
->addBuiltinAdapter
->findChoices->countDiff->Js.log

let p1 = 
Node_fs.readFileAsUtf8Sync("res/day10.txt")
  ->Js.String2.split("\n")
  ->Array.keepMap(Float.fromString)
  ->addForChargingOutlet
  ->addBuiltinAdapter
  ->findChoices
  ->countDiff
  ->Js.log