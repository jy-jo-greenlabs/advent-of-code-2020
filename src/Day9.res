open Belt

type numberSet = {
  n: float,
  previous: array<float>,
}
type numberSets = array<numberSet>

let sumOfTwoNotExists = ({n, previous}) => {
  //previous->Array.forEach(a => previous->Array.forEach(b => if a + b == n {Js.log3(a,b,n)} ));
  previous->Array.some(a => previous->Array.some(b => a +. b == n && a != b))->(x => !x)
}

let initialize = (arr, preamble) => {
  arr
  ->Array.keepMap(Float.fromString)
  ->(
    a =>
      a
      ->Array.mapWithIndex((i, x) =>
        switch preamble <= i {
        | true => Some({n: x, previous: a->Array.slice(~offset=i - preamble, ~len=preamble)})
        | false => None
        }
      )
      ->Array.keepMap(x => x)
  )
}

let inputs = Node_fs.readFileAsUtf8Sync("res/day9.txt")->Js_string2.split("\n")

let invalidNumber =
  inputs->initialize(25)->Array.getBy(sumOfTwoNotExists)->Option.mapWithDefault(0., ({n}) => n)

let findContiguousList = (allList, invalid) => {
  open List
  let rec find = (queue, numberList) => {
    let sum = queue->List.reduce(0., (s, n) => s +. n)
    if sum < invalidNumber {
      switch (numberList->head, numberList->tail) {
      | (Some(h), Some(t)) => queue->add(h)->find(t)
      | _ => None
      }
    } else if invalid < sum {
      queue
      ->(q => (q, q->length))
      ->(((q, l)) => q->take(l - 1))
      ->Option.flatMap(q => q->find(numberList))
    } else {
      Some(queue)
    }
  }
  list{}->find(allList)
}

let findMax = l => l->List.reduce(0., (m, n) => m > n ? m : n)

let findMin = l => l->List.reduce(99999999999999999999999999., (m, n) => m < n ? m : n)

let i =
  inputs
  ->Array.keepMap(Float.fromString)
  ->List.fromArray
  ->findContiguousList(invalidNumber)
  ->Option.map(l => l->findMax +. l->findMin)
  ->Js.log