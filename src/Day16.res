open Belt
type vrange = {
  min: int,
  max: int,
}

type field = {
  name: string,
  isInRange: int => bool,
}

let getField = line => {
  line
  ->Js.String2.match_(%re("/(\w+): (\d+)-(\d+) or (\d+)-(\d+)/"))
  ->(s =>
    switch s {
    | Some([_, n, a, b, c, d]) =>
      Some(n, Int.fromString(a), Int.fromString(b), Int.fromString(c), Int.fromString(d))
    | _ => None
    })
  ->(
    s =>
      switch s {
      | Some(name, Some(a), Some(b), Some(c), Some(d)) =>
        Some({
          name: name,
          isInRange: x => (a <= x && x <= b) || (c <= x && x <= d),
        })
      | _ => None
      }
  )
}

let readFields = input => {
  input->Js.String2.split("\n")->Array.keepMap(getField)
}

type ticket = array<int>

let getMyTicketNumbers = input => {
  input
  ->Js.String2.split("\n")
  ->Array.get(1)
  ->Option.getWithDefault("")
  ->Js.String2.split(",")
  ->Array.keepMap(Int.fromString)
}

type nearbyTicketsType = array<ticket>

let getNearbyNumbers = input => {
  open Js.String2
  open Array

  input->split("\n")->Array.sliceToEnd(1)->map(l => l->split(",")->keepMap(Int.fromString))
}

let sumError = (ticket, fields) => {
  open Array
  ticket
  ->Array.keep(n => fields->Array.every(f => n->f.isInRange->(x => !x)))
  ->reduce(0, (s, n) => s + n)
}

let fileInput = Node_fs.readFileAsUtf8Sync("res/day16.txt")

let solution =
  fileInput
  ->Js.String2.split("\n\n")
  ->(inputs =>
    switch inputs {
    | [a, b, c] => Some(a->readFields, b->getMyTicketNumbers, c->getNearbyNumbers)
    | _ => None
    })
  ->Option.map(((fields, _, tickets)) => {
    tickets->Array.map(ticket => ticket->sumError(fields))
  })
  ->Option.getWithDefault([])
  ->Array.reduce(0, (s, n) => s + n)
  ->Js.log
