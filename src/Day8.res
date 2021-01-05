open Belt

type instruction = Nop(int) | Acc(int) | Jmp(int)
type instState = {inst: instruction, count: int}
type accumulator = array<instState>
type accState = {
  insts: accumulator,
  line: int,
  value: int,
}

let getInstruction = s =>
  switch s {
  | Some([_, "nop", num]) => Some(Nop(num->Int.fromString->Option.getWithDefault(0)))
  | Some([_, "jmp", num]) => Some(Jmp(num->Int.fromString->Option.getWithDefault(0)))
  | Some([_, "acc", num]) => Some(Acc(num->Int.fromString->Option.getWithDefault(0)))
  | _ => None
  }

let parseLine = line => {
  line->Js.String2.match_(%re("/(nop|acc|jmp) (\+\d+|\-\d+)/"))->getInstruction
}

let nextInstruction = (instruction, line, currVal) => {
  switch instruction {
  | Nop(_) => (line + 1, currVal)
  | Acc(v) => (line + 1, currVal + v)
  | Jmp(l) => (line + l, currVal)
  }
}

let nextState = current => {
  let incCount = (insts, line) =>
    insts->Array.mapWithIndex((i, {inst, count} as state) =>
      if i == line {
        {inst: inst, count: count + 1}
      } else {
        state
      }
    )
  let mapNext = ({insts, line, value}) => {
    insts
    ->Array.get(line)
    ->Option.map(({inst}) => inst->nextInstruction(line, value))
    ->Option.map(((nextLine, nextValue)) => {
      insts: insts->incCount(line),
      line: nextLine,
      value: nextValue,
    })
  }
  current->Option.flatMap(mapNext)
}

let initialize = insts =>
  insts->Array.map(i => {inst: i, count: 0})->(i => Some({insts: i, line: 0, value: 0}))

let initialState =
  Node_fs.readFileAsUtf8Sync("res/day8.txt")
  ->Js_string2.split("\n")
  ->Array.keepMap(parseLine)
  ->initialize

let rec getFirstCycle = currentState => {
  let next = currentState->nextState
  next->Option.flatMap(({insts}) => {
    if insts->Array.some(({count}) => count == 2) {
      currentState
    } else {
      getFirstCycle(next)
    }
  })
}

let getFirstCycleMoment = current => {
  let rec cycle = current => {
    let next = current->nextState
    switch next {
    | Some({insts}) =>
      if insts->Array.some(({count}) => count == 2) {
        (current, next)
      } else {
        cycle(next)
      }
    | _ => (current, None)
    }
  }
  current->cycle
}

let rec findNoLoopRun = current => {
  let changeAccumulation = ({insts, line} as state) => {
    ...state,
    insts: insts->Array.mapWithIndex((i, {inst, count} as acc) =>
      switch (i == line, inst) {
      | (true, Nop(x)) => {inst: Jmp(x), count: count}
      | (true, Jmp(x)) => {inst: Nop(x), count: count}
      | _ => acc
      }
    ),
  }

  let (last, result) = current->Option.map(changeAccumulation)->getFirstCycleMoment
  switch result {
  | None => last
  | _ => findNoLoopRun(current->nextState)
  }
}

let first =
  initialState
  ->getFirstCycle
  ->Option.forEach(x => {
    Js.log(x)
    Js.log(x.value)
  })

let second = initialState->findNoLoopRun->Js.log
