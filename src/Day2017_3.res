/* --- Day 3: Spiral Memory ---
You come across an experimental new kind of memory stored on an infinite two-dimensional grid.

Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...
While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.

For example:

Data from square 1 is carried 0 steps, since it's at the access port.
Data from square 12 is carried 3 steps, such as: down, left, left.
Data from square 23 is carried only 2 steps: up twice.
Data from square 1024 must be carried 31 steps.
How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?

Your puzzle input is 347991. */

let sample1 = 1

type position = {down: int, left: int}
type directType = Up | Down | Left | Right
type data = {
  number: int,
  len: int,
  steps: position,
  direct: directType,
}
type grid = array<data>

let initialData = {number: 1, steps: {down: 0, left: 0}, len: 0, direct: Left}

let nextPos = ({down, left}, direct) =>
  switch direct {
  | Up => {down: down - 1, left: left}
  | Down => {down: down + 1, left: left}
  | Right => {down: down, left: left - 1}
  | Left => {down: down, left: left + 1}
  }

let nextStep = ({number, steps, direct} as data) => {
  {
    ...data,
    number: number + 1,
    steps: steps->nextPos(direct),
  }
}

let next = ({len} as data) => {
  let next = data->nextStep
  let {down, left} = next.steps
  if -down == len && left == len + 1 {
    {...next, direct: Down, len: len + 1}
  } else if down == len && left == len {
    {...next, direct: Right}
  } else if down == len && left == -len {
    {...next, direct: Up}
  } else if down == -len && left == -len {
    {...next, direct: Left}
  } else {
    next
  }
}
let logRet = x => {
  x->Js.log
  x
}

let findStep = number => {
  let rec nextStep = (number, data) => {
    if data.number == number {
      data
    } else {
      nextStep(number, data->next)
    }
  }
  nextStep(number, initialData)
}

findStep(1024)->Js.log

findStep(347991)->(s => s.steps->(s => (Js.Math.abs_int(s.down) + Js.Math.abs_int(s.left))->ignore))

/* --- Part Two ---
As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.

So, the first few squares' values are chosen as follows:

Square 1 starts with the value 1.
Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:

147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...
What is the first value written that is larger than your puzzle input? */

let sumNeighBors = (steps: position, previous: list<data>) => {
  open Belt.List

  let isNeighBor = (cand: data) =>
  { let downDiff = Js.Math.abs_int(steps.down - cand.steps.down)
    let leftDiff = Js.Math.abs_int(steps.left - cand.steps.left)

    Js.log4("(", steps.down, steps.left, ")") 
    Js.log4("(", cand.steps.down, cand.steps.left, ")")
    Js.log3(downDiff, leftDiff, cand.number)
    downDiff <= 1 &&
    leftDiff <= 1 
  }
  previous->keep(isNeighBor)->map(d => d.number)->reduce(0, (x, y) => x + y)
}

let nextStepP2 = previous => {
  open Belt.List
  let {steps, direct} as data = previous->headExn
  let nextSteps = steps->nextPos(direct)
  Js.log2("current head :", data)
  {
    ...data,
    number: sumNeighBors(nextSteps, previous),
    steps: nextSteps,
  }->(
    x => {
      Js.log2("next step :", x)
      x
    }
  )
}

let nextP2 = previous => {
  open Belt.List

  let {len} = previous->headExn

  let nextStep = nextStepP2(previous)
  let {down, left} = nextStep.steps
  let nextData = if -down == len && left == len + 1 {
    {...nextStep, direct: Down, len: len + 1}
  } else if down == len && left == len {
    {...nextStep, direct: Right}
  } else if down == len && left == -len {
    {...nextStep, direct: Up}
  } else if down == -len && left == -len {
    {...nextStep, direct: Left}
  } else {
    nextStep
  }

  previous->add(nextData)
}

let findStepP2 = input => {
  let rec nextStep = (stepList) => {
    open Belt.List
    switch stepList->head {
    | Some({number}) when number > input => number
    | _ => nextStep(stepList->nextP2)
    }
  }
  nextStep(list{initialData})
}

findStepP2(347991)->Js.log
