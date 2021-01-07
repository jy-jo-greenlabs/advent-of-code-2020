let budget_list = list{1721, 979, 366, 299, 675, 1456}

module IntFindable = {
    type t = int
    let guard = (x, y) => x + y == 2020 && x != y
}

module IntTwoCombFinder = Util.MakeTwoCombsFinder(IntFindable)

// Belt.Array.keepMap :: array('a) => ('a, f) => array(option('a))

let findSolution = (l: array<int>) => {
  l
  ->IntTwoCombFinder.find
  ->Belt.Array.get(0)
  ->Belt.Option.map(((e0, e1)) => e0 * e1)
}
@bs.module("fs") external readFileSync: (string, string) => string = "readFileSync"

/**
let readFileSync = %raw(`
  function (fileName) {
    var fs = require('fs');
    var stream = fs.readFileSync(fileName, 'utf8');
    return stream;
  }
`)**/

let readStr = readFileSync("./res/day1.txt", "utf8")
let splitted = Js.String.split("\n", readStr)
let numArrays = splitted->Belt_Array.map(x => Belt.Int.fromString(x)->Belt.Option.getExn)
let ans = findSolution(numArrays)
Js.log(ans)

let findP2Solution = (l: list<int>) => {
  let (x0, x1, x2) =
    Belt.List.map(l, x0 => Belt.List.map(l, x1 => Belt.List.map(l, x2 => (x0, x1, x2))))
    ->Belt.List.flatten
    ->Belt.List.flatten
    ->Belt.List.keep(p => {
      let (x0, x1, x2) = p
      x0 != x1 && x1 != x2 && x0 != x2 && x0 + x1 + x2 == 2020
    })
    ->Belt.List.getExn(0)
  x0 * x1 * x2
}
let numList = Belt.List.fromArray(numArrays)
let arrs = findP2Solution(numList)
Js.log(arrs)
