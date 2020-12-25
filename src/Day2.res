type pwP1 = {min: int, max: int, ch: string, word: string}

let convertToObj = x => {
  switch x {
  | [a0, a1, a2, a3] => {
      min: Belt.Int.fromString(a0)->Belt.Option.getExn,
      max: Belt.Int.fromString(a1)->Belt.Option.getExn,
      ch: a2,
      word: a3,
    }
  | _ => raise(Not_found)
  }
}

let parsedList = Node_fs.readFileAsUtf8Sync("res/day2.txt")
->Js.String2.trim
->Js.String2.split("\n")
->Belt.Array.keepMap(s => {
  s
  ->Js.String2.trim
  ->Js.String2.match_(%re("/(\d+)-(\d+) (\w): (\w+)/"))
  ->Belt_Option.map(a => Belt_Array.sliceToEnd(a, 1))
})

let countChar = (ch, word): int => {
  let arr = Js.Array2.fromMap(Js.String2.castToArrayLike(word), x => x)
  let filtered = Belt.Array.keep(arr, x => x == ch)
  Belt.Array.length(filtered)
}

let isInRange = (min, max, number) => min <= number && number <= max

let isValid = ({min, max, ch, word}) => {
  let count = countChar(ch, word)
  isInRange(min, max, count)
}

let solution = parsedList->Belt_Array.map(convertToObj)->Belt.Array.keep(isValid)->Belt.Array.length

type pwP2 = {first: int, second: int, ch: string, word: string}

let convertToP2 = x => {
  switch x {
  | [a0, a1, a2, a3] => {
      first: Belt.Int.fromString(a0)->Belt.Option.getExn,
      second: Belt.Int.fromString(a1)->Belt.Option.getExn,
      ch: a2,
      word: a3,
    }
  | _ => raise(Not_found)
  }
}

let isValidP2 = ({first, second, ch, word}) => {
  (Js.String2.charAt(word, first - 1) == ch && Js.String2.charAt(word, second - 1) != ch) ||
    (Js.String2.charAt(word, first - 1) != ch && Js.String2.charAt(word, second - 1) == ch)
}

let solutionP2 =
  parsedList->Belt_Array.map(convertToP2)->Belt.Array.keep(isValidP2)->Belt.Array.length

Js.log("Test 1 : ")
Js.log(isValid({min: 2, max: 4, ch: "s", word: "skss"}))
//10-17 j: jjjjjjjjjfjjjjjjk
Js.log("Test 2 : ")
Js.log(isValid({min: 10, max: 17, ch: "j", word: "jjjjjjjjjfjjjjjjk"}))
//2-3 x: fxpr
Js.log("Test 3 : ")
Js.log(isValid({min: 2, max: 3, ch: "x", word: "fxpr"}))
//1-3 a: abcde
Js.log("Test 4 : ")
Js.log(isValid({min: 1, max: 3, ch: "a", word: "abcde"}))
//1-3 b: cdefg
Js.log("Test 5 : ")
Js.log(isValid({min: 1, max: 3, ch: "b", word: "cdefg"}))
//2-9 c: ccccccccc
Js.log("Test 6 : ")
Js.log(isValid({min: 2, max: 9, ch: "c", word: "ccccccccc"}))
Js.log("Test 7 : ")
Js.log(isValid({min: 2, max: 3, ch: "e", word: "cdeeeefg"}))

Js.log("Day 2 P1 solution : ")
Js.log(solution)

Js.log("Day 2 P2 solution : ")
Js.log(solutionP2)
