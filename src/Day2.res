let convertToTuple = x => {
  switch x {
  | [min, max, ch, word] => {
      let min = Belt.Int.fromString(min)->Belt.Option.getExn
      let max = Belt.Int.fromString(max)->Belt.Option.getExn
      (min, max, ch, word)
    }
  | _ => //   rase Exception
    (0, 0, "", "aaa")
  }
}

let parsedList = Node_fs.readFileAsUtf8Sync("res/day2.txt")
->Js.String2.trim
->Js.String2.split("\n")
->Belt.Array.keepMap(s => {
  let parsed = s->Js.String2.trim->Js.String2.match_(%re("/(\d+)-(\d+) (\w): (\w+)/"))
  switch parsed {
  | None => {
      Js.log2("Parsing Fail :", s)
      None
    }
  | Some(l) => Some(Belt_Array.sliceToEnd(l, 1)->convertToTuple)
  }
})

let countChar = (ch, word): int => {
  let arr = Js.Array2.fromMap(Js.String2.castToArrayLike(word), x => x)
  let filtered = Belt.Array.keep(arr, x => x == ch)
  Belt.Array.length(filtered)
}

let isInRange = (min, max, number) => min <= number && number <= max

let isValid = ((min, max, ch, word)) => {
  //Js.log4(min, max, ch, word)
  let count = countChar(ch, word)
  let r = isInRange(min, max, count)

  //Js.log(r)
  r
}

let solution = parsedList->Belt.Array.keep(isValid)->Belt.Array.length

Js.log("Test 1 : ")
Js.log(isValid((2, 4, "s", "skss")))
//10-17 j: jjjjjjjjjfjjjjjjk
Js.log("Test 2 : ")
Js.log(isValid((10, 17, "j", "jjjjjjjjjfjjjjjjk")))
//2-3 x: fxpr
Js.log("Test 3 : ")
Js.log(isValid((2, 3, "x", "fxpr")))
//1-3 a: abcde
Js.log("Test 4 : ")
Js.log(isValid((1, 3, "a", "abcde")))
//1-3 b: cdefg
Js.log("Test 5 : ")
Js.log(isValid((1, 3, "b", "cdefg")))
//2-9 c: ccccccccc
Js.log("Test 6 : ")
Js.log(isValid((2, 9, "c", "ccccccccc")))
Js.log("Test 7 : ")
Js.log(isValid((2, 3, "e", "cdeeeefg")))

Js.log("Day 2 P1 solution : ")
Js.log(solution)

let isValidP2 = ((first, second, ch, word)) => {
  let r =
    (Js.String2.charAt(word, first - 1) == ch && Js.String2.charAt(word, second - 1) != ch) ||
      (Js.String2.charAt(word, first - 1) != ch && Js.String2.charAt(word, second - 1) == ch)

  Js.log4(first, second, ch, word)
  Js.log("result : ")
  Js.log(r)
  r
}

let solutionP2 = parsedList->Belt.Array.keep(isValidP2)->Belt.Array.length

Js.log("Day 2 P2 solution : ")
Js.log(solutionP2)
