open Belt

type rec bag = {
  color: string,
  contains: list<content>,
}
and content = {kind: bag, count: int}

let readFile = fileName => Node_fs.readFileAsUtf8Sync(fileName)
let splitByLF = input => input->Js.String2.split("\n")

let getObjectColor = sentence =>
  sentence
  ->Js.String2.match_(%re("/([\w\s]+) bags contain|contains/"))
  ->Option.flatMap(s => s->Array.get(1))

let getContainsPart = (sentence: string): option<string> =>
  sentence
  ->Js.String2.match_(%re("/[\w\s]+ bags (contain|contains) (.+)\./"))
  ->Option.flatMap(s => s->Array.get(2))

let splitContainerPart = s =>
  switch s {
  | Some(x) => x->Js.String2.splitByRe(%re("/, /"))
  | _ => []
  }

let getColorAndNumber = s =>
  switch s {
  | "no other bags" => None
  | x =>
    x
    ->Js.String2.match_(%re("/(\d+) ([\s\w]+) bag|bags/"))
    ->Option.map(a => (a->Array.get(1)->Option.flatMap(Int.fromString), a->Array.get(2)))
    ->Option.flatMap(x =>
      switch x {
      | (Some(n), Some(c)) => Some(c, n)
      | _ => None
      }
    )
  }
let getContentInfo = s =>
  s->getContainsPart->splitContainerPart->Array.keepMap(x => x->Option.flatMap(getColorAndNumber))

let generateBags = (sentences: array<string>) => {
  sentences
  ->Array.map(s => (s->getObjectColor, s->getContentInfo))
  ->Array.map(((clr, contents)) =>
    {
      "color": clr->Option.getWithDefault(""),
      "contains": contents,
    }
  )
}

//"dotted black bags contain no other bags."->getObjectColor->Js.log

//"dotted black bags contain no other bags."->getContainsPart->Js.log

//"light red bags contain 1 bright white bag, 2 muted yellow bags.\n"->getObjectColor->Js.log

//"light red bags contain 1 bright white bag, 2 muted yellow bags.\n"->getContainsPart->splitContainerPart->Js.log

//"1 bright white bag"->getColorAndNumber->Js.log
//"1 bright white bag"->Js.String2.match_(%re("/(\d+) ([\s\w]+) bag/"))->Js.log
//"2 muted yellow bags"->getColorAndNumber->Js.log

let bagKinds = (bags, tailTarget) => {
  let isContainTargetColor = (color, target, bags) => {
    bags
    ->Array.getBy(a => a["color"] == color)
    ->Option.mapWithDefault([], a => a["contains"])
    ->Array.some(((color, _)) => color == target)
  }
  let rec findingContainer = (set, target) => {
    bags
    ->Array.map(b => b["color"])
    ->Array.keep(c => isContainTargetColor(c, target, bags))
    ->Array.reduce(set->Set.String.add(target), findingContainer)
  }
  findingContainer(Set.String.empty, tailTarget)
}

//shiny gold, dark olive, vibrant plum
let bags = readFile("./res/day7.txt")->splitByLF->generateBags
Js.log(bagKinds(bags, "shiny gold")->Set.String.size - 1)

let bagVolume = (bags, mainColor) => {
  let rec countSubBag = color => {
    bags
    ->Array.getBy(b => b["color"] == color)
    ->Option.mapWithDefault([], b => b["contains"])
    ->Array.reduce(1, (cnt, (clr, count)) => cnt + count * countSubBag(clr))
  }
  countSubBag(mainColor) - 1
}
let b = bags->bagVolume("shiny gold")->Js.log
