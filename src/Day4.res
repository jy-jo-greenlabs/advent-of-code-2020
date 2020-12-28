type passport = {
  byr: option<string>,
  iyr: option<string>,
  eyr: option<string>,
  hgt: option<string>,
  hcl: option<string>,
  ecl: option<string>,
  pid: option<string>,
  cid: option<string>,
}
let passportTemplate = {
  byr: None,
  iyr: None,
  eyr: None,
  hgt: None,
  hcl: None,
  ecl: None,
  pid: None,
  cid: None,
}

let toPassport = (s: string) =>
  s
  ->Js_string2.splitByRe(%re("/\s/"))
  ->Belt_Array.map(s => s->Belt_Option.getExn->Js_string2.split(":"))
  ->Belt_Array.reduce(passportTemplate, (passport, arr) => {
    switch arr {
    | ["byr", val] => {...passport, byr: Some(val)}
    | ["iyr", val] => {...passport, iyr: Some(val)}
    | ["eyr", val] => {...passport, eyr: Some(val)}
    | ["hgt", val] => {...passport, hgt: Some(val)}
    | ["hcl", val] => {...passport, hcl: Some(val)}
    | ["ecl", val] => {...passport, ecl: Some(val)}
    | ["pid", val] => {...passport, pid: Some(val)}
    | ["cid", val] => {...passport, cid: Some(val)}
    | _ => passport
    }
  })

let isValid = passport => {
  switch passport {
  | {
      byr: Some(_),
      iyr: Some(_),
      eyr: Some(_),
      hgt: Some(_),
      hcl: Some(_),
      ecl: Some(_),
      pid: Some(_),
    } => true
  | _ => false
  }
}

let inputStr = Node_fs.readFileAsUtf8Sync("res/day4.txt")
let passportStrs = inputStr->Js.String2.split("\n\n")
let solution = passportStrs->Belt_Array.map(toPassport)->Belt_Array.keep(isValid)->Belt_Array.length

Js.log(solution)

let numStrPass = (s, p) =>
  s->Belt_Option.getWithDefault("")->Belt_Int.fromString->Belt_Option.mapWithDefault(false, p)

let isBirthValid = ({byr}) => byr->numStrPass(x => 1920 <= x && x <= 2002)
let isIssueValid = ({iyr}) => iyr->numStrPass(x => 2010 <= x && x <= 2020)
let isExpValid = ({eyr}) => eyr->numStrPass(x => 2020 <= x && x <= 2030)
let isHeightValid = ({hgt}) =>
  hgt
  ->Belt_Option.getWithDefault("")
  ->Js_string2.match_(%re("/(\d+)(\w+)/"))
  ->(
    x =>
      switch x {
      | Some([_, num, "cm"]) => Some(num)->numStrPass(x => 150 <= x && x <= 193)
      | Some([_, num, "in"]) => Some(num)->numStrPass(x => 59 <= x && x <= 76)
      | _ => false
      }
  )
let isHairColorValid = ({hcl}) =>
  hcl
  ->Belt_Option.getWithDefault("")
  ->Js_string2.match_(%re("/#[0-9a-f]{6}/"))
  ->Belt_Option.getWithDefault([])
  ->Js_array.length
  ->(n => n == 1)

let isEyeColorValid = ({ecl}) =>
  switch ecl->Belt_Option.getWithDefault("") {
  | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true
  | _ => false
  }
let isPidValid = ({pid}) => {
  pid
  ->Belt_Option.getWithDefault("")
  ->Js_string2.match_(%re("/\d{9}/"))
  ->Belt_Option.getWithDefault([])
  ->Belt_Array.length
  ->(x => 1 == x)
}
Js.log(isPidValid({...passportTemplate, pid: Some("123456789")}))

Js.log(isPidValid({...passportTemplate, pid: Some("123456789")}))
let isValid2 = pass => {
  let b1 = isBirthValid(pass)
  let b2 = isIssueValid(pass)
  let b3 = isExpValid(pass)
  let b4 = isHeightValid(pass)
  let b5 = isHairColorValid(pass)
  let b6 = isEyeColorValid(pass)
  let b7 = isPidValid(pass)

  //Js.log(pass)
  //Js.logMany([b1, b2, b3, b4, b5, b6, b7])
  b1 && b2 && b3 && b4 && b5 && b6 && b7
}

let inputStr = Node_fs.readFileAsUtf8Sync("res/day4.txt")
let passportStrs = inputStr->Js.String2.split("\n\n")
let solution =
  passportStrs->Belt_Array.map(toPassport)->Belt_Array.keep(isValid2)->Belt_Array.length

Js.log(solution)
