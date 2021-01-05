type heightType = CM(int) | INCH(int)
type eclType = AMB | BLU | BRN | GRY | GRN | HZL | OTH
type passport = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: heightType,
  hcl: string,
  ecl: eclType,
  pid: string,
  cid: option<string>,
}
let passportTemplate = {
  byr: 1920,
  iyr: 2010,
  eyr: 2020,
  hgt: CM(150),
  hcl: "#ffffff",
  ecl: AMB,
  pid: "012345678",
  cid: None,
}

let parseByr = byrInput =>
  switch byrInput->Belt_Int.fromString {
  | Some(x) =>
    if 1920 <= x && x <= 2002 {
      Some(x)
    } else {
      None
    }
  | _ => None
  }
let parseIyr = iyrInput =>
  switch iyrInput->Belt_Int.fromString {
  | Some(x) =>
    if 2010 <= x && x <= 2020 {
      Some(x)
    } else {
      None
    }
  | _ => None
  }

let parseEyr = eyrInput =>
  switch eyrInput->Belt_Int.fromString {
  | Some(x) =>
    if 2020 <= x && x <= 2030 {
      Some(x)
    } else {
      None
    }
  | _ => None
  }

let parseHgt = hgtInput => {
  hgtInput
  ->Js_string2.match_(%re("/(\d+)(\w+)/"))
  ->(
    x =>
      switch x {
      | Some([_, num, _type]) =>
        switch (Belt_Int.fromString(num), _type) {
        | (Some(n), "cm") => Some(CM(n))
        | (Some(n), "in") => Some(INCH(n))
        | _ => None
        }
      | _ => None
      }->(
        a =>
          switch a {
          | Some(CM(x)) =>
            if 150 <= x && x <= 193 {
              a
            } else {
              None
            }
          | Some(INCH(x)) =>
            if 59 <= x && x <= 76 {
              a
            } else {
              None
            }
          | _ => None
          }
      )
  )
}

let parseHcl = hclInput =>
  hclInput
  ->Js_string2.match_(%re("/#[0-9a-f]{6}/"))
  ->Belt_Option.flatMap(y =>
    if y->Belt_Array.length == 1 {
      Some(hclInput)
    } else {
      None
    }
  )

let parseEcl = eclInput =>
  switch eclInput {
  | "amb" => Some(AMB)
  | "blu" => Some(BLU)
  | "brn" => Some(BRN)
  | "gry" => Some(GRY)
  | "grn" => Some(GRN)
  | "hzl" => Some(HZL)
  | "oth" => Some(OTH)
  | _ => None
  }

let parsePid = pidInput =>
  switch pidInput->Js_string2.match_(%re("/\d{9}/")) {
  | Some(_) => Some(pidInput)
  | _ => None
  }

let parseCid = cidInput => Some(cidInput)

let parsePassport = (s: string) =>
  s
  ->Js_string2.splitByRe(%re("/\s/"))
  ->Belt_Array.map(s => s->Belt_Option.getExn->Js_string2.split(":"))
  ->Belt_Array.reduce(passportTemplate, (passport, arr) => {
    switch arr {
    | ["byr", val] => {...passport, byr: parseByr(val)->Belt_Option.getExn}
    | ["iyr", val] => {...passport, iyr: parseIyr(val)->Belt_Option.getExn}
    | ["eyr", val] => {...passport, eyr: parseEyr(val)->Belt_Option.getExn}
    | ["hgt", val] => {...passport, hgt: parseHgt(val)->Belt_Option.getExn}
    | ["hcl", val] => {...passport, hcl: parseHcl(val)->Belt_Option.getExn}
    | ["ecl", val] => {...passport, ecl: parseEcl(val)->Belt_Option.getExn}
    | ["pid", val] => {...passport, pid: parsePid(val)->Belt_Option.getExn}
    | ["cid", val] => {...passport, cid: parseCid(val)}
    | _ => passport
    }
  })

let inputStr = Node_fs.readFileAsUtf8Sync("res/day4.txt")
let passportStrs = inputStr->Js.String2.split("\n\n")
Js.log(passportStrs->Belt_Array.length)
let arrs = passportStrs->Belt_Array.keepMap(st =>
  switch st->parsePassport {
  | exception Not_found => None
  | p => Some(p)
  }
)
Js.log(arrs->Belt_Array.length)
