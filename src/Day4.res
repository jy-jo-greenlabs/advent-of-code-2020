
type heightType = CM(int)|INCH(int)
type eclType = AMB|BLU|BRN|GRY|GRN|HZL|OTH
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

let parseByr = byrInput => byrInput->Belt_Int.fromString
let validateByr = byr => switch byr {
  |Some(x) => if 1920 <= x && x <= 2002 {Some(x)} else {None}
  |_ => None
}
let parseIyr = iyrInput => iyrInput->Belt_Int.fromString
let validateIyr = iyr => switch iyr {
| Some(x) => if 2010 <= x && x <= 2020 {Some(x)} else {None}
| _ => None
}

let parseEyr = eyrInput => eyrInput->Belt_Int.fromString
let validateEyr = eyr => switch eyr {
| Some(x) => if 2020 <= x && x <= 2030 {Some(x)} else {None}
| _ => None
}

let parseHgt = hgtInput => 
{ hgtInput
  -> Js_string2.match_(%re("/(\d+)(\w+)/"))
  -> x => switch x {
    |Some([_, num, "cm"]) => {
    let parsed = num->Belt_Int.fromString;
    parsed->Belt_Option.map(n => CM(n))}
    | Some([_, num, "in"]) => {
    let parsed = num->Belt_Int.fromString;
    parsed->Belt_Option.map(n => INCH(n))}
  | _ => None}}

let validateHgt = hgt => switch hgt {
  |Some(CM(x)) => if 150 <= x && x <= 193 {hgt} else{None}
  |Some(INCH(x)) => if 59 <= x && x <= 76 {hgt} else{None}
  |_ => None
}

let parseHcl = hclInput => Some(hclInput)
let validateHcl = hcl => 
{
  hcl->Belt_Option.map(x => 
  x->Js_string2.match_(%re("/#[0-9a-f]{6}/"))
  ->Belt_Option.map(y => if y->Belt_Array.length == 1 {hcl} else {None}))
  ->Belt_Option.getExn
  ->Belt_Option.getExn
}


let parseEcl = eclInput => switch eclInput {
  |"amb" => Some(AMB)
  |"blu" => Some(BLU)
  |"brn" => Some(BRN)
  |"gry" => Some(GRY)
  |"grn" => Some(GRN)
  |"hzl" => Some(HZL)
  |"oth" => Some(OTH)
  |_ => None
}
let validateEcl = ecl => ecl

let parsePid = pidInput => Some(pidInput)
let validatePid = pid => switch pid {
  | Some(a) => {
    switch a->Js_string2.match_(%re("/\d{9}/")) {
      |Some(b) => 
        if b->Belt_Array.length == 1 {pid} else {None}
      |_ => None
    }
    
  }
  | _ => None
}


let parseCid = cidInput => Some(cidInput)
let validateCid = cid => cid


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

let validatePassport = ({byr,iyr,eyr,hgt,hcl,ecl,pid,cid}) => 
  {byr: Some(byr)->validateByr->Belt_Option.getExn,
   eyr: Some(eyr)->validateEyr->Belt_Option.getExn,
   hgt: Some(hgt)->validateHgt->Belt_Option.getExn,
   hcl: Some(hcl)->validateHcl->Belt_Option.getExn,
   ecl: Some(ecl)->validateEcl->Belt_Option.getExn,
   pid: Some(pid)->validatePid->Belt_Option.getExn,
   iyr: Some(iyr)->validateIyr->Belt_Option.getExn,
   cid: cid->validateCid,}

let inputStr = Node_fs.readFileAsUtf8Sync("res/day4.txt")
let passportStrs = inputStr->Js.String2.split("\n\n")
Js.log(passportStrs->Belt_Array.length)
let arrs = passportStrs -> Belt_Array.keepMap(st => 
switch st->parsePassport->validatePassport {
| exception Not_found => None
| p => Some(p)
})
Js.log(arrs->Belt_Array.length)
