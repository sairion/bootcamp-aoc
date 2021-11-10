// AoC 2020 Day 4 pt. 2 Procedure

// 필드들에 대한 validation 짜기
// 1. \n\n으로 split해서 구분한 후, 필드들의 이름값들을 Array.get(0) 으로 수집한 다음

open Belt

let input =
  Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day4.test.txt")->Js.String2.split("\n\n")

let split = line => {
  let chunks = line->Js.String2.replaceByRe(%re("/\\n/g"), " ")->Js.String2.split(" ")
  chunks
  ->Array.map(c => c->Js.String2.split(":"))
  ->Array.keepMap(arr =>
    switch arr {
    | [key, value] => Some((key, value))
    | _ => None
    }
  )
  ->List.fromArray
}

type height = Cm(int) | Inch(int)
type parseObject = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: height,
  hcl: string,
  ecl: string,
  pid: string,
  cid: string,
}

let parseHexColor = str => {
  switch Js.Re.test_(%re("/^#([0-9A-F]{6})$/i"), str) {
  | true => Some(str)
  | false => None
  }
}

let parsePid = str => {
  // let arr = str->Js.String2.split("") // TODO
  Some(str)
}

let between = (number, comp1, comp2) => {
  let ok = comp1 <= number && number <= comp2
  switch ok {
  | true => Some(number)
  | false => None
  }
}

let parseNumberBetween = (str, a, b) => {
  str->Int.fromString->Option.flatMap(between(_, a, b))
}

let parseHeight = str => {
  let endPos = str->Js.String2.length
  let unit = str->Js.String2.substring(~from=endPos - 2, ~to_=endPos)
  let num = str->Js.String2.substring(~from=0, ~to_=endPos - 2)->Int.fromString

  num->Option.flatMap(n =>
    switch unit {
    | "cm" => n->between(150, 193)->Option.flatMap(value => Some(Cm(value)))
    | "in" => n->between(59, 76)->Option.flatMap(value => Some(Inch(value)))
    | _ => None
    }
  )
}

let parseEyeColor = str =>
  switch str {
  | "amb"
  | "blu"
  | "brn"
  | "gry"
  | "grn"
  | "hzl"
  | "oth" =>
    Some(str)
  | _ => None
  }

let resolve = ((rcd, str), parseFn, updateFn) => {
  rcd->Option.flatMap(r =>
    str
    ->parseFn
    ->Option.flatMap(v => {
      updateFn(r, v)
    })
  )
}

let parseAndUpdateRcd = list{
  (
    "byr",
    (rcd, str) => {
      (rcd, str)->resolve(parseNumberBetween(_, 1920, 2002), (r, v) => Some({
        ...r,
        byr: v,
      }))
    },
  ),
  (
    "iyr",
    (rcd, str) => {
      (rcd, str)->resolve(parseNumberBetween(_, 2010, 2020), (r, v) => Some({
        ...r,
        iyr: v,
      }))
    },
  ),
  (
    "eyr",
    (rcd, str) => {
      (rcd, str)->resolve(parseNumberBetween(_, 2020, 2030), (r, v) => Some({
        ...r,
        eyr: v,
      }))
    },
  ),
  (
    "hgt",
    (rcd, str) => {
      (rcd, str)->resolve(parseHeight, (r, v) => Some({
        ...r,
        hgt: v,
      }))
    },
  ),
  (
    "hcl",
    (rcd, str) => {
      (rcd, str)->resolve(parseHexColor, (r, v) => Some({
        ...r,
        hcl: v,
      }))
    },
  ),
  (
    "ecl",
    (rcd, str) => {
      (rcd, str)->resolve(parseEyeColor, (r, v) => Some({
        ...r,
        ecl: v,
      }))
    },
  ),
  (
    "pid",
    (rcd, str) => {
      (rcd, str)->resolve(parsePid, (r, v) => Some({
        ...r,
        pid: v,
      }))
    },
  ),
  (
    "cid",
    (rcd, str) => {
      (rcd, str)->resolve(
        str => Some(str),
        (r, v) => Some({
          ...r,
          cid: v,
        }),
      )
    },
  ),
}

let parse = fields => {
  let initialRcd = {
    byr: 0,
    iyr: 0,
    eyr: 0,
    hgt: Cm(0),
    hcl: "",
    ecl: "",
    pid: "",
    cid: "",
  }

  fields->List.reduce(Some(initialRcd), (rcd, (k, v)) => {
    switch parseAndUpdateRcd->List.getAssoc(k, (a, b) => a == b) {
    | Some(fn) => fn(rcd, v)
    | None => rcd
    }
  })
}

input->Array.map(split)->Array.keepMap(parse)->Array.length->Js.log
