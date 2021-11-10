// AoC 2020 Day 4 pt. 1 Procedure

// 주어진 필드들이 모두 존재하는지 (일치 x) 찾는 문제
// 1. \n\n으로 split해서 구분한 후, 필드들의 이름값들을 Array.get(0) 으로 수집한 다음
// 2. mandatoryFields의 모든 필드들을 가지고 있는지 비교

open Belt

let input =
  Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day4.test.txt")->Js.String2.split("\n\n")

let mandatoryFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]->List.fromArray

let parse = line => {
  let chunks = line->Js.String2.replaceByRe(%re("/\\n/g"), " ")->Js.String2.split(" ")
  chunks->Array.map(c => c->Js.String2.split(":"))
}

let getFieldNames = fields => fields->Array.keepMap(f => f->Array.get(0))->List.fromArray

let checkFields = fieldKeys => {
  let hasField = key => fieldKeys->List.has(key, (a, b) => a == b)
  mandatoryFields->List.every(hasField)
}

input->Array.map(parse)->Array.map(getFieldNames)->Array.keep(checkFields)->Array.length->Js.log
