// AoC 2020 Day 5 Procedure

// 1. replace each char to (F|L > 0, B|R > 1)
// 2. split every 7 chars into tuple with 2 elements: row (first 6 digits), column (remaining 3 digits)
// 3. map and calculate list element -> (ID = (row * 8) + column)
// 4. order by descending order and get head of the list

open Js.String2
open Belt

let input =
  Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day5.test.txt")->split("\n")->List.fromArray

let binaryToDecimal = binaryString => Js.Float.fromString("0b" ++ binaryString)

let replaceChars = str => str->replaceByRe(%re("/[FL]/g"), "0")->replaceByRe(%re("/[BR]/g"), "1")

let splitPosition = chars => (substring(chars, ~from=0, ~to_=7), substring(chars, ~from=7, ~to_=10))

let calculateId = ((row, col)) => {
  (row->binaryToDecimal *. 8. +. col->binaryToDecimal)->Float.toInt
}

let sorted =
  input
  ->List.map(replaceChars)
  ->List.map(splitPosition)
  ->List.map(calculateId)
  ->List.sort((a, b) => a - b)

// solution 1: imperative
let initial = sorted->List.getExn(0)

let (_, foundId) = sorted->List.reduce((0, 0), ((prevId, foundId), id) => {
  if foundId == 0 {
    if prevId == 0 || prevId + 1 == id {
      (id, 0)
    } else {
      (id, prevId + 1)
    }
  } else {
    (prevId, foundId)
  }
})

foundId->Js.log

// solution 2: use sliding window
let sorted2 = sorted->List.tailExn

let (before, _) =
  List.zip(sorted, sorted2)
  ->List.keep(((a, b)) => {
    b - a > 1
  })
  ->List.getExn(0)

(before + 1)->Js.log
