// AoC 2020 Day 5 Procedure

// 1. replace each char to (F|L > 0, B|R > 1)
// 2. split every 7 chars into tuple with 2 elements: row (first 6 digits), column (remaining 3 digits)
// 3. map and calculate list element -> (ID = (row * 8) + column)
// 4. order by descending order and get head of the list

open Js.String2
open Belt

let input =
  Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day5.sample.txt")->split("\n")->List.fromArray

let binaryToDecimal = binaryString => Js.Float.fromString("0b" ++ binaryString)

let replaceChars = str => str->replaceByRe(%re("/[FL]/g"), "0")->replaceByRe(%re("/[BR]/g"), "1")

let splitPosition = chars => (substring(chars, ~from=0, ~to_=7), substring(chars, ~from=7, ~to_=10))

let calculateId = ((row, col)) => {
  (row->binaryToDecimal *. 8. +. col->binaryToDecimal)->Float.toInt
}

input
->List.map(replaceChars)
->List.map(splitPosition)
->List.map(calculateId)
->List.sort((a, b) => b - a)
->List.head
->Js.log

// (string, string) => int
// map
// Option<(string, string)> => Option<int>
