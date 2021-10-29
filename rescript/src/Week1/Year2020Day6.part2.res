// AoC 2020 Day 6 pt. 2 Procedure

// 1. make a list contains string a-z
// 2. iterate through a to z, to check if a alphabet character is included in every line of a answer group
// 3. sum marked alphabets' count

open Js.String2
open Belt

let input =
  Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day6.test.txt")->split("\n\n")->List.fromArray

let getAsciiStringByRange = (startString, endString) => {
  let start = startString->charCodeAt(0)->Float.toInt
  let end = endString->charCodeAt(0)->Float.toInt
  Array.range(start, end)->List.fromArray->List.map(fromCharCode)
}

let alphabetRange = getAsciiStringByRange("a", "z")

/*
dagn I dea I da I abc...z
        da   da    da
*/

/*

let sum = arr => arr.reduce((acc, curr) => acc + curr, 0)
arr = [1, 2, 3, 4, 5, 6]

(head, (head, (head, Nil)))

          1 : 2 : 3 : 4 : 5 : 6 : Nil

          1 + 2 + 3 + 4 + 5 + 6 + 0

*/

let id = x => x

let markMatchingChars = row => {
  let rows = row->split("\n")
  let x = alphabetRange->List.map(char => rows->Array.every(row => row->includes(char)))
  // [a,b,c,d,...,z]
  // [true, false, false, true...]
  x->List.keep(id)->List.length
}

input->List.map(markMatchingChars)->List.reduce(0, (cnt, t) => cnt + t)->Js.log
