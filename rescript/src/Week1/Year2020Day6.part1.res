// AoC 2020 Day 6 pt. 1 Procedure
// 1. split with "\n\n"
// 2. split each group with "" and remove "\n"s, and use set to remove duplicates
// 3. calculate sum of each set size

open Js.String2
open Belt

let input =
  Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day6.test.txt")->split("\n\n")->List.fromArray

input
->List.map(row => row->split("")->Array.keep(str => str != "\n")->Set.String.fromArray)
->List.reduce(0, (cnt, set) => cnt + set->Set.String.size)
->Js.log
