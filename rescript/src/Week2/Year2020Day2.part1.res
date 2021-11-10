// AoC 2020 Day 2 pt. 1 Procedure

// 1. split rows, and split each row into 3 parts : (occurrence range, character to check occurrence, string to test)
// 2. define `extractData` (min, max, count of the char occurrence)
// 3. keep tuple satisfies the condition, and get length of the list of tuples

// 6-12 f: mqcccdhxfbrhfpf
/*
{
  min: int,
  max: int,
  char: string,
  password: string
}
*/

open Belt

let input =
  Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day2.test.txt")
  ->Js.String2.split("\n")
  ->List.fromArray

let countChar = (str, char) =>
  str->Js.String2.split("")->Array.keep(str => str == char)->Array.length

// // extract minimum occurrence, max occurrence and count of given char
// let extractData = line => {
//   let chunk = line->Js.String2.split(" ")->List.fromArray

//   switch chunk {
//   | list{range, char, password} => {
//       let range =
//         range
//         ->Js.String2.split("-")
//         ->Array.map(a => {
//           switch Int.fromString(a) {
//           | Some(num) => num
//           | None => 0
//           }
//         })

//       let min = range->Array.getExn(0)
//       let max = range->Array.getExn(1)
//       let char = char->Js.String2.substring(~from=0, ~to_=1)
//       let count = password->countChar(char)

//       Some(min, max, count)
//     }
//   | _ => None
//   }
// }

// let isValid = ((min, max, count)) => {
//   min <= count && count <= max
// }

// input->List.map(extractData)->List.keepMap(x => x->Option.map(isValid))->List.length->Js.log

// extract minimum occurrence, max occurrence and count of given char

type parseResult = {
  min: int,
  max: int,
  char: string,
  password: string,
}

let parse = line => {
  let chunks = line->Js.String2.split(" ")

  switch chunks {
  | [range, char, password] => {
      let range = range->Js.String2.split("-")->Array.keepMap(str => str->Int.fromString)

      let result: parseResult = {
        min: range->Array.getExn(0),
        max: range->Array.getExn(1),
        char: char->Js.String2.substring(~from=0, ~to_=1),
        password: password,
      }

      Some(result)
    }
  | _ => None
  }
}

let isValid = ({min, max, char, password}) => {
  let count = password->countChar(char)
  min <= count && count <= max
}

input->List.keepMap(parse)->List.keep(isValid)->List.length->Js.log
