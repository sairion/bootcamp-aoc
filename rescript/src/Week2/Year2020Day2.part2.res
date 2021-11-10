// AoC 2020 Day 2 pt. 2 Procedure

// 1. split rows, and split each row into 3 parts : (indexes to check, character to check occurrence, string to test)
// 2. define `extractData` ~> (idx 1, idx 2, char, password)
// 3. define `getMatchingCount` (idx 1, idx 2, char, password) ~> (count of occurrence in idx 1 and idx 2)
// 3. keep tuple satisfies the condition, and get length of the list of tuples

open Belt

let input =
  Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day2.test.txt")
  ->Js.String2.split("\n")
  ->List.fromArray

// extract minimum occurrence, max occurrence and count of given char
// let extractData = line => {
//   let chunk = line->Js.String2.split(" ")->List.fromArray

//   switch chunk {
//   | list{idxes, char, password} => {
//       let idxes =
//         idxes
//         ->Js.String2.split("-")
//         ->Array.map(a => {
//           switch Int.fromString(a) {
//           | Some(num) => num
//           | None => 0
//           }
//         })
//       let idx1 = idxes->Array.getExn(0)
//       let idx2 = idxes->Array.getExn(1)

//       let char = char->Js.String2.substring(~from=0, ~to_=1)

//       (idx1, idx2, char, password)
//     }
//   | _ => (0, 0, "", "")
//   }
// }

// let getMatching = ((idx1, idx2, char, password)) => {
//   let cnt = list{idx1, idx2}->List.reduce(0, (cnt, idx) => {
//     cnt + (password->Js.String2.get(idx - 1) == char ? 1 : 0)
//   })

//   cnt == 1
// }

// input->List.map(extractData)->List.keep(getMatching)->List.length->Js.log

type parseResult = {
  idx1: int,
  idx2: int,
  char: string,
  password: string,
}

let parse = line => {
  let chunks = line->Js.String2.split(" ")

  switch chunks {
  | [range, char, password] => {
      let range = range->Js.String2.split("-")->Array.keepMap(str => str->Int.fromString)

      switch range {
      | [idx1, idx2] =>
        Some({
          idx1: idx1,
          idx2: idx2,
          char: char->Js.String2.substring(~from=0, ~to_=1),
          password: password,
        })
      | _ => None
      }
    }
  | _ => None
  }
}

let isCharAt = ((password, char), idx) => password->Js.String2.get(idx - 1) == char

let isValid = ({idx1, idx2, char, password}) => {
  let cond = (password, char)
  cond->isCharAt(idx1) != cond->isCharAt(idx2)
}

input->List.keepMap(parse)->List.keep(isValid)->List.length->Js.log
