// AoC 2020 Day 8 pt. 1 Procedure

open Belt

/*
example)

nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
*/

type operations = Nop(int) | Acc(int) | Jmp(int)

// TODO: redux reducer-like ...
let parseLine = line => {
  switch line->Js.String2.split(" ") {
  | [op, num] =>
    Int.fromString(num)->Option.flatMap(n =>
      switch op {
      | "nop" => Some(Nop(n))
      | "acc" => Some(Acc(n))
      | "jmp" => Some(Jmp(n))
      | _ => None
      }
    )
  | _ => None
  }
}

let input =
  Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day8.test.txt")
  ->Js.String2.split("\n")
  ->Array.keepMap(parseLine)
  ->List.fromArray

let reducer = (acc, line, op) => {
  switch op {
  | Nop(_) => (acc, line + 1)
  | Acc(amount) => (acc + amount, line + 1)
  | Jmp(amount) => (acc, line + amount)
  }
}

let rec solve = (ops, acc, line, visited) => {
  let op = ops->List.getExn(line)
  let (acc, line) = reducer(acc, line, op)
  if visited->Set.Int.has(line) == false {
    let v2 = visited->Set.Int.add(line)
    solve(ops, acc, line, v2)
  } else {
    acc
  }
}

input->solve(0, 0, Set.Int.empty)->Js.log
