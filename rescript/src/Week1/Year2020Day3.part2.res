let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.test.txt")->Js.String2.split("\n")

// make every row strings to Array
let input = input->Belt.Array.map(s => s->Js.String2.split(""))

let input =
  input->Belt.Array.map(line => line->Belt.Array.map(x => x === "#" ? 1 : 0))->Belt.List.fromArray

// use mod for continuation
let mod_len = input->Belt.List.headExn->Belt.Array.length

// list of slope x/y movement
let checklist = list{(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)}

let filter_input = (input, y_acc) =>
  switch y_acc {
  | 1 => input
  | y_acc =>
    input->Belt.List.keepWithIndex((_, i) => {
      mod(i, y_acc) == 0
    })
  }

let f = ((x_acc, y_acc)) =>
  input
  ->filter_input(y_acc)
  ->Belt.List.tailExn
  ->Belt.List.reduce((0, x_acc), ((total, index), item) => {
    let x = index->mod(mod_len)
    switch item {
    | [] => (total, 0)
    | item => (total + Belt.Array.getExn(item, x), index + x_acc)
    }
  })
  ->(((total, _)) => total)
  ->Belt.Int.toFloat

// 접근방식
// 1. iteration skip을 할 수 있는 reduce를 만들까 했는데, 먼저 row를 filter (filter_input) 하는 게 로직 상 단순할 것 같아 선택
checklist->Belt.List.map(f)->Belt.List.reduce(1., (acc, curr) => acc *. curr)->Js.log

/*
List<(int, int)>
->float

f((int, int)) -> float

List<(int, int)> -> map(f)
List<float>
->float

*/
