let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.test.txt")->Js.String2.split("\n")

// make every row strings to Array
let input = input->Belt.Array.map(s => s->Js.String2.split(""))

let input =
  input->Belt.Array.map(line => line->Belt.Array.map(x => x === "#" ? 1 : 0))->Belt.List.fromArray

// we need to calculate from the 2nd line
let input = input->Belt.List.tailExn

// use mod for continuation
let mod_len = input->Belt.List.headExn->Belt.Array.length

input
->Belt.List.reduce((0, 3), ((total, index), item) => {
  let x = index->mod(mod_len)

  switch item {
  | [] => (total, 0)
  | item => (total + Belt.Array.getExn(item, x), index + 3)
  }
})
->(((total, _)) => total->Js.log)
