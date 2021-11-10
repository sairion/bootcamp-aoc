// AoC 2020 Day 7 pt. 2 Procedure

open Belt

// clear fuchsia bags contain 5 wavy gray bags, 4 wavy white bags, 4 drab gray bags, 4 bright indigo bags.
// light red bags contain 2 clear fuchsia

/*
clear fuchsia
  5 wavy gray
  4 wavy white
  4 drab gray
  4 bright indigo

{
  cl-fu:
    {
      wa-gr: 5
      wa-wh: 4
      dr-gr: 4
      br-in: 4
    }
}
*/
let nil = ("nil", 0)
let cmpEq = (a, b) => a == b

let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day7.test.txt")->Js.String2.split("\n")

// function to create a tuple or update(add) existing tuple: (string, array<string>)
// li->appendToDict(k, el)
let appendToDict = (li, k, el: int) => {
  let entry = li->List.getAssoc(k, cmpEq)
  let next = switch entry {
  | Some(_li) => _li->List.add((k, el))
  | None => list{(k, el)}
  }
  li->List.setAssoc(k, next, cmpEq)
}

let parseNodes = line => {
  switch line {
  | "no other bags" => [nil]
  | line =>
    line
    ->Js.String2.replaceByRe(%re("/[.,]/g"), "")
    ->Js.String2.replaceByRe(%re("/ ?bags?/g"), "bag")
    ->Js.String2.replaceByRe(%re("/bag\w?/g"), "\n")
    ->Js.String2.split("\n")
    ->Array.keep(a => a !== "")
    ->Array.keepMap(a => {
      let matched = a->Js.String2.match_(%re("/(\d+) (.*)/"))
      switch matched {
      | Some([_, num, color]) => num->Int.fromString->Option.flatMap(n => Some((color, n)))
      | _ => None
      }
    })
  }
}

let matchNodes = line => {
  let matched = line->Js.String2.match_(%re("/(.*) bags contain (.*).$/"))
  matched->Option.flatMap(matchObj => {
    switch matchObj {
    | [_, dest, nodeStr] => Some(dest, nodeStr)
    | _ => None
    }
  })
}

// { a: { b: 1 }, c: { d: 1 }, ... }
// list{("color", list{("color2", 0), ("color3", 1)}), ... )}
let makeGraph = (graph, (dest, nodeStr)) => {
  let children = nodeStr->parseNodes->List.fromArray
  let innerGraph = children->List.reduce(list{}, (g, (color, qty)) => {
    if color == "nil" {
      g
    } else {
      g->appendToDict(color, qty)
    }
  })
  graph->List.concat(innerGraph)->List.concat(innerGraph)->List.setAssoc(dest, children, cmpEq)
}

let rec traverse = (graph, node) => {
  let cntInitial = 1
  let children = graph->List.getAssoc(node, cmpEq)

  switch children {
  | Some(cs) =>
    cs->List.reduce(cntInitial, (cnt, (child, qty)) => {
      cnt + qty * traverse(graph, child)
    })
  | None => 1
  }
}

input
->Array.keepMap(matchNodes)
->Array.reduce(list{}, makeGraph)
->traverse("shiny gold")
->(n => n - 1)
->Js.log
