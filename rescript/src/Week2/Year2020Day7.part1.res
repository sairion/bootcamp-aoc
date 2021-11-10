// AoC 2020 Day 7 pt. 1 Procedure

open Belt

// clear fuchsia bags contain 5 wavy gray bags, 4 wavy white bags, 4 drab gray bags, 4 bright indigo bags.
// light red bags contain 2 clear fuchsia

/*
clear fuchsia
  5 wavy gray
  4 wavy white
  4 drab gray
  4 bright indigo

light red bag
  2 clear fuchsia
    5 wavy gray
    4 wavy white
    4 drab gray
    4 bright indigo
*/
let nil = ("nil", 0)
let cmpEq = (a, b) => a == b

let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day7.test.txt")->Js.String2.split("\n")

// function to create a tuple or update(add) existing tuple: (string, list<string>)
// li->appendToDict(k, el)
let appendToDict = (li, k, el: string) => {
  let entry = li->List.getAssoc(k, cmpEq)
  let next = entry->Option.mapWithDefault(list{el}, _li => _li->List.add(el))
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
      matched->Option.flatMap(xs =>
        switch xs {
        | [_, num, color] => num->Int.fromString->Option.map(n => (color, n))
        | _ => None
        }
      )
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

// TODO: replace reduce
let makeInvertedDict = (invertedDict, (dest, nodeStr)) => {
  let children = nodeStr->parseNodes
  children->Array.reduce(invertedDict, (d, (color, _)) => {
    if color == "nil" {
      d
    } else {
      d->appendToDict(color, dest)
    }
  })
}

// record visiting nodes in `Set.String` while traversing upwards.
let rec traverse = (invertedDict, node, visited) => {
  let s = visited->Set.String.add(node)
  let edges = invertedDict->List.getAssoc(node, cmpEq)

  edges->Option.mapWithDefault(s, es =>
    es->List.reduce(s, (s_, e) => {
      traverse(invertedDict, e, s_)
    })
  )
}

input
->Array.keepMap(matchNodes)
->Array.reduce(list{}, makeInvertedDict)
->traverse("shiny gold", Set.String.empty)
->Set.String.size
->(n => n - 1)
->Js.log
