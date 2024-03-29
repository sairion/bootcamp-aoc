// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Belt_SetInt = require("rescript/lib/js/belt_SetInt.js");

function parseLine(line) {
  var match = line.split(" ");
  if (match.length !== 2) {
    return ;
  }
  var op = match[0];
  var num = match[1];
  return Belt_Option.flatMap(Belt_Int.fromString(num), (function (n) {
                switch (op) {
                  case "acc" :
                      return {
                              TAG: /* Acc */1,
                              _0: n
                            };
                  case "jmp" :
                      return {
                              TAG: /* Jmp */2,
                              _0: n
                            };
                  case "nop" :
                      return {
                              TAG: /* Nop */0,
                              _0: n
                            };
                  default:
                    return ;
                }
              }));
}

var input = Belt_List.fromArray(Belt_Array.keepMap(Fs.readFileSync("input/Week2/Year2020Day8.test.txt", "utf8").split("\n"), parseLine));

function reducer(acc, line, op) {
  switch (op.TAG | 0) {
    case /* Nop */0 :
        return [
                acc,
                line + 1 | 0
              ];
    case /* Acc */1 :
        return [
                acc + op._0 | 0,
                line + 1 | 0
              ];
    case /* Jmp */2 :
        return [
                acc,
                line + op._0 | 0
              ];
    
  }
}

function solve(ops, _acc, _line, _visited) {
  while(true) {
    var visited = _visited;
    var line = _line;
    var acc = _acc;
    var op = Belt_List.getExn(ops, line);
    var match = reducer(acc, line, op);
    var line$1 = match[1];
    var acc$1 = match[0];
    if (Belt_SetInt.has(visited, line$1) !== false) {
      return acc$1;
    }
    var v2 = Belt_SetInt.add(visited, line$1);
    _visited = v2;
    _line = line$1;
    _acc = acc$1;
    continue ;
  };
}

console.log(solve(input, 0, 0, undefined));

exports.parseLine = parseLine;
exports.input = input;
exports.reducer = reducer;
exports.solve = solve;
/* input Not a pure module */
