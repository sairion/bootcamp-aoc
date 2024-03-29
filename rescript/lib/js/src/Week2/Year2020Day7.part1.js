// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Caml_obj = require("rescript/lib/js/caml_obj.js");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Belt_SetString = require("rescript/lib/js/belt_SetString.js");

var nil = [
  "nil",
  0
];

var cmpEq = Caml_obj.caml_equal;

var input = Fs.readFileSync("input/Week2/Year2020Day7.test.txt", "utf8").split("\n");

function appendToDict(li, k, el) {
  var entry = Belt_List.getAssoc(li, k, cmpEq);
  var next = Belt_Option.mapWithDefault(entry, {
        hd: el,
        tl: /* [] */0
      }, (function (_li) {
          return Belt_List.add(_li, el);
        }));
  return Belt_List.setAssoc(li, k, next, cmpEq);
}

function parseNodes(line) {
  if (line === "no other bags") {
    return [nil];
  } else {
    return Belt_Array.keepMap(Belt_Array.keep(line.replace(/[.,]/g, "").replace(/ ?bags?/g, "bag").replace(/bag\w?/g, "\n").split("\n"), (function (a) {
                      return a !== "";
                    })), (function (a) {
                  var matched = a.match(/(\d+) (.*)/);
                  return Belt_Option.flatMap(matched === null ? undefined : Caml_option.some(matched), (function (xs) {
                                if (xs.length !== 3) {
                                  return ;
                                }
                                var num = xs[1];
                                var color = xs[2];
                                return Belt_Option.map(Belt_Int.fromString(num), (function (n) {
                                              return [
                                                      color,
                                                      n
                                                    ];
                                            }));
                              }));
                }));
  }
}

function matchNodes(line) {
  var matched = line.match(/(.*) bags contain (.*).$/);
  return Belt_Option.flatMap(matched === null ? undefined : Caml_option.some(matched), (function (matchObj) {
                if (matchObj.length !== 3) {
                  return ;
                }
                var dest = matchObj[1];
                var nodeStr = matchObj[2];
                return [
                        dest,
                        nodeStr
                      ];
              }));
}

function makeInvertedDict(invertedDict, param) {
  var dest = param[0];
  var children = parseNodes(param[1]);
  return Belt_Array.reduce(children, invertedDict, (function (d, param) {
                var color = param[0];
                if (color === "nil") {
                  return d;
                } else {
                  return appendToDict(d, color, dest);
                }
              }));
}

function traverse(invertedDict, node, visited) {
  var s = Belt_SetString.add(visited, node);
  var edges = Belt_List.getAssoc(invertedDict, node, cmpEq);
  return Belt_Option.mapWithDefault(edges, s, (function (es) {
                return Belt_List.reduce(es, s, (function (s_, e) {
                              return traverse(invertedDict, e, s_);
                            }));
              }));
}

console.log(Belt_SetString.size(traverse(Belt_Array.reduce(Belt_Array.keepMap(input, matchNodes), /* [] */0, makeInvertedDict), "shiny gold", undefined)) - 1 | 0);

exports.nil = nil;
exports.cmpEq = cmpEq;
exports.input = input;
exports.appendToDict = appendToDict;
exports.parseNodes = parseNodes;
exports.matchNodes = matchNodes;
exports.makeInvertedDict = makeInvertedDict;
exports.traverse = traverse;
/* input Not a pure module */
