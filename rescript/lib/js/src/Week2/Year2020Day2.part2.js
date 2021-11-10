// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");

var input = Belt_List.fromArray(Fs.readFileSync("input/Week2/Year2020Day2.test.txt", "utf8").split("\n"));

function parse(line) {
  var chunks = line.split(" ");
  if (chunks.length !== 3) {
    return ;
  }
  var range = chunks[0];
  var $$char = chunks[1];
  var password = chunks[2];
  var range$1 = Belt_Array.keepMap(range.split("-"), Belt_Int.fromString);
  if (range$1.length !== 2) {
    return ;
  }
  var idx1 = range$1[0];
  var idx2 = range$1[1];
  return {
          idx1: idx1,
          idx2: idx2,
          char: $$char.substring(0, 1),
          password: password
        };
}

function isCharAt(param, idx) {
  return param[0][idx - 1 | 0] === param[1];
}

function isValid(param) {
  var cond_0 = param.password;
  var cond_1 = param.char;
  var cond = [
    cond_0,
    cond_1
  ];
  return isCharAt(cond, param.idx1) !== isCharAt(cond, param.idx2);
}

console.log(Belt_List.length(Belt_List.keep(Belt_List.keepMap(input, parse), isValid)));

exports.input = input;
exports.parse = parse;
exports.isCharAt = isCharAt;
exports.isValid = isValid;
/* input Not a pure module */
