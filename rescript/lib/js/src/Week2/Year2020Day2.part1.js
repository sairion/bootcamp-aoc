// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");

var input = Belt_List.fromArray(Fs.readFileSync("input/Week2/Year2020Day2.test.txt", "utf8").split("\n"));

function countChar(str, $$char) {
  return Belt_Array.keep(str.split(""), (function (str) {
                return str === $$char;
              })).length;
}

function parse(line) {
  var chunks = line.split(" ");
  if (chunks.length !== 3) {
    return ;
  }
  var range = chunks[0];
  var $$char = chunks[1];
  var password = chunks[2];
  var range$1 = Belt_Array.keepMap(range.split("-"), Belt_Int.fromString);
  return {
          min: Belt_Array.getExn(range$1, 0),
          max: Belt_Array.getExn(range$1, 1),
          char: $$char.substring(0, 1),
          password: password
        };
}

function isValid(param) {
  var count = countChar(param.password, param.char);
  if (param.min <= count) {
    return count <= param.max;
  } else {
    return false;
  }
}

console.log(Belt_List.length(Belt_List.keep(Belt_List.keepMap(input, parse), isValid)));

exports.input = input;
exports.countChar = countChar;
exports.parse = parse;
exports.isValid = isValid;
/* input Not a pure module */
