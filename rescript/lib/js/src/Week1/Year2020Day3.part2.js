// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_int32 = require("rescript/lib/js/caml_int32.js");

var input = Fs.readFileSync("input/Week1/Year2020Day3.test.txt", "utf8").split("\n");

var input$1 = Belt_Array.map(input, (function (s) {
        return s.split("");
      }));

var input$2 = Belt_List.fromArray(Belt_Array.map(input$1, (function (line) {
            return Belt_Array.map(line, (function (x) {
                          if (x === "#") {
                            return 1;
                          } else {
                            return 0;
                          }
                        }));
          })));

var mod_len = Belt_List.headExn(input$2).length;

var checklist = {
  hd: [
    1,
    1
  ],
  tl: {
    hd: [
      3,
      1
    ],
    tl: {
      hd: [
        5,
        1
      ],
      tl: {
        hd: [
          7,
          1
        ],
        tl: {
          hd: [
            1,
            2
          ],
          tl: /* [] */0
        }
      }
    }
  }
};

function filter_input(input, y_acc) {
  if (y_acc !== 1) {
    return Belt_List.keepWithIndex(input, (function (param, i) {
                  return Caml_int32.mod_(i, y_acc) === 0;
                }));
  } else {
    return input;
  }
}

function f(param) {
  var x_acc = param[0];
  return Belt_List.reduce(Belt_List.tailExn(filter_input(input$2, param[1])), [
                0,
                x_acc
              ], (function (param, item) {
                  var index = param[1];
                  var total = param[0];
                  var x = Caml_int32.mod_(index, mod_len);
                  if (item.length !== 0) {
                    return [
                            total + Belt_Array.getExn(item, x) | 0,
                            index + x_acc | 0
                          ];
                  } else {
                    return [
                            total,
                            0
                          ];
                  }
                }))[0];
}

console.log(Belt_List.reduce(Belt_List.map(checklist, f), 1, (function (acc, curr) {
            return acc * curr;
          })));

exports.input = input$2;
exports.mod_len = mod_len;
exports.checklist = checklist;
exports.filter_input = filter_input;
exports.f = f;
/* input Not a pure module */
