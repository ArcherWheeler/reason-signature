// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var $$Map       = require("bs-platform/lib/js/map.js");
var Char        = require("bs-platform/lib/js/char.js");
var List        = require("bs-platform/lib/js/list.js");
var Block       = require("bs-platform/lib/js/block.js");
var Curry       = require("bs-platform/lib/js/curry.js");
var React       = require("react");
var $$String    = require("bs-platform/lib/js/string.js");
var Caml_obj    = require("bs-platform/lib/js/caml_obj.js");
var Caml_float  = require("bs-platform/lib/js/caml_float.js");
var Pervasives  = require("bs-platform/lib/js/pervasives.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");

function rollAverage(n, weightedList, list) {
  var fl = n;
  if (weightedList && list) {
    return /* :: */[
            (fl * weightedList[0] + list[0]) / (fl + 1.0),
            rollAverage(n, weightedList[1], list[1])
          ];
  } else {
    return /* [] */0;
  }
}

function averageLists(_$staropt$star, _lists) {
  while(true) {
    var lists = _lists;
    var $staropt$star = _$staropt$star;
    var n = $staropt$star ? $staropt$star[0] : 1;
    if (lists) {
      var match = lists[1];
      var listA = lists[0];
      if (match) {
        _lists = /* :: */[
          rollAverage(n, listA, match[0]),
          match[1]
        ];
        _$staropt$star = /* Some */[n + 1 | 0];
        continue ;
        
      } else {
        return listA;
      }
    } else {
      return /* [] */0;
    }
  };
}

function printFloats(lst) {
  return List.fold_left((function (acc, x) {
                return acc + (", " + Pervasives.string_of_float(x));
              }), "", lst);
}

function verifySignature(trained, canidate, fn) {
  var averageDiffs = averageLists(/* None */0, trained);
  var percentageDiff = function (correct, other) {
    return Math.abs(other - correct) / correct;
  };
  if (averageDiffs) {
    if (canidate) {
      var percentageDiffList = List.map2(percentageDiff, averageDiffs, canidate);
      console.log(printFloats(percentageDiffList));
      return List.for_all(fn, percentageDiffList);
    } else {
      return /* true */1;
    }
  } else {
    return /* false */0;
  }
}

function codeOf(keyEv) {
  return keyEv[0];
}

function timeOf(keyEv) {
  return keyEv[1];
}

function removeDuplicates(_actions) {
  while(true) {
    var actions = _actions;
    if (actions) {
      var a = actions[0];
      var exit = 0;
      if (a.tag) {
        var rest = actions[1];
        var kc = a[0];
        if (kc > 90 || kc < 65) {
          _actions = rest;
          continue ;
          
        } else if (rest) {
          var match = rest[0];
          if (match.tag) {
            if (kc === match[0]) {
              _actions = /* :: */[
                /* KeyUp */Block.__(1, [
                    kc,
                    match[1]
                  ]),
                rest[1]
              ];
              continue ;
              
            } else {
              exit = 1;
            }
          } else {
            exit = 1;
          }
        } else {
          exit = 1;
        }
      } else {
        var rest$1 = actions[1];
        var kc$1 = a[0];
        if (kc$1 > 90 || kc$1 < 65) {
          _actions = rest$1;
          continue ;
          
        } else if (rest$1) {
          var match$1 = rest$1[0];
          if (match$1.tag) {
            exit = 1;
          } else if (kc$1 === match$1[0]) {
            _actions = /* :: */[
              /* KeyDown */Block.__(0, [
                  kc$1,
                  match$1[1]
                ]),
              rest$1[1]
            ];
            continue ;
            
          } else {
            exit = 1;
          }
        } else {
          exit = 1;
        }
      }
      if (exit === 1) {
        return /* :: */[
                a,
                removeDuplicates(actions[1])
              ];
      }
      
    } else {
      return /* [] */0;
    }
  };
}

function verifyKeys(str, keyPresses) {
  var writtenString = List.fold_left((function (acc, param) {
          return acc + Char.escaped(Char.chr(param[0]));
        }), "", keyPresses);
  return +(str === $$String.lowercase(writtenString));
}

function pairOff(actions) {
  if (actions) {
    var match = actions[0];
    if (match.tag) {
      return /* [] */0;
    } else {
      var match$1 = actions[1];
      if (match$1) {
        var match$2 = match$1[0];
        if (match$2.tag) {
          return /* :: */[
                  /* KeyPress */[
                    match[0],
                    match[1],
                    match$2[1]
                  ],
                  pairOff(match$1[1])
                ];
        } else {
          return /* [] */0;
        }
      } else {
        return /* [] */0;
      }
    }
  } else {
    return /* [] */0;
  }
}

var compare = Caml_obj.caml_compare;

var IntCompare = /* module */[/* compare */compare];

var IntMap = $$Map.Make(IntCompare);

function groupBy(fn) {
  var partial_arg = IntMap[/* empty */0];
  return (function (param) {
      return List.fold_left((function (map, element) {
                    var key = Curry._1(fn, element);
                    var current = Curry._2(IntMap[/* mem */2], key, map) ? Curry._2(IntMap[/* find */21], key, map) : /* [] */0;
                    var append = /* :: */[
                      element,
                      current
                    ];
                    return Curry._3(IntMap[/* add */3], key, append, map);
                  }), partial_arg, param);
    });
}

function extractKeyPresses(actions) {
  var keyEvents = removeDuplicates(actions);
  var keyMap = groupBy(codeOf)(keyEvents);
  var keyPressMap = Curry._2(IntMap[/* map */22], (function (lst) {
          return pairOff(List.sort((function (a, b) {
                            return Caml_float.caml_float_compare(timeOf(a), timeOf(b));
                          }), lst));
        }), keyMap);
  var keyPresses = List.concat(List.map((function (prim) {
              return prim[1];
            }), Curry._1(IntMap[/* bindings */16], keyPressMap)));
  return List.sort((function (param, param$1) {
                return Caml_float.caml_float_compare(param[1], param$1[1]);
              }), keyPresses);
}

function lengthSignature(presses) {
  return List.map((function (param) {
                return param[2] - param[1];
              }), presses);
}

function diffSignature(presses) {
  if (presses) {
    var match = presses[1];
    if (match) {
      var match$1 = match[0];
      var down = match$1[1];
      return /* :: */[
              down - presses[0][2],
              diffSignature(/* :: */[
                    /* KeyPress */[
                      match$1[0],
                      down,
                      match$1[2]
                    ],
                    match[1]
                  ])
            ];
    } else {
      return /* [] */0;
    }
  } else {
    return /* [] */0;
  }
}

function print(lst) {
  return List.iter((function (x) {
                console.log(x);
                return /* () */0;
              }), lst);
}

function printActions(actions) {
  if (actions) {
    var match = actions[0];
    if (match.tag) {
      return "(" + (Pervasives.string_of_int(match[0]) + ("," + (Pervasives.string_of_float(match[1]) + (") " + printActions(actions[1])))));
    } else {
      return "(" + (Pervasives.string_of_int(match[0]) + ("," + (Pervasives.string_of_float(match[1]) + (") " + printActions(actions[1])))));
    }
  } else {
    return "";
  }
}

function printPresses(lst) {
  if (lst) {
    var match = lst[0];
    return "(" + (Char.escaped(Char.chr(match[0])) + ("," + (Pervasives.string_of_float(match[2] - match[1]) + (") " + printPresses(lst[1])))));
  } else {
    return "";
  }
}

function getTime() {
  return new Date().getTime();
}

function handleTrainKeyDown($$event) {
  return /* KeyTrainEvent */Block.__(0, [/* KeyDown */Block.__(0, [
                $$event.which,
                new Date().getTime()
              ])]);
}

function handleTrainKeyUp($$event) {
  var kc = $$event.which;
  if (kc === 13) {
    return /* Enter */0;
  } else {
    return /* KeyTrainEvent */Block.__(0, [/* KeyUp */Block.__(1, [
                  kc,
                  new Date().getTime()
                ])]);
  }
}

function handleCheckKeyDown($$event) {
  return /* KeyCheckEvent */Block.__(1, [/* KeyDown */Block.__(0, [
                $$event.which,
                new Date().getTime()
              ])]);
}

function handleCheckKeyUp($$event) {
  var kc = $$event.which;
  if (kc === 13) {
    return /* Enter */0;
  } else {
    return /* KeyCheckEvent */Block.__(1, [/* KeyUp */Block.__(1, [
                  kc,
                  new Date().getTime()
                ])]);
  }
}

function str(prim) {
  return prim;
}

function make(message, _) {
  var newrecord = ReasonReact.reducerComponent("Page").slice();
  newrecord[/* render */9] = (function (param) {
      var state = param[/* state */2];
      var reduce = param[/* reduce */1];
      return React.createElement("div", undefined, message, React.createElement("div", {
                      onKeyDown: Curry._1(reduce, handleTrainKeyDown),
                      onKeyUp: Curry._1(reduce, handleTrainKeyUp)
                    }, React.createElement("textarea", undefined)), React.createElement("div", {
                      onKeyDown: Curry._1(reduce, handleCheckKeyDown),
                      onKeyUp: Curry._1(reduce, handleCheckKeyUp)
                    }, React.createElement("textarea", undefined)), React.createElement("div", undefined, printPresses(extractKeyPresses(state[/* keys */0]))), React.createElement("div", undefined, "=================="), React.createElement("div", undefined, printFloats(diffSignature(extractKeyPresses(state[/* keys */0])))), React.createElement("div", undefined, Pervasives.string_of_int(List.length(state[/* signatures */1]))));
    });
  newrecord[/* initialState */10] = (function () {
      return /* record */[
              /* keys : [] */0,
              /* signatures : [] */0,
              /* attempt : [] */0
            ];
    });
  newrecord[/* reducer */12] = (function (action, state) {
      if (typeof action === "number") {
        var keyPresses = extractKeyPresses(removeDuplicates(state[/* keys */0]));
        var attempKeyPresses = extractKeyPresses(removeDuplicates(state[/* attempt */2]));
        if (verifyKeys("archer", attempKeyPresses)) {
          var verifyLenSignature = verifySignature(List.map(lengthSignature, state[/* signatures */1]), lengthSignature(attempKeyPresses), (function (a) {
                  return +(a < 0.3);
                }));
          var verifyDifSignature = verifySignature(List.map(diffSignature, state[/* signatures */1]), diffSignature(attempKeyPresses), (function (a) {
                  return +(a < 4.0);
                }));
          if (verifyLenSignature && verifyDifSignature) {
            console.log("yes");
          } else {
            console.log("no");
          }
        }
        if (verifyKeys("archer", keyPresses)) {
          return /* Update */Block.__(0, [/* record */[
                      /* keys : [] */0,
                      /* signatures : :: */[
                        keyPresses,
                        state[/* signatures */1]
                      ],
                      /* attempt : [] */0
                    ]]);
        } else {
          return /* Update */Block.__(0, [/* record */[
                      /* keys : [] */0,
                      /* signatures */state[/* signatures */1],
                      /* attempt : [] */0
                    ]]);
        }
      } else if (action.tag) {
        return /* Update */Block.__(0, [/* record */[
                    /* keys */state[/* keys */0],
                    /* signatures */state[/* signatures */1],
                    /* attempt */removeDuplicates(/* :: */[
                          action[0],
                          state[/* attempt */2]
                        ])
                  ]]);
      } else {
        return /* Update */Block.__(0, [/* record */[
                    /* keys */removeDuplicates(/* :: */[
                          action[0],
                          state[/* keys */0]
                        ]),
                    /* signatures */state[/* signatures */1],
                    /* attempt */state[/* attempt */2]
                  ]]);
      }
    });
  return newrecord;
}

exports.rollAverage        = rollAverage;
exports.averageLists       = averageLists;
exports.printFloats        = printFloats;
exports.verifySignature    = verifySignature;
exports.codeOf             = codeOf;
exports.timeOf             = timeOf;
exports.removeDuplicates   = removeDuplicates;
exports.verifyKeys         = verifyKeys;
exports.pairOff            = pairOff;
exports.IntCompare         = IntCompare;
exports.IntMap             = IntMap;
exports.groupBy            = groupBy;
exports.extractKeyPresses  = extractKeyPresses;
exports.lengthSignature    = lengthSignature;
exports.diffSignature      = diffSignature;
exports.print              = print;
exports.printActions       = printActions;
exports.printPresses       = printPresses;
exports.getTime            = getTime;
exports.handleTrainKeyDown = handleTrainKeyDown;
exports.handleTrainKeyUp   = handleTrainKeyUp;
exports.handleCheckKeyDown = handleCheckKeyDown;
exports.handleCheckKeyUp   = handleCheckKeyUp;
exports.str                = str;
exports.make               = make;
/* IntMap Not a pure module */
