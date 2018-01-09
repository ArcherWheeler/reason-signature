type keyCode = int;

let rec rollAverage(n: int, weightedList: list(float), list: list(float)): list(float) = {
  let fl = float_of_int(n);
  switch (weightedList, list) {
    | ([a, ...restA], [b, ...restB]) => [(((fl *. a) +. b) /. (fl +. 1.0)), ...rollAverage(n, restA, restB)]
    | _otherwise => []
  }
};
let rec averageLists(~n = 1, lists: list(list(float))): list(float) =
  switch lists {
    | [listA, listB, ...rest] => averageLists(~n = n+1, [rollAverage(n, listA, listB), ...rest])
    | [list] => list
    | [] => []
  };

type signature = {
  lengthSignature: list(float),
  diffSignature: list(float)
};

let printFloats(lst: list(float)): string = List.fold_left((acc, x) => acc ++ ", " ++ string_of_float(x), "",lst);

let verifySignature(trained: list(list(float)), canidate: list(float), fn: float => bool): bool = {
  /* let averageLengths = trained |> List.map(r => r.lengthSignature) |> averageLists; */
  let averageDiffs = averageLists(trained);
  let percentageDiff(correct, other) = abs_float(other -. correct) /. correct;
  switch (averageDiffs, canidate) {
    | ([], _) => false
    | (_, []) => true
    | (avdif, _) => {
      let percentageDiffList = List.map2(percentageDiff, avdif, canidate);
      Js.log(printFloats(percentageDiffList));
      List.for_all(fn, percentageDiffList)
    }
  }
};

type keyEvent =
  | KeyDown(keyCode, float)
  | KeyUp(keyCode, float);

type action =
  | KeyTrainEvent(keyEvent)
  | KeyCheckEvent(keyEvent)
  | Enter;

let codeOf(keyEv) =
  switch keyEv {
    | KeyDown(kc, _) => kc
    | KeyUp(kc, _) => kc
  };
let timeOf(keyEv) =
  switch keyEv {
    | KeyDown(_, ts) => ts
    | KeyUp(_, ts) => ts
  };

let rec removeDuplicates(actions: list(keyEvent)): list(keyEvent) =
  switch actions {
    | [KeyUp(kc, _), ...rest] when (kc > 90 || kc < 65) => removeDuplicates(rest)
    | [KeyDown(kc, _), ...rest] when (kc > 90 || kc < 65) => removeDuplicates(rest)
    | [KeyUp(a, _), KeyUp(b, ts), ...rest] when a == b => removeDuplicates([KeyUp(a, ts), ...rest])
    | [KeyDown(a, _), KeyDown(b, ts), ...rest] when a == b => removeDuplicates([KeyDown(a, ts), ...rest])
    | [a, ...rest] => [a, ...removeDuplicates(rest)]
    | [] => []
  };

type keyPress = KeyPress(keyCode, float, float);

let verifyKeys(str: string, keyPresses: list(keyPress)) = {
  let writtenString = List.fold_left((acc, KeyPress(kc, _, _)) => acc ++ (kc |> Char.chr |> Char.escaped), "", keyPresses);
  str == String.lowercase(writtenString)
};

let rec pairOff(actions: list(keyEvent)): list(keyPress) =
  switch actions {
    | [KeyDown(kc, ts1), KeyUp(_, ts2), ...rest] => [KeyPress(kc, ts1, ts2), ...pairOff(rest)]
    | _otherwise => []
  };

module IntCompare = {
   type t = int;
   let compare(x, y) = Pervasives.compare(x, y);
 };

module IntMap = Map.Make(IntCompare);

let groupBy: ('a => int) => list('a) => IntMap.t(list('a)) =
  (fn) => List.fold_left(
    (map, element) => {
      let key = fn(element);
      let current =
        if (IntMap.mem(key, map)) {
          IntMap.find(key, map);
        } else [];
      let append = [element, ...current];
      IntMap.add(key, append, map);
    },
    IntMap.empty
  );

let extractKeyPresses(actions: list(keyEvent)) = {
  let keyEvents = removeDuplicates(actions);
  let keyMap = groupBy(codeOf, keyEvents);
  let keyPressMap = IntMap.map(
    lst => pairOff(List.sort((a,b) => Pervasives.compare(timeOf(a), timeOf(b)), lst)),
    keyMap
  );
  let keyPresses = List.concat(List.map(snd, IntMap.bindings(keyPressMap)));
  List.sort(
    ((KeyPress(_, down1, _), KeyPress(_, down2, _)) => Pervasives.compare(down1, down2)),
    keyPresses
  )
};

let lengthSignature(presses: list(keyPress)): list(float) =
  List.map(
    (KeyPress(_, down, up)) => up -. down,
    presses
  );

let rec diffSignature(presses: list(keyPress)): list(float) =
  switch presses {
    | [KeyPress(_, _, up), KeyPress(kc, down, up2), ...rest] => [down -. up, ...diffSignature([KeyPress(kc, down, up2), ...rest])]
    | _otherwise => []
  };

let print(lst: list(keyPress)) = List.iter(x => Js.log(x), lst);

let rec printActions(actions: list(keyEvent)): string =
  switch actions {
    | [KeyDown(kc, t), ...rest] => "(" ++ string_of_int(kc) ++ "," ++ string_of_float(t) ++ ") " ++ printActions(rest)
    | [KeyUp(kc, t), ...rest] => "(" ++ string_of_int(kc) ++ "," ++ string_of_float(t) ++ ") " ++ printActions(rest)
    | [] => ""
  };
let rec printPresses(lst: list(keyPress)): string =
  switch lst {
    | [KeyPress(kc, t1, t2), ...rest] => "(" ++ Char.escaped(Char.chr(kc)) ++ "," ++ string_of_float(t2 -. t1) ++ ") " ++ printPresses(rest)
    | [] => ""
  };

type state = {
  keys: list(keyEvent),
  signatures: list(list(keyPress)),
  attempt: list(keyEvent)
};

let getTime = () => Js.Date.getTime(Js.Date.make());

let handleTrainKeyDown = (event) => KeyTrainEvent(KeyDown(ReactEventRe.Keyboard.which(event), getTime()));
let handleTrainKeyUp(event) =
  switch (ReactEventRe.Keyboard.which(event)) {
    | kc when kc == 13 => Enter
    | kc => KeyTrainEvent(KeyUp(kc, getTime()))
  };

let handleCheckKeyDown(event) = KeyCheckEvent(KeyDown(ReactEventRe.Keyboard.which(event), getTime()));
let handleCheckKeyUp(event) =
  switch (ReactEventRe.Keyboard.which(event)) {
    | kc when kc == 13 => Enter
    | kc => KeyCheckEvent(KeyUp(kc, getTime()))
  };

let str = ReasonReact.stringToElement;

let make = (~message, _children) => {
  ...ReasonReact.reducerComponent("Page"),
  initialState: () => {keys: [], signatures: [], attempt: []},
  reducer: (action, state) => {
    switch action {
      | KeyTrainEvent(evnt) => ReasonReact.Update({keys: removeDuplicates([evnt, ...state.keys]), attempt: state.attempt, signatures: state.signatures})
      | KeyCheckEvent(evnt) => ReasonReact.Update({keys: state.keys, attempt: removeDuplicates([evnt, ...state.attempt]), signatures: state.signatures})
      | Enter => {
        let keyPresses = state.keys |> removeDuplicates |> extractKeyPresses;
        let attempKeyPresses = state.attempt |> removeDuplicates |> extractKeyPresses;
        if (verifyKeys("archer", attempKeyPresses)) {
          let verifyLenSignature = verifySignature(state.signatures |> List.map(lengthSignature), attempKeyPresses |> lengthSignature, a => a < 0.3);
          let verifyDifSignature = verifySignature(state.signatures |> List.map(diffSignature), attempKeyPresses |> diffSignature, a => a < 4.0);
          if (verifyLenSignature && verifyDifSignature) {
            Js.log("yes")
          } else Js.log("no");
        } else ();

        if (verifyKeys("archer", keyPresses)) {
          ReasonReact.Update({
            keys: [],
            signatures: [keyPresses, ...state.signatures],
            attempt: []
          })
        } else {
          ReasonReact.Update({
            keys: [],
            signatures: state.signatures,
            attempt: []
          })
        }
      }
    }
  },
  render: ({state, reduce}) =>
    <div>
      (ReasonReact.stringToElement(message))
      <div onKeyDown=(reduce(handleTrainKeyDown)) onKeyUp=(reduce(handleTrainKeyUp))>
        <textarea> </textarea>
      </div>
      <div onKeyDown=(reduce(handleCheckKeyDown)) onKeyUp=(reduce(handleCheckKeyUp))>
        <textarea> </textarea>
      </div>
      <div>
        (ReasonReact.stringToElement(printPresses(extractKeyPresses(state.keys))))
      </div>
      <div>
        (ReasonReact.stringToElement("=================="))
      </div>
      <div>
        (ReasonReact.stringToElement(printFloats(diffSignature(extractKeyPresses(state.keys)))))
      </div>
      <div>
        (ReasonReact.stringToElement(string_of_int(List.length(state.signatures))))
      </div>
    </div>
};
