open! Base;

type subscriber('a) = ('a, Events.event) => 'a;
let subscribe = (stream, initialState, subscriber) => {
  let events =
    switch (stream) {
    | Types.Basic => Events.read("../data/basic.json")
    | Types.Full => Events.read("../data/full.json")
    };
  List.fold_left(~f=subscriber, ~init=initialState, events);
};
