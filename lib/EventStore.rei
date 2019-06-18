type subscriber('a) = ('a, Events.event) => 'a;
let subscribe: (Types.stream, 'a, subscriber('a)) => 'a;
