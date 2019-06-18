type subscriber('a) = ('a, Types.event) => 'a;
let subscribe: (Types.stream, 'a, subscriber('a)) => 'a;
