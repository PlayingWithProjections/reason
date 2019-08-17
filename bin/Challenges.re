open Lib;

let countEvents = () => {
  let counter = (count, _event) => count + 1;
  let outcome = EventStore.subscribe(Types.Basic, 0, counter);

  outcome;
};

let countRegisteredPlayers = () => {
  let counter = (count, event) =>
    switch (event.Events.type_) {
    | Events.PlayerHasRegistered(_) => count + 1
    | _ => count
    };
  let outcome = EventStore.subscribe(Types.Basic, 0, counter);

  outcome;
};

let main = () => {
  Console.log("Running challenges");
  Console.log("Count events");
  Console.log(countEvents());

  Console.log("Count registered players");
  Console.log(countRegisteredPlayers());
};

main();
