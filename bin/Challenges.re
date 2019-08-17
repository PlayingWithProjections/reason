let countEvents = () => {
  let counter = (count, _event) => count + 1;
  let outcome = Lib.EventStore.subscribe(Lib.Types.Basic, 0, counter);

  outcome;
};

let main = () => {
  Console.log("Running challenges");
  Console.log("Count events");
  Console.log(countEvents());
};

main();
