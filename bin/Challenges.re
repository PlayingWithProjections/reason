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

let countRegisteredPlayersPerMonth = () => {
  let parseDate = timestamp => {
    open CalendarLib;
    let date = Printer.Calendar.from_fstring("%iT%TZ", timestamp);
    let month =
      switch (Calendar.month(date)) {
      | Jan => 1
      | Feb => 2
      | Mar => 3
      | Apr => 4
      | May => 5
      | Jun => 6
      | Jul => 7
      | Aug => 8
      | Sep => 9
      | Oct => 10
      | Nov => 11
      | Dec => 12
      };
    (Calendar.year(date), month);
  };
  let counter = (monthMap, event) =>
    switch (event.Events.type_) {
    | Events.PlayerHasRegistered(_) =>
      let key = parseDate(event.timestamp);
      MonthMap.update(
        key,
        current => {
          switch (current) {
          | None => Some(1)
          | Some(x) => Some(x + 1)
          }
        },
        monthMap,
      );
    | _ => monthMap
    };
  let outcome = EventStore.subscribe(Types.Basic, MonthMap.empty, counter);

  MonthMap.iter((key, val') => {Console.log((key, val'))}, outcome);
  "-";
};

let main = () => {
  Console.log("Running challenges");
  Console.log("Count events");
  Console.log(countEvents());

  Console.log("Count registered players");
  Console.log(countRegisteredPlayers());

  Console.log("Count registered players per month");
  Console.log(countRegisteredPlayersPerMonth());
};

main();
