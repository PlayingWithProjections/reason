open Lib;

let parseMonth = timestamp => {
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

let countEvents = () => {
  let counter = (count, _event) => count + 1;
  let outcome = EventStore.subscribe(Types.Full, 0, counter);

  outcome;
};

let countRegisteredPlayers = () => {
  let counter = (count, event) =>
    switch (event.Events.type_) {
    | Events.PlayerHasRegistered(_) => count + 1
    | _ => count
    };
  let outcome = EventStore.subscribe(Types.Full, 0, counter);

  outcome;
};

let countRegisteredPlayersPerMonth = () => {
  let counter = (monthMap, event) =>
    switch (event.Events.type_) {
    | Events.PlayerHasRegistered(_) =>
      let key = parseMonth(event.timestamp);
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
  let outcome = EventStore.subscribe(Types.Full, MonthMap.empty, counter);

  MonthMap.iter((key, val') => {Console.log((key, val'))}, outcome);
  "-";
};

module FindMostPopularQuizzes = {
  module StringMap = Map.Make(String);

  type state = {
    quizMap: StringMap.t(string),
    gameIdToQuizId: StringMap.t(string),
    gamesPerMonth: MonthMap.t(StringMap.t(int)),
  };

  let projection = (state, event) =>
    switch (event.Events.type_) {
    | Events.QuizWasCreated(quiz) => {
        ...state,
        quizMap: StringMap.add(quiz.quizId, quiz.quizTitle, state.quizMap),
      }
    | Events.GameWasOpened(game) => {
        ...state,
        gameIdToQuizId:
          StringMap.add(game.gameId, game.quizId, state.gameIdToQuizId),
      }
    | Events.GameWasFinished({gameId}) =>
      let quizId = StringMap.find(gameId, state.gameIdToQuizId);
      let key = parseMonth(event.timestamp);
      {
        ...state,
        gamesPerMonth:
          MonthMap.update(
            key,
            current => {
              switch (current) {
              | None =>
                let map = StringMap.add(quizId, 1, StringMap.empty);
                Some(map);

              | Some(gameCount) =>
                Some(
                  StringMap.update(
                    quizId,
                    count => {
                      switch (count) {
                      | None => Some(1)
                      | Some(n) => Some(n + 1)
                      }
                    },
                    gameCount,
                  ),
                )
              }
            },
            state.gamesPerMonth,
          ),
      };
    | _ => state
    };

  let takeN = (n, lst) => {
    let rec take = (n, lst, result) => {
      switch (n, lst) {
      | (n, _) when n <= 0 => result
      | (_, []) => result
      | (_, [hd, ...tl]) => take(n - 1, tl, [hd, ...result])
      };
    };
    List.rev(take(n, lst, []));
  };

  let sortPerMonth = (quizzesThisMonth, outcome) => {
    StringMap.to_seq(quizzesThisMonth)
    |> Seq.map(((quizId, count)) => {
         let quizName = StringMap.find(quizId, outcome.quizMap);
         (quizId, quizName, count);
       })
    |> List.of_seq
    |> List.sort(((_, _, countA), (_, _, countB)) =>
         compare(countB, countA)
       )
    |> takeN(10);
  };

  let sortTotal = outcome => {
    MonthMap.fold(
      (_, quizzes, acc) => {
        StringMap.fold(
          (quizId, count, acc) => {
            StringMap.update(
              quizId,
              c => {
                switch (c) {
                | None => Some(count)
                | Some(n) => Some(n + count)
                }
              },
              acc,
            )
          },
          quizzes,
          acc,
        )
      },
      outcome.gamesPerMonth,
      StringMap.empty,
    )
    |> sortPerMonth;
  };

  let execute = () => {
    let emptyState = {
      quizMap: StringMap.empty,
      gameIdToQuizId: StringMap.empty,
      gamesPerMonth: MonthMap.empty,
    };
    let outcome = EventStore.subscribe(Types.Full, emptyState, projection);

    MonthMap.iter(
      (key, val') => {
        Console.log(key);
        List.iter(
          ((quizId, quizName, count)) =>
            Console.log(
              quizId ++ " " ++ quizName ++ ": " ++ string_of_int(count),
            ),
          sortPerMonth(val', outcome),
        );
      },
      outcome.gamesPerMonth,
    );
    Console.log("in total");
    MonthMap.iter(
      (key, val') => {
        Console.log(key);
        List.iter(
          ((quizId, quizName, count)) =>
            Console.log(
              quizId ++ " " ++ quizName ++ ": " ++ string_of_int(count),
            ),
          sortPerMonth(val', outcome),
        );
      },
      outcome.gamesPerMonth,
    );
    "-";
  };
};

let main = () => {
  Console.log("Running challenges");
  Console.log("Count events");
  Console.log(countEvents());

  Console.log("Count registered players");
  Console.log(countRegisteredPlayers());

  Console.log("Count registered players per month");
  Console.log(countRegisteredPlayersPerMonth());

  Console.log("Find the most popular quizzes");
  Console.log(FindMostPopularQuizzes.execute());
};

main();
