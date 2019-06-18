type event = {
  id: string,
  timestamp: string,
  type_: payload,
}
and payload =
  | PlayerHasRegistered{
      playerId: string,
      lastName: string,
      firstName: string,
    }
  | QuizWasCreated{
      quizId: string,
      ownerId: string,
      quizTitle: string,
    }
  | QuestionAddedToQuiz{
      quizId: string,
      questionId: string,
      question: string,
      answer: string,
    }
  | QuizWasPublished{quizId: string}
  | GameWasOpened{
      quizId: string,
      gameId: string,
      playerId: string,
    }
  | GameWasCancelled{gameId: string}
  | GameWasStarted{gameId: string}
  | PlayerJoinedGame{
      playerId: string,
      gameId: string,
    }
  | QuestionWasAsked{
      gameId: string,
      questionId: string,
    }
  | TimerHasExpired{
      questionId: string,
      playerId: string,
      gameId: string,
    }
  | GameWasFinished{gameId: string}
  | AnswerWasGiven{
      questionId: string,
      playerId: string,
      gameId: string,
      answer: string,
    }
  | QuestionWasCompleted{
      questionId: string,
      gameId: string,
    };

exception InvalidFormat(string);

let read = filename => {
  let parseEvent = json => {
    open Yojson.Basic.Util;
    let id = json |> member("id") |> to_string;
    let timestamp = json |> member("timestamp") |> to_string;
    let type_ = json |> member("type") |> to_string;
    let payload = json |> member("payload");
    let parsedPayload =
      switch (type_) {
      | "PlayerHasRegistered" =>
        PlayerHasRegistered({
          playerId: payload |> member("player_id") |> to_string,
          firstName: payload |> member("first_name") |> to_string,
          lastName: payload |> member("last_name") |> to_string,
        })
      | "QuizWasCreated" =>
        QuizWasCreated({
          quizId: payload |> member("quiz_id") |> to_string,
          ownerId: payload |> member("owner_id") |> to_string,
          quizTitle: payload |> member("quiz_title") |> to_string,
        })
      | "QuestionAddedToQuiz" =>
        QuestionAddedToQuiz({
          quizId: payload |> member("quiz_id") |> to_string,
          questionId: payload |> member("question_id") |> to_string,
          question: payload |> member("question") |> to_string,
          answer: payload |> member("answer") |> to_string,
        })
      | "QuizWasPublished" =>
        QuizWasPublished({quizId: payload |> member("quiz_id") |> to_string})
      | "GameWasOpened" =>
        GameWasOpened({
          quizId: payload |> member("quiz_id") |> to_string,
          gameId: payload |> member("game_id") |> to_string,
          playerId: payload |> member("player_id") |> to_string,
        })
      | "GameWasCancelled" =>
        GameWasCancelled({gameId: payload |> member("game_id") |> to_string})
      | "GameWasStarted" =>
        GameWasStarted({gameId: payload |> member("game_id") |> to_string})
      | "GameWasFinished" =>
        GameWasFinished({gameId: payload |> member("game_id") |> to_string})
      | "PlayerJoinedGame" =>
        PlayerJoinedGame({
          playerId: payload |> member("player_id") |> to_string,
          gameId: payload |> member("game_id") |> to_string,
        })
      | "QuestionWasAsked" =>
        QuestionWasAsked({
          gameId: payload |> member("game_id") |> to_string,
          questionId: payload |> member("question_id") |> to_string,
        })
      | "TimerHasExpired" =>
        TimerHasExpired({
          gameId: payload |> member("game_id") |> to_string,
          questionId: payload |> member("question_id") |> to_string,
          playerId: payload |> member("player_id") |> to_string,
        })
      | "AnswerWasGiven" =>
        AnswerWasGiven({
          gameId: payload |> member("game_id") |> to_string,
          questionId: payload |> member("question_id") |> to_string,
          playerId: payload |> member("player_id") |> to_string,
          answer: payload |> member("answer") |> to_string,
        })
      | "QuestionWasCompleted" =>
        QuestionWasCompleted({
          gameId: payload |> member("game_id") |> to_string,
          questionId: payload |> member("question_id") |> to_string,
        })
      | _ => raise(InvalidFormat("unrecognised event"))
      };
		{id, timestamp, type_: parsedPayload}
  };
  let json = Yojson.Basic.from_file(filename);
  switch (json) {
  | `List(events) => List.map(parseEvent, events)
  | _ => raise(InvalidFormat("expected a json list"))
  };
};
