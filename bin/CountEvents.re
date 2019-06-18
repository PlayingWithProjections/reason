Console.log("Counting events");

let counter = (count, _event)  => count + 1;	
let outcome = Lib.EventStore.subscribe(Lib.Types.Basic, 0, counter);

let () = print_endline(string_of_int(outcome));
