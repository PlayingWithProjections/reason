type subscriber('a) = ('a, Events.event) => 'a;
let subscribe = (stream, initialState, subscriber) => {
	switch (stream) {
		| Types.Basic => 
			let events = Events.read("../data/0.json");
			List.fold_left(subscriber, initialState, events)
	}
}
