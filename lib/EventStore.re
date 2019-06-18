type subscriber('a) = ('a, Types.event) => 'a;
let subscribe = (stream, initialState, subscriber) => {
	switch (stream) {
		| Types.Basic => 
			let events = [Types.Foo, Types.Foo];
			List.fold_left(subscriber, initialState, events)
	}
}
