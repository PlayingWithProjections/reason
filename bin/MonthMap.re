module IntPairs = {
  type t = (int, int);
  let compare = ((a0, b0), (a1, b1)) => {
    switch (Pervasives.compare(a0, a1)) {
    | 0 => Pervasives.compare(b0, b1)
    | c => c
    };
  };
};

module PairsMap = Map.Make(IntPairs);

include PairsMap;
