-module(fibonacci).
-export([get/2]).

-spec get(non_neg_integer(), 'list' | 'number') -> [pos_integer()] | non_neg_integer().
get(N, list) -> generate(N, 0, 1, [], list);
get(N, number) -> generate(N, 0, 1, [], number).

-spec generate(non_neg_integer(), non_neg_integer(), pos_integer(), [pos_integer()], 'list' | 'number') -> [pos_integer()] | non_neg_integer().
generate(0, _, _, Acc, list) -> lists:reverse(Acc);
generate(0, Pp, _, _, number) -> Pp;
generate(N, Pp, P, Acc, Type) when N > 0 ->
    generate(N - 1, P, Pp + P, [P | Acc], Type).