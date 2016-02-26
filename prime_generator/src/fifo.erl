-module(fifo).
-author("oleg").

%% API
-export([new/0, push/2, next/1, empty/1]).

%% implemented as two stacks; push on the first, pop on the second.
%% when the second is empty, reverse the first and make it the second.
new() -> {fifo, [], []}.

push({fifo, In, Out}, X) -> {fifo, [X|In], Out}.

next({fifo, [], []}) -> {undefined, {fifo, [], []}};
next({fifo, In, []}) -> next({fifo, [], lists:reverse(In)});
next({fifo, In, [H|T]}) -> {H, {fifo, [H | In], T}}.

empty({fifo, [], []}) -> true;
empty({fifo, _, _}) -> false.