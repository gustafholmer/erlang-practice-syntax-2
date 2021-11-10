%%%-------------------------------------------------------------------
%%% gustafholmer guho0000
%%%-------------------------------------------------------------------
-module(pmap).
-export([unordered/2]).

unordered(Fun, L) ->
  Self = self(),
  Ref = erlang:make_ref(),
  lists:map(fun(I) -> spawn(fun() -> execute_function(Self, Ref, Fun, I) end) end, L),
    gather(Ref, length(L), []).


execute_function(Parent, Ref, F, I) ->
  Parent ! {Ref, F(I)}.

gather(_, 0,  L) -> L;
gather(Ref, Len, L) ->
  receive
    {Ref, Value} -> gather(Ref, Len - 1, [Value|L])
  end.









