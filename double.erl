%%%-------------------------------------------------------------------
%%% gustafholmer guho0000
%%%-------------------------------------------------------------------
-module(double).

-export([start/0, double/1]).

start() ->
  Pid = spawn(fun() -> eval_input() end),
  register(double, Pid).

eval_input() ->
  receive
    {From, Ref, Number} when is_integer(Number); is_float(Number) -> From ! {Ref, Number * 2},
      eval_input();
    {_From, _Ref, _Number} ->
      exit(badarith)
  end.

double(Number) ->
  Ref = make_ref(),
  double ! {self(), Ref, Number},
  receive
    {Ref, Number_calculated} -> Number_calculated
  after 1000 ->
    double(Number)
  end.

