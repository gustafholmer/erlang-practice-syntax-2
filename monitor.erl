%%%-------------------------------------------------------------------
%%% gustafholmer guho0000
%%%-------------------------------------------------------------------
-module(monitor).

-export([start/0]).

start() ->
  spawn(fun() -> resume_proc() end).

resume_proc() ->
  process_flag(trap_exit, true),
  double:start(),
  erlang:monitor(process, double),
  receive
    {'DOWN', _Ref, process, _Ref2, _Reason} ->
      resume_proc()
  end.


