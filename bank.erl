%%%-------------------------------------------------------------------
%%% gustafholmer guho0000
%%%-------------------------------------------------------------------
-module(bank).
-export([start/0, balance/2, deposit/3, withdraw/3, lend/4]).

start() ->
  spawn(fun() -> bank_server(ets:new(accounts, [set])) end).

bank_server(Bank_accounts) ->
  receive
    {From, Ref, Who, balance} -> case ets:lookup(Bank_accounts, Who) of
                                   [{_, Current_amount}] -> From ! {Ref, Who, Current_amount};
                                   [] -> From ! {Ref, Who, no_account}
                                 end, bank_server(Bank_accounts);

    {From, Ref, Who, Input_amount, deposit} -> case ets:lookup(Bank_accounts, Who) of
                                   [{_, Current_amount}] -> ets:insert(Bank_accounts, {Who, Input_amount + Current_amount}),
                                     From ! {Ref, Who, Input_amount + Current_amount};
                                   [] -> ets:insert(Bank_accounts, {Who, Input_amount}), From ! {Ref, Who, Input_amount}
                                               end, bank_server(Bank_accounts);

    {From, Ref, Who, Input_amount, withdraw} -> case ets:lookup(Bank_accounts, Who) of
                                    [{_, Current_amount}] when Current_amount >= Input_amount -> ets:insert(Bank_accounts,
                                      {Who, Current_amount - Input_amount}), From ! {Ref, Who,  Current_amount - Input_amount};
                                    [{_, Current_amount}] when Input_amount > Current_amount -> From ! {Ref, Who, insufficient_funds};
                                    [] -> From ! {Ref, Who, no_account}
                                                end, bank_server(Bank_accounts);

    {From, Ref, Who, To, Input_amount, lend} -> case ets:lookup(Bank_accounts, Who) of
                                    [{_, Current_amount}] when Current_amount >= Input_amount ->
                                              case ets:lookup(Bank_accounts, To) of
                                                  [{_, Current_amount2}] ->
                                                      ets:insert(Bank_accounts, {Who, Current_amount - Input_amount}),
                                                      ets:insert(Bank_accounts, {To, Current_amount2 + Input_amount}),
                                                      From ! {Ref, Who,  ok};

                                                  [] -> From ! {Ref, To, no_account}
                                              end;
                                    [{_, Current_amount}] when Input_amount > Current_amount -> From ! {Ref, Who, insufficient_funds};
                                    [] -> From ! case ets:lookup(Bank_accounts, To) of
                                                   [{_, _Current_amount2}] -> From ! {Ref, Who, no_account};
                                                   [] -> From ! {Ref, both, no_account}
                                                 end
                                                end, bank_server(Bank_accounts)
  end.



balance(Pid, Who) when is_pid(Pid) ->
  Ref = make_ref(),

  try erlang:is_process_alive(Pid)
  catch
    false -> no_bank
  end,
  process_flag(trap_exit, true),
  erlang:monitor(process, Pid),

  Pid ! {self(), Ref, Who, balance},

  receive
    {Ref, _Name, no_account} -> no_account;
    {Ref, _Name, Amount} -> {ok, Amount};
    {'DOWN', _Ref, process, _Ref2, _Reason} -> no_bank
  end;
balance(_Pid, _Who) -> no_bank.


deposit(Pid, Who, Amount) when is_pid(Pid) ->
  Ref = make_ref(),

  try erlang:is_process_alive(Pid)
  catch
    false -> no_bank
  end,
  process_flag(trap_exit, true),

  Pid ! {self(), Ref, Who, Amount, deposit},

  receive
    {Ref, _Name, Deposit_value} -> {ok, Deposit_value};
    {'DOWN', _Ref, process, _Ref2, _Reason} -> no_bank
  end;
deposit(_Pid, _Who, _Amount) -> no_bank.




withdraw(Pid, Who, Amount) when is_pid(Pid) ->
  Ref = make_ref(),

  try erlang:is_process_alive(Pid)
  catch
    false -> no_bank
  end,
  process_flag(trap_exit, true),
  erlang:monitor(process, Pid),

  Pid ! {self(), Ref, Who, Amount, withdraw},

  receive
    {Ref, _Name, insufficient_funds} -> insufficient_funds;
    {Ref, _Name, no_account} -> no_account;
    {Ref, _Name, Withdraw_amount} -> {ok, Withdraw_amount};
    {'DOWN', _Ref, process, _Ref2, _Reason} -> no_bank
  end;
withdraw(_Pid, _Who, _Amount) -> no_bank.



lend(Pid, From, To, Amount) when is_pid(Pid) ->
  Ref = make_ref(),

  try erlang:is_process_alive(Pid)
  catch
    false -> no_bank
  end,
  process_flag(trap_exit, true),
  erlang:monitor(process, Pid),

  Pid ! {self(), Ref, From, To, Amount, lend},

  receive
    {Ref, _Name, insufficient_funds} -> insufficient_funds;
    {Ref, both, no_account} -> {no_account, both};
    {Ref, Name, no_account} -> {no_account, Name};
    {Ref, _Name, ok} -> ok;
    {'DOWN', _Ref, process, _Ref2, _Reason} -> no_bank
  end;
lend(_Pid, _From, _To, _Amount) -> no_bank.

