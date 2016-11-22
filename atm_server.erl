-module(atm_server).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link() -> gen_server:start_link({global, atm_server}, atm_server, [], []).


init([]) -> {'ok', [5000, 50, 50, 50, 1000, 5000, 1000, 500, 100]}.

terminate(_, _) -> 'ok'.

handle_call({widthdraw, Amount}, _, State) ->
    try atm:widthdraw(Amount, State) of
        {Response, BanknotesToGive, RestBanknotes} -> 
              {'reply', {Response, BanknotesToGive}, RestBanknotes}
    catch
          _:_ -> {'stop', 'normal', State}
    end;

handle_call(_, _, State) -> {'reply', 'Unknown command', State}.

handle_cast(_, State) -> {'noreply', State}.

handle_info(_, State) -> {'noreply', State}.

code_change(_, State, _) -> {'ok', State}.