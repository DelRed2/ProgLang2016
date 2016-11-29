-module(atm_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link(atm_sup, []).


init(_) ->
    RestartKind = 'permanent',
    ShutdownTime = 3600,
    ChildType = 'worker',
      
    AtmServer = {'atm_server', {atm_server, start_link, []},
        RestartKind, ShutdownTime, ChildType, [atm_server]},
    
    TransactionsServer = {'transactions_server', {transactions_server, start_link, []},
        RestartKind, ShutdownTime, ChildType, [transactions_server]},
      
    MaxRestarts = 100,
    MaxRestartTime = 60,
    Settings = {'one_for_one', MaxRestarts, MaxRestartTime},
    
    {'ok', {Settings, [AtmServer, TransactionsServer]}}.