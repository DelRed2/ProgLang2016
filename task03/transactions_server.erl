-module(transactions_server).
-behaviour(gen_server).
-export([start_link/0, init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).


start_link() ->
    gen_server:start_link({global, transactions_server}, transactions_server, [], []).

%% State = {CurrentLine, File, History}
%%     * CurrentLine - номер последней операции (соответствует номеру строки в истории)
%%     * File - файл с историей
%%     * History - представление истории в памяти

init([]) ->
    {'ok', loadStateFromFile()}.  

terminate(_, {_, File, _}) -> dets:close(File).

    
handle_call(history, _, {CurrentLine, File, History}) -> {'reply', History, {CurrentLine, File, History}}.
    
handle_cast({widthdraw, Amount}, {CurrentLine, File, History}) ->
    dets:insert(File, {CurrentLine + 1, Amount}),
    {noreply, {CurrentLine + 1, File, History ++ [Amount]}};
    
handle_cast(clear, {_, File, _}) ->
    dets:delete_all_objects(File),
    {noreply, {0, File, []}};

handle_cast(_, State) -> {'noreply', State}.      

handle_info(_, State) -> {'noreply', State}.

code_change(_, State, _) -> {'ok', State}.


%% === Private ============================================

loadStateFromFile() ->
    {'ok', File} = dets:open_file('history', []),
    {MaxLine, Lines} = loadHistoryLines(dets:match(File, '$1')),
    %% Сортируем по номерам строк
    SortedLines = lists:sort(fun({LineNum1, _}, {LineNum2, _}) -> LineNum1 < LineNum2 end, Lines),
    %% Оставляем только значения (Amount)
    History = lists:map(fun({_, V}) -> V end, SortedLines),
    {MaxLine, File, History}.

loadHistoryLines([]) -> {0, []};

loadHistoryLines([[{FirstLine, First}] | Rest]) ->
    {RestMaxLine, RestLines} = loadHistoryLines(Rest),

    if
        FirstLine > RestMaxLine 
            -> MaxLine = FirstLine;
        true 
            -> MaxLine = RestMaxLine
    end,

    Lines = [{FirstLine, First}] ++ RestLines,
    {MaxLine, Lines}.