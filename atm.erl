-module(atm).
-export([widthdraw/2]).

%% === Public ============================================

%% Нечего выдавать
widthdraw(0, Banknotes) -> {'ok', [], Banknotes};

%% Нет банкнот
widthdraw(_, []) -> erlang:error('Empty Banknotes');

%% Основной метод выдачи
widthdraw(Amount, Banknotes) ->
    Variants = getAllVariants(Amount, Banknotes),
    {Response, BanknotesToGive} = getBestVariant(Variants),
    {Response, BanknotesToGive, Banknotes -- BanknotesToGive}.


%% === Private ============================================

%% Возвращает все варианты выдачи
getAllVariants(Amount, Banknotes) -> getAllVariants(0, [], Amount, Banknotes).

%% Больше купюр нет
getAllVariants(_, _, _, []) -> [];

%% Рекурсивная функция, возвращающая варианты выдачи
getAllVariants(CurrentAmount, CurrentBanknotes, RequiredAmount, [FirstBanknote | RestBanknotes]) ->
    NewAmount = CurrentAmount + FirstBanknote,
    NewBanknotesVariant = CurrentBanknotes ++ [FirstBanknote],

    RestVariants = getAllVariants(CurrentAmount, CurrentBanknotes, RequiredAmount, RestBanknotes),

    if
        %% Если набрали ровно - запоминаем разбиение
        NewAmount == RequiredAmount ->
            Variants = RestVariants ++ [NewBanknotesVariant];

        %% Если недобор - то добавляем банкноту к текущим и вызываем рекурсивно
        NewAmount < RequiredAmount ->
            FirstVariants = getAllVariants(NewAmount, NewBanknotesVariant, RequiredAmount, RestBanknotes),
            Variants = FirstVariants ++ RestVariants;

        %% Если перебор - то только варианты без этой купюры
        true ->
            Variants = RestVariants
    end,

    Variants.

getBestVariant(Variants) -> getBestVariant([], Variants).

%% Если нет ни одного варианта
getBestVariant([], []) -> {'request_another_amount', []};

%% Если кончились варинты - возвращаем текущий
getBestVariant(MinVariant, []) -> {'ok', sortBanknotes(MinVariant)};

getBestVariant(MinVariant, [CurrentVariant | RestVariants]) ->
    CurrentBanknotesCount = length(CurrentVariant),
    MinBanknotesCount = length(MinVariant),

    case (MinBanknotesCount == 0) or (CurrentBanknotesCount =< MinBanknotesCount) of

        true -> getBestVariant(CurrentVariant, RestVariants);
        false -> getBestVariant(MinVariant, RestVariants)

    end.

sortBanknotes(Banknotes) -> lists:sort(fun(L, R) -> R < L end, Banknotes).
