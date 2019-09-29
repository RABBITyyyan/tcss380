% Lab 05
% Chang Yan, Meng Yang
-module(lab05).
-compile(export_all).

-type cardrank() :: 2..10 | king | queen | jack | ace.
-type cardcolor() :: red | black.
-type card() :: {cardrank, cardcolor}.

cardValue({Rank, Color}) ->
    color({Rank, Color}).

value(Rank) -> 
    case Rank of
        king -> 10;
        queen -> 10;
        jack -> 10;
        ace -> 1;
        Other -> Other
        end.

color({Rank, Color}) ->
    case Color of 
        red -> value(Rank);
        black -> value(Rank) + 1
    end.

cardValue2({Rank, Color}) ->
    Value1 = fun(Inrank) ->
            case Inrank of
                king -> 10;
                queen -> 10;
                jack -> 10;
                ace -> 1;
                Other -> Other
                end
            end,       

    Value2 = fun({Inrank, Incolor}) ->
            case Incolor of 
                red -> Value1(Inrank);
                black -> Value1(Inrank) + 1
                end
            end,
    Value2 ({Rank, Color}).

myproduct([X|Xs]) ->
    lists:foldr(fun(Prod, Y) -> Y * Prod end, 1, [X|Xs]).

isDivisibleBy2(X) ->
    X rem 2 == 0.

isDivisibleBy3(X) ->
    X rem 3 == 0.

myfilter([], _Vals) -> [];
myfilter([H|T], Vals) -> [lists:filter(H, Vals)|myfilter(T, Vals)].

myfilterRunner() ->
    Fs = [fun isDivisibleBy2/1, fun isDivisibleBy3/1],
    L = [1, 2, 3, 4, 5, 6, 7, 8],
    myfilter(Fs, L).
    
squareOrCube(2) -> (fun(X) -> X * X end);
squareOrCube(3) -> (fun(X) -> X * X * X end).

bill(Price, DiscountRate) -> 
    fun(TaxRate) -> Price * (1.0 - DiscountRate) + Price * TaxRate 
    end.


