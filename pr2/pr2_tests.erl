% **
%  * pr2_tests.erl
%  * @author Chang Yan (Charlotte)
%  * 
%  * This project is the unit tests for pr2_R.erl.
%  *
%  */
-module(pr2_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

% /** 
%  * dominoes_1_test(): test for double-0 domino.
%  */
dominoes_1_test() ->
    ?assertEqual([{0,0}], pr2_R:dominoes(0)).

% /** 
%  * dominoes_2_test(): test for double-2 domino.
%  */
dominoes_2_test() ->
    ?assertEqual([{2,2},{2,1},{2,0},{1,1},{1,0},{0,0}], pr2_R:dominoes(2)).

% /** 
%  * dominoes_3_test(): test for double-3 domino.
%  */
dominoes_3_test() ->
    ?assertEqual([{3,3},{3,2},{3,1},{3,0},{2,2},{2,1},{2,0},{1,1},{1,0},{0,0}], pr2_R:dominoes(3)).

% /** 
%  * giveRandom_1_test(): test for giveRandom() 
%  * (should return a list of tuples that forms a circular train).
%  */
giveRandom_1_test() ->
    true = true == pr2_R:isRing(pr2_R:giveRandom([{2,1},{1,1},{1,0},{0,0},{2,0},{2,2}])).

% /** 
%  * giveRandom_2_test(): test for giveRandom() 
%  * (should return a list of tuples that forms a circular train).
%  */
giveRandom_2_test() ->
    true = true == pr2_R:isRing(pr2_R:giveRandom([{2,2},{2,1},{2,0},{1,1},{1,0},{0,0}])).

% /** 
%  * giveRandom_3_test(): test for giveRandom() 
%  * (should return a list of tuples that forms a circular train).
%  */
giveRandom_3_test() ->
    true = true == pr2_R:isRing(pr2_R:giveRandom([{4,4},{4,3},{4,2},{4,1},{4,0},{3,3},{3,2},{3,1},{3,0},{2,2},{2,1},{2,0}, {1,1},{1,0},{0,0}])).

% /** 
%  * isRing_1_test(): test for isRing() 
%  * (should return true in this case).
%  */
isRing_1_test() ->
    true = true == pr2_R:isRing([{2,1},{1,1},{1,0},{0,0},{2,0},{2,2}]).

% /** 
%  * isRing_2_test(): test for isRing() 
%  * (should return true in this case).
%  */
isRing_2_test() ->
    true = true == pr2_R:isRing([{2,1},{1,1},{1,0},{0,0},{0,2},{2,2}]).

% /** 
%  * isRing_1_test(): test for isRing() 
%  * (should return false in this case).
%  */
isRing_3_test() ->
    true = false == pr2_R:isRing([{2,1},{1,1},{0,0},{2,0},{2,2}]).

% /** 
%  * isRing_4_test(): test for isRing() 
%  * (should return true in this case).
%  */
isRing_4_test() ->
    true = true == pr2_R:isRing([{2,1},{1,1},{1,0},{0,0},{0,2}]).

% /** 
%  * isRing_5_test(): test for isRing() 
%  * (should return true in this case).
%  */
isRing_5_test() ->
    true = true == pr2_R:isRing([{3,1},{1,1},{1,0},{0,3}]).

% /** 
%  * isRing_6_test(): test for isRing() 
%  * (should return false in this case).
%  */
isRing_6_test() ->
    true = false == pr2_R:isRing([]).

% /** 
%  * flip_1_test: test for flip() 
%  * (should equal a flipped list of tuples that forms a circular train).
%  */
flip_1_test() ->
    ?assertEqual([{2,1},{1,1},{1,0},{0,0},{0,2},{2,2}], pr2_R:flip([{2,1},{1,1},{1,0},{0,0},{2,0},{2,2}])).

% /** 
%  * flip_2_test: test for flip() 
%  * (should equal a flipped list of tuples that forms a circular train).
%  */
flip_2_test() ->
    ?assertEqual([{2,1},{1,1},{1,0},{0,0},{0,2},{2,2}], pr2_R:flip([{2,1},{1,1},{0,1},{0,0},{2,0},{2,2}])).

% /** 
%  * flip_3_test: test for flip() 
%  * (should equal a flipped list of tuples that forms a circular train).
%  */
flip_3_test() ->
    ?assertEqual([{2,1},{1,1},{1,0},{0,0},{0,2}], pr2_R:flip([{2,1},{1,1},{1,0},{0,0},{2,0}])).

% /** 
%  * solution_1_test: test for solution() 
%  * (should equal a flipped list of tuples that forms a circular train).
%  */
solution_1_test() ->
	true = true == pr2_R:isRing(pr2_R:solution(2)).

% /** 
%  * solution_2_test: test for solution() 
%  * (should equal a flipped list of tuples that forms a circular train).
%  */
solution_2_test() ->
	true = true == pr2_R:isRing(pr2_R:solution(4)).

% /** 
%  * solution_3_test: test for solution() 
%  * (should equal a flipped list of tuples that forms a circular train).
%  */
solution_3_test() ->
	true = true == pr2_R:isRing(pr2_R:solution(6)).

% /** 
%  * solution_4_test: test for solution() 
%  * (should equal a flipped list of tuples that forms a circular train).
%  */
solution_4_test() ->
	true = true == pr2_R:isRing(pr2_R:solution(8)).

% /** 
%  * listAsString_1_test: test for listAsString() 
%  * (should equal a string representation of a list of tuples).
%  */
listAsString_1_test() ->
    ?assertEqual("[ {2, 1}, {1, 1}, {1, 0}, {0, 0}, {0, 2}, {2, 2} ]", pr2_R:listAsString([{2,1},{1,1},{1,0},{0,0},{0,2},{2,2}])).

% /** 
%  * listAsString_2_test: test for listAsString() 
%  * (should equal a string representation of a list of tuples).
%  */
listAsString_2_test() ->
    ?assertEqual("[]", pr2_R:listAsString([])).

% /** 
%  * listAsString_3_test: test for listAsString() 
%  * (should equal a string representation of a list of tuples).
%  */
listAsString_3_test() ->
    ?assertEqual("[ {3, 1}, {1, 1}, {1, 0}, {0, 3} ]", pr2_R:listAsString([{3,1},{1,1},{1,0},{0,3}])).

% /** 
%  * driver_1_test: test for driver() 
%  * (should return a string representation of a list of tuples that forms a circular train).
%  */
driver_1_test() -> 
	pr2_R:driver(fun pr2_R:listAsString/1, fun pr2_R:solution/1, 2).

% /** 
%  * driver_2_test: test for driver() 
%  * (should return a string representation of an empty list).
%  */
driver_2_test() -> 
	pr2_R:driver(fun pr2_R:listAsString/1, fun pr2_R:solution/1, 3).

% /** 
%  * driver_3_test: test for driver() 
%  * (should return a string representation of a list of tuples that forms a circular train).
%  */
driver_3_test() -> 
	pr2_R:driver(fun pr2_R:listAsString/1, fun pr2_R:solution/1, 4).

% /** 
%  * driver_4_test: test for driver() 
%  * (should return a string representation of a list of tuples that forms a circular train).
%  */
driver_4_test() -> 
	pr2_R:driver(fun pr2_R:listAsString/1, fun pr2_R:solution/1, 6).

% /** 
%  * driver_1_test: test for driver() 
%  * (should return a string representation of a list of tuples that forms a circular train).
%  */
shuffle_1_test() -> 
	pr2_R:shuffle([{2,1},{1,1},{1,0},{0,0},{0,2}]).

% /** 
%  * driver_1_test: test for driver() 
%  * (should return a string representation of a list of tuples that forms a circular train).
%  */
shuffle_2_test() -> 
	pr2_R:shuffle([{2,1},{1,1},{1,0},{0,0},{0,2}]).
