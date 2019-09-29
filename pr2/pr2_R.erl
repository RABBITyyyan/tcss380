% **
%  * pr2_R.erl
%  * @author Chang Yan (Charlotte)
%  * 
%  * This project finds a circular train constructed from all domino tiles from the
%  * given set of dominoes using a list of tuples to represent dominoes.
%  *
%  */

-module(pr2_R).
-compile(export_all).

% /** 
%  * dominoes( N ): Given an integer, returns a list of tuples containing 
%  * all the tiles in the set of double-N dominoes.
%  */
dominoes( 0 ) -> [ {0, 0} ];   
dominoes( N ) -> 
    [ {N, N} ] ++ dominoes_helper( N, N - 1 ) ++ dominoes( N - 1 ).

% /** 
%  * dominoes_helper(X, Y): Helper function for dominoes( N ). Given two integers X and Y, 
%  * returns a list of tuples containing all the tiles in the set of double-X dominoes.
%  * For example, given (2, 1), return [ {2 , 1}, {2 , 0} ].
%  * For example, given (1, 0), return [ {1 , 0} ].
%  */
dominoes_helper( X, 0 ) -> [ {X, 0} ];
dominoes_helper( X, Y ) ->
	[ {X, Y} ] ++ dominoes_helper(X, Y - 1).
 
% /** 
%  * giveRandom( MyList ): given a list of tuples, returns randomly assembled list of tuples where the 
%  * algorithm picks a random tuple to begin the list and then picks randomly another tuple that
%  * matches the preceding one in the fashion described before; the function returns when it can find
%  * no more matches; the random number generator must be seeded properly so that each time the
%  * function is called, it results in a different outcome.
%  */
giveRandom( [] ) -> [];
giveRandom( MyList ) -> 
	flip( MyList ),
	OriginalList = MyList,
	giveRandomHelper( MyList, OriginalList ).

% /** 
%  * giveRandomHelper( MyList, OriginalList ): helper function for giveRandom( MyList ).
%  * Returns a list of tuples that forms a circular train but need to be flipped.
%  */
giveRandomHelper( MyList, OriginalList ) -> 
	if 
		length( MyList ) == 0 ->
			io:format( "no ring formed" );
		true ->
			Elem = lists:nth( rand:uniform( length( MyList )), MyList ),
			Rest = lists:delete( Elem, MyList ),
			RestElems = lists:delete( Elem, OriginalList ),
			NewList = [ Elem ] ++ getNext( Elem, RestElems, RestElems ),
			Res = isRing( NewList ),
		if 
			Res == true ->
				NewList;
			true ->
				giveRandomHelper( Rest, OriginalList )
		end
	end.

% /** 
%  * getNext( {X, Y}, List, TempList ): helper function for giveRandom( MyList ).
%  * Given a tuple, returns all its neighbors that forms a circular train.
%  */
getNext( _, [], TempList ) -> TempList;
getNext( { X, Y }, List, TempList ) -> 
	if 
		( Y == element( 1, hd( List )))  -> 
			{ X1, Y1 } = hd( List ),
			NewList = lists:delete( { X1, Y1 }, TempList ),
			[] ++ [ {X1, Y1} ] ++ getNext( {X1, Y1}, NewList, NewList );		
		Y == element( 2, hd( List ))  ->
			{ X1, Y1 } = hd( List ),
			NewList = lists:delete( { X1, Y1 }, TempList ),
			[] ++ [ { X1, Y1 } ] ++ getNext( { Y1, X1 }, NewList, NewList );
		true ->
			getNext( {X,Y}, tl(List), TempList )
	end.

% /** 
%  * isRing( List ): Given a list of tuples, returns true if tuples form a ring and all tiles from a
%  * double-N domino set have been used, or false otherwise.
%  */
isRing( [] ) -> false;
isRing( [ {_,_} ] ) -> false;
isRing( List ) ->
	OriginalList = List,
	Res = isRingHelper( List, OriginalList ),
	if 
		Res -> true;
		true -> 
			NewList = flip( List ),
			isRingHelper( NewList, OriginalList )
	end.
	
% /** 
%  * isRingHelper( List, OriginalList ): Helper function for isRing( List ).
%  * Given a list of tuples, checks if the list which needs to be flipped is also a ring.
%  * Returns true if tuples form a ring and all tiles from a
%  * double-N domino set have been used, or false otherwise.
%  */	
isRingHelper ([{_, _}], _) -> true;	
isRingHelper( List, OriginalList ) -> 
	LastElem = lists:nth(length(List), List),
	if 
		element(1, hd(OriginalList)) == element(2, LastElem) ->
			if 
				element(2, hd(List)) == element(1, hd(tl(List))) -> 
					isRingHelper(tl(List), OriginalList);
			true -> 
				false		
			end;
		true -> 
			false
	end.

% /** 
%  * flip( List ): Given a list of tuples that form a circular train, 
%  * returns a list with tiles that are flipped where
%  * appropriate to make the loop self-evident.
%  */
flip( [] ) -> [];
flip( List ) -> flipHelper( hd( List ), tl( List ), [] ).

% /** 
%  * flipHelper( Tuple, List, NewList ): Given the tuple as a start, 
%  * find all the tuples that can be connect with the start tuple, and flip them,
%  * then add them into the NewList. 
%  */
flipHelper( { X1, Y1 }, [], List ) -> List ++ [ { X1, Y1 } ];
flipHelper( { X1, Y1 }, [ { X2, Y2 } | T ], List) -> 
	if 
		Y1 == X2 ->
			flipHelper( { X2, Y2 }, T, List ++ [ { X1, Y1 } ]);
		true ->
			flipHelper( { Y2, X2 }, T, List ++ [ { X1, Y1 } ])
	end.

% /** 
%  * solution( N ): Given an integer, generates a solution – ties all the functions 
%  * given above in a functional manner.
%  */
solution( N ) -> 
	if 
		N rem 2 == 1 ->
			[];
		true ->
			flip(giveRandom(dominoes( N )))
	end.

% /** 
%  * listAsString( MyList ): Given a list of tuples, returns a string representing that list 
%  */
listAsString( [] ) -> "[]";
listAsString( List ) ->
    TuplesAsString = string:join( lists:map( fun format_list/1, List ), ", " ),
    "[ " ++ TuplesAsString ++ " ]".
 
% /** 
%  * format_list( {X, Y} ): Helper method for listAsString to tie all the tuples together.
%  */   
format_list( {X, Y} ) ->
    lists:flatten( io_lib:format("{~p, ~p}", [ X, Y ] )).

% /** 
%  * driver(F1, F2, N): Given listAsString and solution functions, 
%  * as well as N - the initial highest number of dots
%  * for the set, returns a string representing a solution to the program
%  */
driver(F1, F2, N) -> 
	F1( F2( N ) ).

% experiment
% /** 
%  * shuffle( MyList ): Randomly shuffle a list of tuples. 
%  */
shuffle( [] ) -> [];
shuffle( [ X ] ) -> [ X ];
shuffle( MyList ) -> 
	shuffle_helper1( MyList, length( MyList ), [] ).

% /** 
%  * shuffle_helper1: Helper method for shuffle. Get the seed for the rand function recursively. 
%  */
shuffle_helper1([], 0, RandomList) -> RandomList;
shuffle_helper1( MyList, Length, RandomList ) ->
	{ Shuffled, UnShuffled } = shuffle_helper2( rand:uniform( Length ), MyList ),
	shuffle_helper1( UnShuffled, Length - 1, [ Shuffled | RandomList ]).

% /** 
%  * shuffle_helper2: Helper method for shuffle. Generates the shuffled list.
%  */
shuffle_helper2( N, MyList ) -> 
	shuffle_helper2( N, MyList, [] ).
shuffle_helper2(1, [ H | MyList], Rest ) -> 
	{ H, Rest ++ MyList };
shuffle_helper2( N, [ H | MyList ], Rest ) -> 
	shuffle_helper2( N - 1, MyList, [ H | Rest ] ).

