% Lab 05
% Chang Yan, Meng Yang
-module(bst).

-export([insert/2, contains/2, sizeIs/1, buildTree/1, helper/2]).

-type bst() :: empty
    | { integer(), bst(), bst()}.


insert(N, Tree) ->
    case Tree of
	empty -> {N, empty, empty};
	{Value, Left, Right} ->
	   if 
	      N < Value -> {Value, insert(N, Left), Right};
	      N > Value -> {Value, Left, insert(N, Right)}
	   end
    end.

contains(N, Tree) ->
    case Tree of
	empty->
	    false;
	{Value, Left, Right} -> 
	    if 
		N == Value ->
		    true;
		N < Value -> contains(N, Left);
		N > Value -> contains(N, Right)
	   end
    end.  

sizeIs(Tree) -> 
    case Tree of
		empty -> 0;
		{_Value, Left, Right} -> 1 + sizeIs(Left) + sizeIs(Right)
    end.  

buildTree([]) -> empty;
buildTree([X|Xs]) -> helper([X|Xs], empty).	

helper([], Tree) -> Tree;
helper([X|Xs], Tree) -> helper(Xs, insert(X, Tree)).

