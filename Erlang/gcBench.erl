
-module(gcBench).

-export([make_node/2]).
-export([make_empty_tree/0]).

-export([kStretchTreeDepth/0]).
-export([kLongLivedTreeDepth/0]).
-export([kArraySize/0]).
-export([kMinTreeDepth/0]).
-export([kMaxTreeDepth/0]).

-export([treeSize/1]).
-export([numIters/1]).

-export([populate/2]).
-export([makeTree/1]).

-export([populate_loop/2]).
-export([makeTree_loop/2]).
-export([timeConstruction/1]).

-export([printDiagnostics/0]).

-export([makeArr/0]).
-export([loop_arr/3]).

-export([loop_timeConstruction/2]).

-export([main/0]).


%Tree setup
make_node(Left,Right) -> {Left,Right,0,0}.
make_empty_tree() -> {nil,nil,0,0}.


%variables (need to be defined as functions in Erlang)
kStretchTreeDepth() -> 18 .
kLongLivedTreeDepth() -> 16 . 
kArraySize() -> 500000 . 
kMinTreeDepth() -> 4 .
kMaxTreeDepth() -> 16 .

% Nodes used by a tree of a given size
treeSize(X) -> round(math:pow(2,X+1) - 1.0).

% Number of iterations to use for a given tree depth
numIters(X) -> trunc((2 * treeSize(kStretchTreeDepth())) / treeSize(X)).


% Build tree top down, assigning to older objects.
populate(IDepth, ThisNode) when IDepth > 0 ->
	IDepth_upd = IDepth - 1,
	ThisNode_fill_left = setelement(1, ThisNode, make_empty_tree()),
	ThisNode_fill_all =  setelement(2, ThisNode_fill_left, make_empty_tree()),
	
	populate(IDepth_upd, element(1,ThisNode_fill_all) ),
	populate(IDepth_upd, element(2,ThisNode_fill_all) );
	
populate(0, _) ->
	ok.

% Build tree bottom-up 
makeTree(IDepth) when IDepth > 0 ->
    make_node(makeTree(IDepth - 1), makeTree(IDepth - 1));
makeTree(0) ->
	make_empty_tree().



printDiagnostics() ->
	%_getData = memsup:get_system_memory_data(),
	% no support for memsup on Linux, will try with memsup later.
	{_,Total} = lists:nth(1, erlang:memory()),
	{_,Processes} = lists:nth(2, erlang:memory()),
	io:fwrite(" Total memory available=" ++ integer_to_list(Total) ++ " bytes"), 
	io:fwrite("  Free memory=" ++ integer_to_list(Processes) ++ " bytes\n").


populate_loop(N, Depth) when N > 0 ->
	populate(Depth, make_empty_tree()),
	populate_loop(N-1, Depth);
populate_loop(0, _) ->
	ok.

makeTree_loop(N, Depth) when N > 0 ->
	makeTree(Depth),
	makeTree_loop(N-1, Depth);
makeTree_loop(0, _) ->
	ok.


timeConstruction(Depth) ->
	INumIters = numIters(Depth),
	io:fwrite("Creating " ++ integer_to_list(INumIters) ++
			  " trees of depth " ++ integer_to_list(Depth) ++ "\n"),

	statistics(wall_clock),

	populate_loop(INumIters, Depth),
	{_, _tFinish} = statistics(wall_clock),
	io:fwrite("\tTop down construction took " 
			++ integer_to_list(_tFinish) ++ "msecs\n"),
	
	statistics(wall_clock),

	makeTree_loop(INumIters, Depth),
	{_, _tFinish2} = statistics(wall_clock),

	io:fwrite("\tBottom up construction took " 
			++ integer_to_list(_tFinish2) ++ "msecs\n").


%making an empty array
makeArr() -> array:new(kArraySize() , {default,0.0}).

% a loop for generating an array
loop_arr(N, A, K_size) when N=< K_size ->  % no support for Infinity in erlang (first index), filling one more in the end instead
	A1 = array:set(round(N), 1.0 / N , A),
	loop_arr( N + 1.0 , A1, K_size);
loop_arr(N, A, K_size) when N>K_size ->
	A.

% a loop for generating timeConstruction
loop_timeConstruction(N, _kMaxTreeDepth) when N =< _kMaxTreeDepth ->
	timeConstruction(N),
	loop_timeConstruction(N+2, _kMaxTreeDepth);
loop_timeConstruction(N, _kMaxTreeDepth) when N> _kMaxTreeDepth ->
	ok.


main() ->
	io:fwrite("Garbage Collector Test\n"),
	io:fwrite(" Stretching memory with a binary tree of depth " 
			  ++ integer_to_list(kStretchTreeDepth()) ++"\n" ),
	printDiagnostics(),

	_tStart = erlang:now(),

	% Stretch the memory space quickly
	_tempTree = makeTree(kStretchTreeDepth()),

	% Create a long lived object
	io:fwrite(" Creating a long-lived binary tree of depth " 
			  ++  integer_to_list(kLongLivedTreeDepth()) ++"\n" ),

	_longLivedTree = make_empty_tree(),
	populate(kLongLivedTreeDepth(), _longLivedTree),

	% Create long-lived array, filling half of it
	io:fwrite(" Creating a long-lived array of " 
			  ++  integer_to_list(kArraySize()) ++" floats\n" ),
	_array = makeArr(),
	io:fwrite( float_to_list(array:get(0, _array)) ++ "\n"),	
	_array_upd = loop_arr(1.0, _array, kArraySize()/2 ),

	printDiagnostics(),

	loop_timeConstruction(kMinTreeDepth(), kMaxTreeDepth()),
	_array_if = array:get(1000, _array_upd),

	if
		(_longLivedTree == nil) or ( _array_if /= 1.0/1000  ) ->
			io:fwrite("Failed\n");
		true -> ok
	end,

	_tFinish = erlang:now(),
	_tElapsed = timer:now_diff(_tFinish, _tStart),
	printDiagnostics(),

	io:fwrite("Completed in " 
			++ integer_to_list( trunc(_tElapsed/1000) ) ++ "ms.\n").




