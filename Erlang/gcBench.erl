
-module(gcbench).

-export([make_node/2, make_empty_tree/0, populate/2, makeTree/1]).
-export([printDiagnostics/0]).
-export([makeArr/0, loop_arr/3]).
-export([fib/1]).
-export([start_gc_thread/4, gc_func/5]).
-export([start_comp_thread/5, comp_func/6]).

-export([kStretchTreeDepth/0]).
-export([kLongLivedTreeDepth/0]).
-export([kArraySize/0]).
-export([kMinTreeDepth/0]).
-export([kMaxTreeDepth/0]).

-export([getTime/0]).
-export([main/1]).


%Tree setup
make_node(Left,Right) -> {Left,Right,0,0}.
make_empty_tree() -> {nil,nil,0,0}.

kStretchTreeDepth() -> 18 .
kLongLivedTreeDepth() -> 16 .
kArraySize() -> 500000 .
kMinTreeDepth() -> 4 .
kMaxTreeDepth() -> 16 .


getTime () ->
	{Time1, Time2, Decimals} = erlang:timestamp(),
	Time = integer_to_list(Time1) ++ integer_to_list(Time2) ++ "." ++ integer_to_list(Decimals),
	Time.


% Build tree top down, assigning to older objects.
populate(IDepth, ThisNode) when IDepth > 0 ->
	ThisNode_fill_left = setelement(1, ThisNode, make_empty_tree()),
	ThisNode_fill_all =  setelement(2, ThisNode_fill_left, make_empty_tree()),

	populate(IDepth - 1, element(1,ThisNode_fill_all) ),
	populate(IDepth - 1, element(2,ThisNode_fill_all) );

populate(0, _) ->
	ok.


% Build tree bottom-up
makeTree(IDepth) when IDepth > 0 ->
    make_node(makeTree(IDepth - 1), makeTree(IDepth - 1));
makeTree(0) ->
	make_empty_tree().


printDiagnostics() ->

	application:start(sasl),  % have to run with these two lines (just once)
	application:start(os_mon), % to execute OS_MON
	GetData = memsup:get_system_memory_data(),
	{ _, LtotalMemory} = lists:nth(1, GetData),
	{ _, LfreeMemory} = lists:nth(2, GetData),

	io:fwrite(" Total memory available=" ++ integer_to_list(LtotalMemory) ++ " bytes"),
	io:fwrite("  Free memory=" ++ integer_to_list(LfreeMemory) ++ " bytes\n").


%making an empty array
makeArr() -> array:new(kArraySize() , {default,0.0}).


% a loop for generating an array
loop_arr(N, A, K_size) when N=< K_size ->  % no support for Infinity in erlang (first index), filling one more in the end instead
	A1 = array:set(round(N), 1.0 / N , A),
	loop_arr( N + 1.0 , A1, K_size);
loop_arr(N, A, K_size) when N>K_size ->
	A.



fib (N) when N < 3 ->
	1;
fib (N) when N >= 3 ->
	fib (N - 1) + fib (N - 2).


start_gc_thread(Num_threads, Tree_depth, Iterations, ID) when Num_threads > 0 ->

	Array = makeArr(),
	Array_upd = loop_arr(1.0, Array, kArraySize()/2 ),
	array:get(1000, Array_upd), % not assigning to a variable; gives a warning if the variable is not used

	makeTree(Tree_depth), % not assigning to a variable

	io:format("~s~w~n", ["starting GC thread #", ID] ),

	spawn(gcbench, gc_func, [self(), Tree_depth, ID, Iterations , 0]),
	receive done ->
		start_gc_thread(Num_threads - 1, Tree_depth, Iterations, ID + 1)
	end;
start_gc_thread(0, _, _, _) ->
	ok.

gc_func(PID, Tree_depth, ID, Iterations, I) when Iterations > 0 ->
	Start_time = getTime(),
	io:format("~s~w~s~w~s~s~n", ["gc:start:", ID, ":", I, ":",Start_time]),
	makeTree(Tree_depth),
	populate(Tree_depth, make_empty_tree()),
	Stop_time = getTime(),
	io:format("~s~w~s~w~s~s~n", ["gc:stop:",ID,":",I,":",Stop_time]),
	gc_func(PID, Tree_depth, ID, Iterations - 1 , I + 1 );
gc_func(PID,_,_,0,_) ->
	PID ! done.

				%    1			37       10        1      1
start_comp_thread(Num_threads, Depth, Iterations, ID, Comp_sleep) when Num_threads > 0 ->
	io:format("~s~w~n", ["starting computing thread #", ID] ),

	spawn(gcbench, comp_func, [self(), Depth, ID, Iterations , 0, Comp_sleep] ),

	receive done ->
		start_comp_thread(Num_threads - 1, Depth, Iterations, ID + 1, Comp_sleep)
	end;
start_comp_thread(0, _, _, _, _) ->
	ok.


comp_func(PID, Depth, ID, Iterations, I, Comp_sleep) when Iterations > 0 ->
	Start_time = getTime(),
	io:format("~s~w~s~w~s~s~n", ["comp:start:",ID,":",I,":",Start_time]),
	fib(Depth),

	Stop_time = getTime(),
	io:format("~s~w~s~w~s~s~n", ["comp:stop:",ID,":",I,":",Stop_time]),
	%timer:sleep(Comp_sleep * 1000),
	comp_func(PID, Depth, ID, Iterations - 1 , I + 1, Comp_sleep);
comp_func(PID,_,_,0,_,_)->
	PID ! done.


main(Args) ->
	_t = lists:nth(1, Args ),
	_d = lists:nth(2, Args ),
	_i = lists:nth(3, Args ),
	_s = lists:nth(4, Args ),
	_g = lists:nth(5, Args ),
	_e = lists:nth(6, Args ),

	{T, _} = string:to_integer( _t ),
	{D, _} = string:to_integer( _d ),
	{I, _} = string:to_integer( _i ),
	{S, _} = string:to_integer( _s ),
	{G, _} = string:to_integer( _g ),
	{E, _} = string:to_integer( _e ),

	if
		(G > 0 ) ->
			start_gc_thread(G,E,I,1);
		true -> ok
	end,

	if
		(T > 0 ) ->
			start_comp_thread(T,D,I,1,S);
		true -> ok
	end,

	timer:sleep(3000),
	init:stop().




	%% compile
	%erlc gcbench.erl

	%% run
	%erl -noshell -run gcbench main 1 37 10 1 1 10 -s


%
