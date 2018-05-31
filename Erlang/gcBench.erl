
-module(gcBench).

-export([nonEmptyNode/2, populate/2, makeTree/1, emptyNode/0,numIters/1,treeSize/1,timeConstruction/1]).
-export([printDiagnostics/0,fillArray/2]).
-export([makeArr/0,kArraySize/0]).
-export([fib/1,simpleLoop/1]).
-export([start_gc_thread/4, gc_func/5]).
-export([start_comp_thread/5, comp_func/6]).


-export([main/1]).



-define(kMinTreeDepth,4).
-define(kMaxTreeDepth,16).
-define(kStretchTreeDepth,18).

-define(EMPTY_NODE, {node, 'empty'}).
%Tree setup

emptyNode () -> ?EMPTY_NODE.

nonEmptyNode(Left,Right) -> {node,{Left,Right}}.


% Build tree bottom-up
makeTree(IDepth) when IDepth > 0 ->
    nonEmptyNode(makeTree(IDepth - 1), makeTree(IDepth - 1));
makeTree(0) ->
	emptyNode ().


%kStretchTreeDepth() -> 18 .
%kLongLivedTreeDepth() -> 16 .
%kMinTreeDepth() -> 4 .
%kMaxTreeDepth() -> 16 .

kArraySize () -> 500000.




% Build tree top down, assigning to older objects.
%populate(IDepth, {node,#{left:=Left , right:=Right} = N}) when IDepth > 0 ->
%        N = N#{left:= emptyNode(), right := emptyNode()},

%	populate(IDepth - 1, maps:get(left,N)),
%	populate(IDepth - 1, maps:get(right,N));
%

populate(IDepth,ThisNode) when IDepth >0 ->

    case ThisNode of
        {node,'empty'} -> {node,{populate(IDepth-1,emptyNode()),populate(IDepth-1,emptyNode())}};
        Otherwise -> io:format("No match:~p~n",[Otherwise])
    end;
populate(0, _) ->
	ok. 

%populate(IDepth,N) when IDepth > 0 ->
%       N = N#{left:= emptyNode(), right:= emptyNode()},

%       populate(IDepth-1, maps:get(left,N)),
%       populate(IDepth-1,maps:get(right,N)).

treeSize(I) -> (1 bsl (I+1) -1).

numIters(I) ->
    (2 * treeSize(?kStretchTreeDepth)) div treeSize(I).

timeConstruction(Depth) -> 
    INumIters = numIters (Depth),
    %io:format("~s~w~s~w~n",["Creating ",INumIters," trees of depth ", Depth]),
    TopDown = fun Loop(CNT)->
                  case CNT < INumIters of
                    true ->  populate(Depth,emptyNode()), Loop(CNT+1);
                    false -> ok
                  end
              end,
    TopDown(0),

    BottomUp = fun Loop(CNT) ->
                  case CNT < INumIters of
                    true -> makeTree(Depth), Loop(CNT+1);
                    false -> ok
                  end
                end,
    BottomUp(0).
    




printDiagnostics() ->

	application:start(sasl),  % have to run with these two lines (just once)
	application:start(os_mon), % to execute OS_MON
	GetData = memsup:get_system_memory_data(),
	{ _, LtotalMemory} = lists:nth(1, GetData),
	{ _, LfreeMemory} = lists:nth(2, GetData),

	io:fwrite(" Total memory available=" ++ integer_to_list(LtotalMemory) ++ " bytes"),
	io:fwrite("  Free memory=" ++ integer_to_list(LfreeMemory) ++ " bytes\n").


%making an empty arrayerlang global variable
makeArr() -> array:new(kArraySize(), {default,0.0}).

fillArray (NewArr,C) when C > 0 ->
    NewVal = 1.0 / (C * 1.0),
    TempArr = array:set(C,NewVal,NewArr),
    fillArray(TempArr,C-1);
fillArray(NewArr,0) ->
    array:set(0, 1.0,NewArr).




fib(0) -> 0 ; 
fib(1) -> 1 ; 
fib(N) when N > 0 -> fib(N-1) + fib(N-2) .

simpleLoop(I) when I > 0->
   simpleLoop(I-1);
simpleLoop(0)-> ok.



start_gc_thread(Num_threads, Tree_depth, Iterations, ID) when Num_threads > 0 ->

	io:format("~s~w~n", ["starting GC thread #", ID] ),

	spawn_opt(gcBench, gc_func, [self(), Tree_depth, ID, Iterations , 0],[{max_heap_size, #{size => 7048608, kill => true, error_logger => true}}]),
	
        start_gc_thread(Num_threads - 1, Tree_depth, Iterations, ID + 1);

start_gc_thread(0, _, _, _) ->
	ok.

gc_func(PID, Tree_depth, ID, Iterations, I) when Iterations > 0 ->

        
        %io:format("~w~n",[process_info(self(),total_heap_size)]),
        Start_time = erlang:timestamp(),
    	
        %stretch the memory space quicklyy
        makeTree(Tree_depth),

        %create a long lived object

        LongLivedTree = emptyNode (),
        populate(Tree_depth,LongLivedTree),
        
        %TempVal = kArraySize()/2,

        LongLivedArray = fillArray(makeArr(),250000),
        
        D = ?kMinTreeDepth,

        Constr = fun While(A) ->
                    case A =< Tree_depth of
                        true -> timeConstruction(A),While(A+2);
                        false -> ok
                    end
                 end,

        Constr(D),
    
        ArrVal =  array:get(1000,LongLivedArray),

        if
            ArrVal /= (1.0/1000) -> io:format("Failed~n");
            true -> ok
        end,
    
	Stop_time = erlang:timestamp(),
        Total_time = timer:now_diff (Stop_time,Start_time),
        io:format("~s~w~s~w~n",["gc:",ID,": Time taken = ",Total_time]),
        
        timer:sleep(3000),

	gc_func(PID, Tree_depth, ID, Iterations - 1 , I + 1 );

gc_func(PID,_,_,0,_) ->
	PID ! done.

				%    1			37       10        1      1
start_comp_thread(Num_threads, Depth, Iterations, ID, Comp_sleep) when Num_threads > 0 ->
	io:format("~s~w~n", ["starting computing thread #", ID] ),

	spawn(gcBench, comp_func, [self(), Depth, ID, Iterations , 0, Comp_sleep] ),

        start_comp_thread(Num_threads - 1, Depth, Iterations, ID + 1, Comp_sleep);

start_comp_thread(0, _, _, _, _) ->
	ok.


comp_func(PID, Depth, ID, Iterations, I, Comp_sleep) when Iterations > 0 ->
	Start_time = erlang:timestamp(),
	%io:format("~s~w~s~w~s~s~n", ["comp:start:",ID,":",I,":",Start_time]),
	fib(Depth),

	Stop_time = erlang:timestamp(),
	%io:format("~s~w~s~w~s~s~n", ["comp:stop:",ID,":",I,":",Stop_time]),
        
       % io:format("~w~n",[process_info(self(),total_heap_size)]),
       % io:format("~w~n",[process_info(self(),memory)]),
        Total_time = timer:now_diff(Stop_time,Start_time),
        io:format("~s~w~s~w~n",["comp:",ID,": Time taken = ",Total_time]),

	timer:sleep(Comp_sleep * 1000),
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
	%{G, _} = string:to_integer( _g ),
	%{E, _} = string:to_integer( _e ),
        
        if
		(T > 0 ) ->
			start_comp_thread(T,D,I,1,S);
		true -> ok
	end,

       
	%if
	%	(G > 0 ) ->
	%		start_gc_thread(G,E,I,1);
	%	true -> ok
	%end,

         
	timer:sleep(30000),
	init:stop().




	%% compile
	%erlc gcBench.erl

	%% run
	%erl -noshell -run gcBench main 1 37 10 1 1 10 -s


%
