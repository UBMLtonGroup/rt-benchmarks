
-module(gcBenchMP).

-export([nonEmptyNode/2, populate/2, makeTree/1, emptyNode/0,numIters/1,treeSize/1,timeConstruction/1]).
-export([printDiagnostics/0,fillArray/2]).
-export([makeArr/0,kArraySize/0]).
-export([fib/1,simpleLoop/1]).
-export([gc_func/5]).
-export([comp_func/6]).


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




gc_func(PID, Tree_depth, ID, Iterations, I) when Iterations > 0 ->

        
    process_flag(message_queue_data,on_heap),
        %io:format("~w~n",[process_info(self(),total_heap_size)]),
        %Start_time = erlang:timestamp(),
    	
        %stretch the memory space quicklyy
    %
        %if
        %    (Iterations =:= 50) ->  timer:send_interval(500,simpleloop,ping);
        %    true ->ok
        %end,
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
    
	%Stop_time = erlang:timestamp(),
        %Total_time = timer:now_diff (Stop_time,Start_time),
        %io:format("~s~w~s~w~n",["gc:",Iterations,": Time taken = ",Total_time]),

        CompPID = whereis(simpleloop),
        CompPID ! {self(),ping},
        receive
            {CompPID,pong} -> true
        end,
        %timer:sleep(1000),
        
       % io:format("~w~n",[process_info(whereis(simpleloop),message_queue_len)]),
	gc_func(PID, Tree_depth, ID, Iterations - 1 , I + 1 );

gc_func(PID,_,_,0,_) ->
	PID ! gcdone.



comp_func(PID, Depth, ID, Iterations, I, Comp_sleep)->
    process_flag(message_queue_data,on_heap),
    {Mega,Sec,Micro} = erlang:timestamp(),
        simpleLoop(Depth),
        receive 
            {GC,ping} ->
                    GC ! {self(),pong},
                    Stop_time = erlang:timestamp(),
                    %io:format("~s~w~s~w~s~s~n", ["comp:stop:",ID,":",I,":",Stop_time]),
                    % io:format("~w~n",[process_info(self(),total_heap_size)]),
                    % io:format("~w~n",[process_info(self(),memory)]),
                    Total_time = timer:now_diff(Stop_time,{Mega,Sec,Micro}),
                    {_,HeapSize} = process_info(self(),memory), 
                    % io:format("~s~w~s~w~s~w~s~w~n",["comp:",Iterations,": Time taken: ",Total_time,": total memory: ",erlang:memory(processes_used),": comp memory: ",HeapSize ]),
                    io:format("~w~s~w~s~w~s~w~n",[Iterations,":",Total_time,":",erlang:memory(processes_used),":",HeapSize ]),
                    %timer:sleep(Comp_sleep * 1000),
                    comp_func(PID, Depth, ID, Iterations - 1 , I + 1, Comp_sleep)
        end.


main(Args) ->
	_d = lists:nth(1, Args ),
	_i = lists:nth(2, Args ),
	_s = lists:nth(3, Args ),
	_e = lists:nth(4, Args ),
        _hs = lists:nth(5,Args),

	{D, _} = string:to_integer( _d ),
	{I, _} = string:to_integer( _i ),
	{S, _} = string:to_integer( _s ),
	{E, _} = string:to_integer( _e ),
        {HS,_} = string:to_integer(_hs),
       
	io:format("~s~n",["#Iterations:RunTime(comp thread):Total Memory(bytes):Heap used by comp thread(words)"]),
 
        register(simpleloop,spawn_opt(gcBenchMP, comp_func, [self(), D, 1, I , 0, S],[{min_heap_size, 35000}] )),
        spawn_opt(gcBenchMP, gc_func, [self(), E, 1, I , 0],[{max_heap_size, #{size => HS , kill => true, error_logger => true}}]),
%Wait for approx 20 iterations before starting gc thread	
	

      %  9048608
        receive 
            gcdone -> true
        end,

        %receive
        %    compdone -> true
        %end,
	
        %timer:sleep(30000),
	init:stop().




	%% compile
	%erlc gcBench.erl

	%% run
	%erl -noshell -run gcBench main 1 37 10 1 1 10 -s


%
