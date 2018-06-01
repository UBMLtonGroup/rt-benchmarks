
-module(complex).

-export([fib/1,simpleLoop/1]).
-export([start_fib_thread/4, fib_func/4]).
-export([start_comp_thread/5, comp_func/6]).


-export([main/1]).





fib(0) -> 0 ; 
fib(1) -> 1 ; 
fib(N) when N > 0 -> fib(N-1) + fib(N-2) .

simpleLoop(I) when I > 0->
   simpleLoop(I-1);
simpleLoop(0)-> ok.


start_fib_thread(Num_threads, Depth, Iterations, ID) when Num_threads > 0 ->
	io:format("~s~w~n", ["starting fib thread #", ID] ),

	spawn(complex, fib_func, [self(), Depth, ID, Iterations] ),

        start_fib_thread(Num_threads - 1, Depth, Iterations, ID + 1);

start_fib_thread(0, _, _, _) ->
	ok.

fib_func(PID, Depth, ID, Iterations) when Iterations > 0 ->
	%Start_time = erlang:timestamp(),
	fib(Depth),

	%Stop_time = erlang:timestamp(),
        %Total_time = timer:now_diff(Stop_time,Start_time),
	%{_,HeapSize} = process_info(self(),memory), 
        %io:format("~s~w~s~w~s~w~s~w~n",["fib:",Iterations,": Time taken: ",Total_time,": total memory: ",erlang:memory(processes_used),": comp memory: ",HeapSize ]),

	timer:sleep(1000),
	fib_func(PID, Depth, ID, Iterations - 1);
fib_func(PID,_,_,0)->
	PID ! fibdone.



start_comp_thread(Num_threads, Depth, Iterations, ID, Comp_sleep) when Num_threads > 0 ->
	io:format("~s~w~n", ["starting computing thread #", ID] ),

	spawn(complex, comp_func, [self(), Depth, ID, Iterations , 0, Comp_sleep] ),

        start_comp_thread(Num_threads - 1, Depth, Iterations, ID + 1, Comp_sleep);

start_comp_thread(0, _, _, _, _) ->
	ok.


comp_func(PID, Depth, ID, Iterations, I, Comp_wait) when Iterations > 0 ->
	Start_time = erlang:timestamp(),
	%io:format("~s~w~s~w~s~s~n", ["comp:start:",ID,":",I,":",Start_time]),
	simpleLoop(Depth),

	Stop_time = erlang:timestamp(),
	%io:format("~s~w~s~w~s~s~n", ["comp:stop:",ID,":",I,":",Stop_time]),
        
       % io:format("~w~n",[process_info(self(),total_heap_size)]),
       % io:format("~w~n",[process_info(self(),memory)]),
        Total_time = timer:now_diff(Stop_time,Start_time),
	{_,HeapSize} = process_info(self(),memory), 
        io:format("~s~w~s~w~s~w~s~w~n",["comp:",Iterations,": Time taken: ",Total_time,": total memory: ",erlang:memory(processes_used),": comp memory: ",HeapSize ]),

	%timer:sleep(Comp_sleep * 1000),

        if
            Iterations =:= Comp_wait -> PID ! startfib;
            true -> ok
        end,

	comp_func(PID, Depth, ID, Iterations - 1 , I + 1, Comp_wait);
comp_func(PID,_,_,0,_,_)->
	PID ! compdone.


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
		(T > 0 ) ->
			start_comp_thread(T,D,I,1,S);
		true -> ok
	end,

        timer:sleep(3000),     

        receive 
            startfib -> if
                            (G > 0 ) ->	start_fib_thread(G,E,I,1);
		            true -> ok
	                end
        end,

        
%        receive 
%            fibdone -> true
%        end,

        receive
            compdone -> true
        end,
	
        %timer:sleep(30000),
	init:stop().




	%% compile
	%erlc gcBench.erl

	%% run
	%erl -noshell -run gcBench main 1 37 10 1 1 10 -s


%
