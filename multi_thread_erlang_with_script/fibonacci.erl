

-module(fibonacci).

-export([fib/1]).
-export([start_time/1, print_time/2]).
-export([thread/2, start/0]).
% Run :
% 1> c(fibonacci).
% 2> fibonacci:start().

start_time(Out) ->
    {_,_,Micro} = os:timestamp(),
    {H, M, S} = time(),
	io:format(Out, "~2w:~2..0w:~2..0w.~6..0w", [H, M, S, Micro]).

print_time(N, Out)->
    %io:fwrite(Out, integer_to_list(11 - N) ++ ") 37th Fibonacci calculation, start_time : "),

    io:format(Out, "~s", [integer_to_list(11 - N) ++ ") 37th Fibonacci calculation, start_time : " ] ),

    io:format(Out, "~s~n", [fibonacci:start_time(Out)]).


fib (N) when N < 3 ->
  1;
fib (N) when N >= 3 ->
  fib (N - 1) + fib (N - 2).



thread (N, Out) when N > 0 ->
    print_time(N, Out),
	_tStart = os:timestamp(),
	fib(37),
	_tElapsed = timer:now_diff(os:timestamp(), _tStart),

    io:format(Out, "~s", [integer_to_list(11 - N) ++ ") 37th Fibonacci calculation, completed in "
			++ integer_to_list( trunc(_tElapsed/1000) ) ++ "ms, end_time : "]),
    io:format(Out, "~s~n~n", [fibonacci:start_time(Out)]),


	%io:fwrite(integer_to_list(11 - N) ++ ") 37th Fibonacci calculation, completed in "
	%		++ integer_to_list( trunc(_tElapsed/1000) ) ++ "ms.\n\n"),
    thread ( N - 1, Out);
thread (0, _) ->
    ok.



start() ->
	{ok, Out} = file:open("fibonacci.csv", write),
    spawn(fibonacci, thread, [10, Out]).






%%
