-module(perm9).

-export([one2n/2]).
-export([sum_element/1, sum_append/1, sum_lists/2]).
-export([permutations/1]).
-export([factorial/1]).
-export([perm9_benchmark/2, permutation_loop/2]).
-export([main/0]).

%-compile([debug_info, export_all]).


% N - number ( integer ) , L - list ( [] )
one2n(N, L) when N > 0->
	one2n( N - 1, [N|L]);
one2n(0, L) ->
	L.


sum_element(X) ->
  lists:foldl(fun(L, Sum) -> L + Sum end,
                0, X).
sum_append(X) ->
  lists:append(X).
%N = length of list X
sum_lists(_, N) when N == 0 ->
  0;
sum_lists(X, N) when N > 0 ->
  sum_element(sum_append(X)).


permutations([]) -> [[]];
permutations(L)  -> [[H|T] || H <- L, T <- permutations(L--[H])].


factorial(N) when N > 1 ->
  N * factorial(N-1);
factorial(1) ->
  1.


permutation_loop(Perms, M) when M > 0 ->
    NewPerms = permutations( hd(Perms)),  %%%%%
    permutation_loop(NewPerms, M-1);
permutation_loop(Perms, _) ->
    Perms.


perm9_benchmark(M, N) ->
    %factorial(N), later
    Temp_perms = permutations( one2n(N, []) ),
    permutation_loop( Temp_perms, M ),
    N * (N + 1) * factorial(N).

main () ->
	perm9_benchmark (5, 9),
	io:fwrite("Done\n").


%
%
