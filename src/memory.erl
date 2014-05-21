-module(memory).

-export([run/0]).

%%------------------------------------------------------------------------------
%% Debugging 

-ifndef(debug).
-define(debug, true).
%% -define(debug, false).
-endif.

-ifndef(print).
-define(print(Var), case ?debug of
			true ->
			    io:format("~p:~p~n  ~p: ~p~n", 
				      [?MODULE, ?LINE, ??Var, Var]);
			false ->
			    ok
		    end).
-endif.

%%------------------------------------------------------------------------------
%% Macros

-define(NTimes, 3).
-define(NImages, 100).

%%------------------------------------------------------------------------------
%% Utility functions  

diff(X, Y) ->
    X - Y.

benchmark(Fun) ->
    benchmark(Fun, ?NImages, ?NTimes).

benchmark(Fun, Arg) ->
    benchmark(Fun, Arg, ?NTimes).

benchmark(Fun, Arg, Times) ->
    benchmark_1(Fun, Arg, Times, []).
    
benchmark_1(Fun, Arg, 1, Acc) ->
    LastAcc = [benchmark_1(Fun, Arg) | Acc],
    lists:sum(LastAcc) / length(LastAcc);
benchmark_1(Fun, Arg, Times, Acc) -> 
    X = benchmark_1(Fun, Arg),
    erlang:garbage_collect(),
    benchmark_1(Fun, Arg, Times-1, [X | Acc]).
    
benchmark_1(Fun, Arg) ->
    _MemBefore = instrument:memory_status(total),
    _ = Fun(Arg),
    MemAfter = instrument:memory_status(total),
    %% ?print(MemBefore),
    ?print(MemAfter),
    %% MaxMemThrough = diff(extractMaxSinceLast(MemAfter), 
    %% 			 extractCurrent(MemBefore)),
    %% ?print(MaxMemThrough),
    %% MaxMemThrough.
    extractMaxSinceLast(MemAfter).

extractMaxSinceLast([{total, [{sizes, _, Y, _} | _]}]) ->
    Y.

extractCurrent([{total, [{sizes, X, _, _} | _]}]) ->
    X.

helper({Fun, Arg}) ->
    ?print(Fun),
    {Fun, Arg, benchmark(Fun, Arg)};
helper({Fun}) ->
    ?print(Fun),
    {Fun, benchmark(Fun)}.

%%------------------------------------------------------------------------------
%% Worker Interface Functions

runSequential() ->
    io:format("Sequential Version~n"),
    benchmark(fun image_merge:merge/1).
    
runManual() ->
    io:format("Manually Parallelised~n"),
    benchmark(fun image_merge:manualMerge/1).

runLists() ->
    io:format("Skel (Lists)~n"),
    lists:map(fun (X) -> helper(X) end, 
	      [{fun list_merge:mergeFarm/1, ?NImages},
	       {fun list_merge:mergeFarmPipe/1, 40}, % Max 40
	       {fun list_merge:mergePipeFarm/1, 20}]). % Max 20

runBinaries() ->
    io:format("Skel (Binaries)~n"),
    lists:map(fun (X) -> helper(X) end, 
	      [{fun list_merge:mergeFarm/1},
	       {fun list_merge:mergeFarmPipe/1},
	       {fun list_merge:mergePipeFarm/1}]).


run_all() ->
    Man = runManual(),
    ?print(Man),
    Lst = runLists(),
    ?print(Lst),
    Bin = runBinaries(),
    ?print(Bin),
    Seq = runSequential(),
    ?print(Seq).

%%------------------------------------------------------------------------------
%% Interface functions 

run() ->
    run_all().
