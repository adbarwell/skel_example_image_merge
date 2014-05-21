-module(titanic).

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

-define(NTimes, 10).
-define(NImages, 100). 

%%------------------------------------------------------------------------------
%% Internal Utility Functions

time(Fun) ->
    time(Fun, [?NImages]).

time(Fun, once) ->
    time(Fun, [?NImages], once);
time(Fun, Args) ->
    [_, _, _, _, {mean, T}, _] = sk_profile:benchmark(Fun, Args, ?NTimes),
    T.

time(Fun, Args, once) ->
    [_, _, _, _, {mean, T}, _] = sk_profile:benchmark(Fun, Args, 1),
    T.

%% speedup(TSeq) ->
%%     fun(X) ->
%% 	    TSeq / X
%%     end.

%%------------------------------------------------------------------------------
%% Internal Interface Functions 

-spec run_all_examples() -> done.

run_all_examples() ->
    [run_examples(X) || X <- [24, 20, 16, 12, 8, 4, 2, 1]],

    Fun = fun image_merge:mergePipe/1,
    ?print(Fun),
    TMP = time(Fun, once),
    ?print(TMP),
    done.

-spec run_examples(non_neg_integer()) -> done.

run_examples(Cores) ->
    erlang:system_flag(schedulers_online, Cores),
    io:format("Running Examples on ~p cores.~n", [Cores]),
    
    %% TSeq = time(fun image_merge:merge/1),
    %% ?print(TSeq),
    %% Speedup = speedup(TSeq),

    F = fun(Fun) ->
		?print(Fun),
		TMP = time(Fun),
		?print(TMP)
		%% SMP = Speedup(TMP),
		%% ?print(SMP)
	end,

    lists:map(F, [%% fun image_merge:mergePipe/1,
		  fun image_merge:mergePipeFarm/1,
		  fun image_merge:mergeFarm/1,
		  fun image_merge:mergeFarmPipe/1
		  %% fun image_merge:mergeMap/1,
		  %% fun image_merge:mergePipeMap/1,
		  %% fun image_merge:mergeMapPipe/1,
		  %% fun image_merge:mergePipeCluster/1
		 ]),
    done.
    
run_manual_examples() ->
    io:format("Manually parallelised version.~n"),
    [run_manual_example(X) || X <- [24, 20, 16, 12, 8, 4, 2, 1]],
    done.

run_manual_example(Cores) ->
    erlang:system_flag(schedulers_online, Cores),
    io:format("Running Examples on ~p cores.~n", [Cores]),
    Time = time(fun image_merge:manualMerge/1),
    ?print(Time).

%%------------------------------------------------------------------------------
%% Interface Functions

-spec run() -> done.

run() ->
    run_all_examples(),
    run_manual_examples().
