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

run_jfp() ->
    erlang:system_flag(schedulers_online, 1),
    NImages = 20,
    NTimes = 1,
    TSeq = sk_profile:benchmark(fun image_merge:merge/1, [NImages], NTimes),
    ?print(TSeq),
    TPar = sk_profile:benchmark(fun image_merge:mergeFarm/1, [NImages], NTimes),
    ?print(TPar).
    

%% run_memory_collection_examples() ->
%%     run_sequential_memory_example(),
%%     run_all_manual_memory_examples(),
%%     run_all_skel_memory_examples(),
%%     run_all_list_memory_examples().
%%     %% io:format("Sequential Version~n"),
%%     %% ?print(instrument:memory_status(total)),
%%     %% _ = sk_profile:benchmark(fun image_merge:merge/1, [?NImages], 1),
%%     %% io:format("Post Sequential~n"),
%%     %% ?print(instrument:memory_status(total)),
%%     %% io:format("Manually Parallelised~n"),
%%     %% run_manual_memory_examples

%% run_sequential_memory_example() ->
%%     io:format("Sequential Version~n"),
%%     ?print(instrument:memory_status(total)),
%%     _ = image_merge:merge(?NImages),
%%     io:format("done~n"),
%%     ?print(instrument:memory_status(total)).

%% run_all_manual_memory_examples() ->
%%     io:format("Manual Memory Examples~n"),
%%     [run_manual_memory_examples(X) || X <- [24, 20, 16, 12, 8, 4, 2, 1]],
%%     done.

%% run_manual_memory_examples(X) ->
%%     erlang:system_flag(schedulers_online, Cores),
%%     io:format("Running Examples on ~p cores.~n", [Cores]),
%%     _Time = time(fun image_merge:manualMerge/1).

%% run_all_skel_memory_examples() ->
%%     io:format("Skel Binary Memory Examples~n"),
%%     [run_skel_memory_examples(X) || X <- [24, 20, 16, 12, 8, 4, 2, 1]],
%%     done.

%% run_skel_memory_examples(X) ->
%%     erlang:system_flag(schedulers_online, Cores),
%%     io:format("Running Examples on ~p cores.~n", [Cores]),
%%     F = fun(Fun) ->
%% 		?print(Fun),
%% 		_Time = time(Fun, mem)
%% 	end,
%%     lists:map(F, [fun image_merge:mergeFarm/1,
%% 		  fun image_merge:mergePipeFarm/1,
%% 		  fun image_merge:mergeFarmPipe/1]).

%% run_all_list_memory_examples() ->
%%     io:format("Skel List Memory Examples~n"),
%%     [run_list_memory_examples(X) || X <- [24, 20, 16, 12, 8, 4, 2, 1]],
%%     done.

%% run_list_memory_examples(X) ->
%%     erlang:system_flag(schedulers_online, Cores),
%%     io:format("Running Examples on ~p cores.~n", [Cores]),
    

%%------------------------------------------------------------------------------
%% Interface Functions

-spec run() -> done.

run() ->
    run_jfp().
    %% run_all_examples(),
    %% run_manual_examples().
    %% run_memory_collection_examples().
