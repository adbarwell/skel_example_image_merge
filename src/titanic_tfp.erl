-module(titanic_tfp).

-export([run/2]).

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
%% Timing Functions

time(Fun, NWorkers, NTimes, NImages) ->
    sk_profile:benchmark(Fun, [NWorkers, NImages], NTimes).

%%------------------------------------------------------------------------------
%% Internal Interface Functions  

run_merge_examples(NWorkers, NTimes, NImages) ->
    erlang:system_flag(schedulers_online, NWorkers),
    NarrowedTime = fun(Fun) ->
			   time(Fun, NWorkers, NTimes, NImages)
		   end,
    {
      {irb, sk_profile:benchmark(fun image_merge:merge/1, [NImages], NTimes)},
      {ire, sk_profile:benchmark(fun ets_merge:merge/1, [NImages], NTimes)},
      {irbfp, NarrowedTime(fun image_merge:tfpFarmPipe/2)},
      {irefp, NarrowedTime(fun ets_merge:tfpFarmPipe/2)},
      {irbpf, NarrowedTime(fun image_merge:tfpPipeFarm/2)},
      {irepf, NarrowedTime(fun ets_merge:tfpPipeFarm/2)},
      {irbpcf, NarrowedTime(fun image_merge:tfpPipeClusterFarm/2)},
      {irepcf, NarrowedTime(fun ets_merge:tfpPipeClusterFarm/2)}
    }.

run_all_merge_examples(NTimes, NImages) ->
    [run_merge_examples(NWorkers, NTimes, NImages) ||
       NWorkers <- [24, 20, 16, 12, 8, 4, 2, 1]].

%%------------------------------------------------------------------------------
%% Interface Functions

-spec run(integer(), integer()) -> done.

run(NTimes, NImages) when NTimes > 0, NImages > 0 ->
    ?print(NTimes),
    ?print(NImages),
    ?print(run_merge_examples(4, NTimes, NImages)),
    io:format("~n"),
    done.
