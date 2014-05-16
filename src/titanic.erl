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

-define(NTimes, 3).
-define(NImages, 2). 

%%------------------------------------------------------------------------------
%% Internal Utility Functions

time(Fun) ->
    time(Fun, [?NImages]).

time(Fun, Args) ->
    [_, _, _, _, {mean, T}, _] = sk_profile:benchmark(Fun, Args, ?NTimes),
    T.

speedup(TSeq) ->
    fun(X) ->
	    TSeq / X
    end.

%%------------------------------------------------------------------------------
%% Internal Interface Functions 

-spec run_examples(non_neg_integer()) -> ok.

run_examples(Cores) ->
    erlang:system_flag(schedulers_online, Cores),
    io:format("Running Examples on ~p cores.~n", [Cores]),
    
    TSeq = time(fun image_merge:merge/1),
    ?print(TSeq),
    Speedup = speedup(TSeq),
    
    TMPF = time(fun image_merge:mergePipeFarm/1),
    ?print(TMPF),
    SMPF = Speedup(TMPF),
    ?print(SMPF).

%%------------------------------------------------------------------------------
%% Interface Functions

-spec run() -> ok.

run() ->
    run_examples(4).
