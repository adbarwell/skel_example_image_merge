-module(list_merge).

-include("../../erlang/erl_img/include/erl_img.hrl").

-compile([export_all]).

-define(NW, 8).

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
%% Worker Utility Functions

-spec removeAlpha(list(), atom()) -> list().

removeAlpha([], _) -> 
    [];
removeAlpha([R,G,B,_ | T], r8g8b8a8) ->
    [R, G, B | removeAlpha(T, r8g8b8a8)];
removeAlpha(Xs, _) ->
    Xs.

-spec convertToWhite(list()) -> list().
convertToWhite([]) -> 
    [];
convertToWhite([R,G,B | T]) when R < 20, G < 20, B < 20 ->
    [ 255,255,255 | convertToWhite(T) ];
convertToWhite([R,G,B | T]) -> 
    [R,G,B | convertToWhite(T)].

-spec mergeTwo(list(), list()) -> list().

mergeTwo([], _) -> 
    [];
mergeTwo(_, []) -> 
    [];
mergeTwo([255,255,255 | T], [R2, G2, B2 | T2]) ->
    [R2,G2,B2 | mergeTwo(T, T2) ];
mergeTwo([R,G,B | T], [_, _, _ | T2]) ->
    [R,G,B | mergeTwo(T,T2)].

%%------------------------------------------------------------------------------
%% Worker Interface Functions

-spec imageList(non_neg_integer()) -> [{string(), string(), string()}].

imageList(0) ->
    [];
imageList(N) ->
    [{"./images/helmetScaled.png", 
      "./images/joeScaled.png", 
      "./images/merged" ++ integer_to_list(N) ++ ".png"} | imageList(N-1)].

-spec readImage({string(), string(), string()}) -> {[list()], [list()], 
						    atom(), atom(), string()}.

readImage({FileName, FileName2, Output}) -> 
    {ok, _Img=#erl_image{format=F1, pixmaps=[PM]}} = erl_img:load(FileName),
    #erl_pixmap{pixels=Rows} =PM,
    R = lists:map(fun({_A,B}) -> binary_to_list(B) end, Rows),

    {ok, _Img2=#erl_image{format=F2, pixmaps=[PM2]}} = erl_img:load(FileName2),
    
    #erl_pixmap{pixels=Rows2} =PM2,
    R2 = lists:map(fun({_A2,B2}) -> binary_to_list(B2) end, Rows2),
      
    {R, R2, F1, F2, Output}.

-spec convertMerge({[list()], [list()], atom(), 
		    atom(), string()}) -> {[list()], integer(), string()}.

convertMerge({R, R2, F1, F2, Name}) ->
    %% ?print(memsup:get_system_memory_data()),
    %% ?print(erlang:i()),
    R1_p = lists:map(fun(L) -> removeAlpha(L, F1) end, R),
    R2_p = lists:map(fun(L2) -> removeAlpha(L2, F2) end, R2),

    WhiteR =  lists:map(fun(Col) -> convertToWhite(Col) end, R1_p),
  
    Result = lists:zipwith(fun(L1,L2) -> mergeTwo(L1, L2) end, WhiteR, R2_p),

    {Result, length(R), Name}.

%%------------------------------------------------------------------------------
%% Interface Functions

-spec merge(non_neg_integer()) -> [{[list()], integer(), string()}].

merge(X) ->
    [convertMerge(readImage(Y)) || Y <- imageList(X)].

-spec mergeFarm(non_neg_integer()) -> [{[list()], integer(), string()}].

mergeFarm(X) ->
    skel:do([{farm, [{seq, fun (Y) -> convertMerge(readImage(Y)) end}],
               ?NW}],imageList(X)).

-spec mergeFarmPipe(non_neg_integer()) -> [{[list()], integer(), string()}].

mergeFarmPipe(X) ->
    skel:do([{farm, [{pipe, [{seq, fun ?MODULE:readImage/1}, 
			     {seq, fun ?MODULE:convertMerge/1}]}], ?NW}],
	    imageList(X)).

-spec mergePipeFarm(non_neg_integer()) -> [{[list()], integer(), string()}].

mergePipeFarm(X) ->
    skel:do([{pipe, [{farm, [{seq, fun ?MODULE:readImage/1}], ?NW}, 
		     {farm, [{seq, fun ?MODULE:convertMerge/1}], ?NW}]}],
	    imageList(X)).

start() ->
    application:load(sasl),
    application:start(sasl),
    application:start(os_mon).
