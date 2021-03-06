-module(ets_merge).

-include_lib("../../../erlang/erl_img/include/erl_img.hrl").

-export([run/1,readImage/1,convertMerge/1,restore/0,create/0]).

-export([merge/1,mergeFarmPipe/1,mergeFarmPipePrime/1]).

-export([tfpDecomp/1, 
	 tfpRecomp/1, 
	 tfpFarmPipe/2, 
	 tfpPipeFarm/2, 
	 tfpPipeClusterFarm/2]).

%%------------------------------------------------------------------------------
%% Macros

-define(tab, images). 
-define(nw, 4).

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

-spec removeAlpha(binary(), atom()) -> binary().

removeAlpha(<<>>, _) -> 
    <<>>;
removeAlpha(<<R,G,B,_, T/binary>>, r8g8b8a8) ->
    U = removeAlpha(T, r8g8b8a8),
    << R,G,B, U/binary>>;
removeAlpha([], _) -> 
    [];
removeAlpha([R,G,B,_ | T], r8g8b8a8) ->
    [R, G, B | removeAlpha(T, r8g8b8a8)];
removeAlpha(Xs, _) ->
    Xs.
-spec convertToWhite(binary()) -> binary().

convertToWhite(<<>>) -> 
    <<>>;
convertToWhite(<<R,G,B, T/binary>>) ->
    case ((R < 20) and (B < 20) and (B < 20)) of
	true -> 
	    U = convertToWhite(T),
	    << 255,255,255, U/binary >>;
	false -> 
	    U = convertToWhite(T),
	    << R,G,B, U/binary >>
    end;
convertToWhite([]) -> 
    [];
convertToWhite([R,G,B | T]) when R < 20, G < 20, B < 20 ->
    [ 255,255,255 | convertToWhite(T) ];
convertToWhite([R,G,B | T]) -> 
    [R,G,B | convertToWhite(T)].

-spec mergeTwo(binary(), binary()) -> binary().

mergeTwo(<<>>, _) -> 
    <<>>;
mergeTwo(_, <<>>) -> 
    <<>>;
mergeTwo(<<255,255,255, T/binary>>, <<R2, G2, B2, T2/binary>>) ->
    T3 = mergeTwo(T, T2),
    <<R2,G2,B2, T3/binary>>;
mergeTwo(<<R,G,B, T/binary>>, <<_, _, _, T2/binary>>) ->
    T3 = mergeTwo(T,T2),
    <<R,G,B, T3/binary>>;
mergeTwo([], _) -> 
    [];
mergeTwo(_, []) -> 
    [];
mergeTwo([255,255,255 | T], [R2, G2, B2 | T2]) ->
    [R2,G2,B2 | mergeTwo(T, T2) ];
mergeTwo([R,G,B | T], [_, _, _ | T2]) ->
    [R,G,B | mergeTwo(T,T2)].

%%------------------------------------------------------------------------------
%% Worker Functions

imageList(0) ->
    [];
imageList(N) ->
    [{"./images/helmetScaled.png", 
      "./images/joeScaled.png", 
      N} | imageList(N-1)].

readImage({FileName, FileName2, I}) ->
    {ok, _Img=#erl_image{format=F1, pixmaps=[PM]}} = erl_img:load(FileName),
    #erl_pixmap{pixels=Cols} =PM,
    R = lists:map(fun({_A,B}) -> B end, Cols),

    {ok, _Img2=#erl_image{format=F2, pixmaps=[PM2]}} = erl_img:load(FileName2),

    #erl_pixmap{pixels=Cols2} =PM2,
    R2 = lists:map(fun({_A2,B2}) -> B2 end, Cols2),

    ets:insert_new(?tab, {I, {R, R2, F1, F2}}),
    
    I.

convertMerge(I) ->
    [{I, {R, R2, F, F2}}] = ets:lookup(?tab, I),
    
    R1_p = lists:map(fun(L) -> removeAlpha(L, F) end, R),
    R2_p = lists:map(fun(L2) -> removeAlpha(L2, F2) end, R2),

    WhiteR =  lists:map(fun(Col) -> convertToWhite(Col) end, R1_p),
  
    Result = lists:zipwith(fun(L1,L2) -> mergeTwo(L1, L2) end, WhiteR, R2_p),

    ets:insert(?tab, {I, {Result}}),

    I.

%% !! This won't work (as an extra stage) -- will create the same ets for each input
create() ->
    ?tab = ets:new(?tab, [set, public, named_table, {write_concurrency, true}, 
			  {read_concurrency, true}]).

createPrime() ->
    ?tab = ets:new(?tab, [set, public, named_table]).

restore() ->
    TabLst = lists:map(fun({_, X}) -> X end, ets:tab2list(?tab)),
    ets:delete(?tab),
    TabLst.

%%------------------------------------------------------------------------------
%% Interface Functions

merge(X) ->
    create(),
    [convertMerge(readImage(Y)) || Y <- imageList(X)],
    restore().

mergeFarmPipe(X) ->
    create(),
    skel:do([{farm, 
	      [{seq, fun ?MODULE:readImage/1}, 
	       {seq, fun ?MODULE:convertMerge/1}], ?nw}], 
	    imageList(X)),
    restore().

mergeFarmPipePrime(X) ->
    createPrime(),
    skel:do([{farm, 
	      [{seq, fun ?MODULE:readImage/1}, 
	       {seq, fun ?MODULE:convertMerge/1}], ?nw}], 
	    imageList(X)),
    restore().

time(Fun, Arg) ->
    sk_profile:benchmark(Fun, [Arg], 3).

run(X) when is_integer(X) ->
    {time(fun ?MODULE:merge/1, X),
     time(fun ?MODULE:mergeFarmPipe/1, X),
     time(fun ?MODULE:mergeFarmPipePrime/1, X)};
    %% R1 = merge(X),
    %% R2 = mergeFarmPipe(X),
    %% R1 =:= R2;
run([X]) ->
    run(list_to_integer(atom_to_list(X))).

%% The table should probably require an extra stage in the pipeline which adds 
%% a 'delete_all_objects' and a ets:tab2lst/1 to pass them out. 
%% Also, add in stage for creating the table -- make sure to delete it after?


%%------------------------------------------------------------------------------
%% TFP Interfaces

tfpDecomp({[], _, _, _}) -> 
    [];
tfpDecomp({_, [], _, _}) ->
    [];
tfpDecomp({R1, R2, F1, F2}) ->
    [{[hd(R1)], [hd(R2)], F1, F2}] ++ 
	tfpDecomp({tl(R1), tl(R2), F1, F2});
tfpDecomp(I) when is_integer(I) ->
    [{I, {R, R2, F, F2}}] = ets:lookup(?tab, I),
    lists:map(fun({I2, Img}) ->
		      ets:insert_new(?tab, {I2, Img}),
		      I2
	      end, 
	      lists:foldl(fun(Img, Acc) ->
				  case Acc of
				      [] ->
					  [{I + 1/(length(R)+1), Img}];
				      _ ->
					  {LI, _} = lists:last(Acc),
					  Acc ++ [{LI + 1/(length(R)+1), Img}]
				  end
			  end,
			  [],
			  tfpDecomp({R, R2, F, F2}))).
	      
tfpRecomp(Indices) -> 
    Parts = lists:map(fun(I) ->
			      [{I, Res}] = ets:lookup(?tab, I),
			      ets:delete(?tab, I),
			      {I, Res}
		      end, Indices),
    Img = lists:map(fun({_I, {[A]}}) -> A end, Parts),
    {I, _} = hd(Parts),
    ets:insert(?tab, {trunc(I), {Img}}).

tfpFarmPipe(NW, NI) -> 
    create(),
    skel:do([{farm, [{seq, fun ?MODULE:readImage/1},
		     {seq, fun ?MODULE:convertMerge/1}], NW}],
	    imageList(NI)),
    restore().

tfpPipeFarm(NW, NI) ->
    create(),
    skel:do([{farm, [{seq, fun ?MODULE:readImage/1}], NW},
	     {farm, [{seq, fun ?MODULE:convertMerge/1}], NW}],
	    imageList(NI)),
    restore().

tfpPipeClusterFarm(NW, NI) ->
    create(),
    skel:do([{farm, [{seq, fun ?MODULE:readImage/1}], NW},
	     {cluster, 
	      [{farm, [{seq, fun ?MODULE:convertMerge/1}], NW}], 
	      fun ?MODULE:tfpDecomp/1,
	      fun ?MODULE:tfpRecomp/1}],
	    imageList(NI)),
    restore().
