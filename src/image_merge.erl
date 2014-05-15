-module(image_merge).

-include_lib("../../erlang/erl_img/include/erl_img.hrl").

-export([merge/1,mergeFarm/1,mergeFarmPipe/1,readImage/1,convertMerge/1]).

%%------------------------------------------------------------------------------
%% Worker Utility Functions

-spec removeAlpha(binary(), atom()) -> binary().

removeAlpha(<<>>, _) -> 
    <<>>;
removeAlpha(<<R,G,B,_, T/binary>>, r8g8b8a8) ->
    U = removeAlpha(T, r8g8b8a8),
    << R,G,B, U/binary>>.

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
    end.

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
    <<R,G,B, T3/binary>>.


%%------------------------------------------------------------------------------
%% Worker Interface Functions 

-spec imageList(non_neg_integer()) -> [{string(), string(), string()}].

imageList(0) ->
    [];
imageList(N) ->
    [{"./images/helmetScaled.png", 
      "./images/joeScaled.png", 
      "./images/merged" ++ integer_to_list(N) ++ ".png"} | imageList(N-1)].

-spec readImage({string(), string(), string()}) -> 
		       {[binary()], [binary()], atom(), atom(), string()}.

readImage({FileName, FileName2, Output}) -> 
  {ok, _Img=#erl_image{format=F1, pixmaps=[PM]}} = erl_img:load(FileName),
  #erl_pixmap{pixels=Cols} =PM,
  R = lists:map(fun({_A,B}) -> B end, Cols),

  {ok, _Img2=#erl_image{format=F2, pixmaps=[PM2]}} = erl_img:load(FileName2),

  #erl_pixmap{pixels=Cols2} =PM2,
  R2 = lists:map(fun({_A2,B2}) -> B2 end, Cols2),

  {R, R2, F1, F2, Output}.

-spec convertMerge({[binary()], [binary()], atom(), atom(), string()}) ->
			  {[binary()], integer(), string()}.
convertMerge({R, R2, F1, F2, Name}) ->
    R1_p = lists:map(fun(L) -> removeAlpha(L, F1) end, R),
    R2_p = lists:map(fun(L2) -> removeAlpha(L2, F2) end, R2),

    WhiteR =  lists:map(fun(Col) -> convertToWhite(Col) end, R1_p),
  
    Result = lists:zipwith(fun(L1,L2) -> mergeTwo(L1, L2) end, WhiteR, R2_p),

    {Result, length(R), Name}.

%%------------------------------------------------------------------------------
%% Interface Functions 

-spec merge(non_neg_integer()) -> [{[binary()], integer(), string()}].

merge(X) ->
    [convertMerge(readImage(Y)) || Y <- imageList(X)].

-spec mergeFarm(non_neg_integer()) -> [{[binary()], integer(), string()}].

mergeFarm(X) ->
    skel:do([{farm, [{seq, fun (Y) -> convertMerge(readImage(Y)) end}],
               8}],imageList(X)).

-spec mergeFarmPipe(non_neg_integer()) -> [{[binary()], integer(), string()}].

mergeFarmPipe(X) ->
    skel:do([{farm, [{pipe, [{seq, fun ?MODULE:readImage/1}, 
			     {seq, fun ?MODULE:convertMerge/1}]}], 8}],
	     imageList(X)).
