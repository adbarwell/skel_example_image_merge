-module(image_merge).

-include_lib("../../../erlang/erl_img/include/erl_img.hrl").

-export([readImage/1,
	 convertMerge/1,
	 decomp/1,
	 recomp/1,
	 collectAllImages/3,
	 convertAllImages/2,
	 readAllImages/2,
	 readImageWrapper/2,
	 convertMergeWrapper/2,
	 merge/1,
	 manualMerge/1,
	 mergePipe/1,
	 mergePipeFarm/1,
	 mergePipeMap/1,
	 mergeFarm/1,
	 mergeFarmPipe/1,
	 mergeMap/1,
	 mergeMapPipe/1,
	 mergePipeCluster/1
	]).

-export([tfpDecomp/1, 
	 tfpRecomp/1, 
	 tfpFarmPipe/2, 
	 tfpPipeFarm/2, 
	 tfpPipeClusterFarm/2]).

%%------------------------------------------------------------------------------
%% Macros

-define(NW, 24).
 
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

decomp({[], _, _, _, _}) -> 
  [];
decomp({_, [], _, _, _}) ->
  [];
decomp({R1, R2, F1, F2, Name}) ->
  [{[hd(R1)], [hd(R2)], F1, F2, Name}] ++ decomp({tl(R1), tl(R2), F1, F2, Name}).

recomp(Parts) -> 
  Img = lists:map(fun({[A], _B, _C}) -> A end, Parts),
  Len = lists:foldl(fun({_, X, _}, Sum) -> X + Sum end, 0, Parts),
  {_, _, Name} = hd(Parts),

  {Img, Len, Name}.

readImageWrapper(Image, NextPid) ->
  NextPid ! {image, readImage(Image)}.

convertMergeWrapper(Image, NextPid) ->
  NextPid ! {merged, convertMerge(Image)}.

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

readAllImages([], _NextPid) ->
  ok;
readAllImages([Image | Images], NextPid) ->
  spawn(image_merge, readImageWrapper, [Image, NextPid]),
  readAllImages(Images, NextPid).

convertAllImages(0, _NextPid) ->
  ok;
convertAllImages(N, NextPid) ->
  receive
    {image, Image} ->
     spawn(image_merge, convertMergeWrapper, [Image, NextPid]),
     convertAllImages(N-1, NextPid)
end.

collectAllImages(0, NextPid, MergedImages) ->
  NextPid ! {done, MergedImages};
collectAllImages(N, NextPid, Acc) ->
  receive
    {merged, Image} ->
     collectAllImages(N-1, NextPid, [Image | Acc])
end.

%%------------------------------------------------------------------------------
%% Interface Functions 

-spec merge(non_neg_integer()) -> [{[binary()], integer(), string()}].

merge(X) ->
    [convertMerge(readImage(Y)) || Y <- imageList(X)].

manualParMerge(Images) ->
  CPid = spawn(image_merge, collectAllImages,
                         [length(Images), self(), []]),
  MPid = spawn(image_merge, convertAllImages,
                          [length(Images), CPid]),
  readAllImages(Images, MPid),
  receive
    {done, MergedImages} ->
     MergedImages
  end.

manualMerge(X) ->
    manualParMerge(imageList(X)).

mergeFarm(X) ->
    skel:do([{farm, [{seq, fun (Y) -> convertMerge(readImage(Y)) end}],
               ?NW}],imageList(X)).

-spec mergeFarmPipe(non_neg_integer()) -> [{[binary()], integer(), string()}].

mergeFarmPipe(X) ->
    skel:do([{farm, [{pipe, [{seq, fun ?MODULE:readImage/1}, 
			     {seq, fun ?MODULE:convertMerge/1}]}], ?NW}],
	     imageList(X)).

-spec mergePipe(non_neg_integer()) -> [{[binary()], integer(), string()}].

mergePipe(X) ->
    skel:do([{pipe, [{seq, fun ?MODULE:readImage/1}, 
		     {seq, fun ?MODULE:convertMerge/1}]}], imageList(X)).

-spec mergePipeFarm(non_neg_integer()) -> [{[binary()], integer(), string()}].

mergePipeFarm(X) ->
    skel:do([{pipe, [{farm, [{seq,fun ?MODULE:readImage/1}], ?NW},
		     {farm, [{seq, fun ?MODULE:convertMerge/1}], ?NW}]}], 
	    imageList(X)).

-spec mergePipeMap(non_neg_integer()) -> [{[binary()], integer(), string()}].

mergePipeMap(X) ->
    skel:do([{pipe, [{map, [{seq,fun ?MODULE:readImage/1}]},
		     {map, [{seq, fun ?MODULE:convertMerge/1}]}]}], 
	    [imageList(X)]).

-spec mergeMap(non_neg_integer()) -> [{[binary()], integer(), string()}].

mergeMap(X) ->
    skel:do([{map, [{seq, fun(Y) -> convertMerge(readImage(Y)) end}]}], 
	    [imageList(X)]).

-spec mergeMapPipe(non_neg_integer()) -> [{[binary()], integer(), string()}].

mergeMapPipe(X) ->
    skel:do([{map, [{pipe, [{seq, fun ?MODULE:readImage/1}, 
			    {seq, fun ?MODULE:convertMerge/1}]}]}],
	    [imageList(X)]).

mergePipeCluster(X) ->
    skel:do([{pipe, [{farm, [{seq, fun ?MODULE:readImage/1}], ?NW},
		     {cluster, [{seq, fun ?MODULE:convertMerge/1}], 
		      fun ?MODULE:decomp/1, fun ?MODULE:recomp/1}]}],
	    imageList(X)).


%%------------------------------------------------------------------------------
%% TFP Interfaces

tfpDecomp({[], _, _, _, _}) -> 
    [];
tfpDecomp({_, [], _, _, _}) ->
    [];
tfpDecomp({R1, R2, F1, F2, Name}) ->
    [{[hd(R1)], [hd(R2)], F1, F2, Name}] ++ 
                        tfpDecomp({tl(R1), tl(R2), F1, F2, Name}).

tfpRecomp(Parts) -> 
    Img = lists:map(fun({[A], _B, _C}) -> A end, Parts),
    {_, _, Name} = hd(Parts),
    {Img, length(Img), Name}.

tfpFarmPipe(NW, NI) -> 
    skel:do([{farm, [{seq, fun ?MODULE:readImage/1},
		     {seq, fun ?MODULE:convertMerge/1}], NW}],
	    imageList(NI)).

tfpPipeFarm(NW, NI) ->
    skel:do([{farm, [{seq, fun ?MODULE:readImage/1}], NW},
	     {farm, [{seq, fun ?MODULE:convertMerge/1}], NW}],
	    imageList(NI)).

tfpPipeClusterFarm(NW, NI) ->
    skel:do([{farm, [{seq, fun ?MODULE:readImage/1}], NW},
	     {cluster, 
	      [{farm, [{seq, fun ?MODULE:convertMerge/1}], NW}], 
	      fun ?MODULE:tfpDecomp/1,
	      fun ?MODULE:tfpRecomp/1}],
	    imageList(NI)).
