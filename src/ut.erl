-module(ut).
-include("ut.hrl").

%% API
-export([
  pget/2,
  pget/3,
  pput/3,
  
  mget/2,
  mget/3,
  mput/3,
  
  is_s_byte/1,
  is_s_unicode/1,
  
  b/1,
  b/2,
  
  ok2/1,
  
  l_skip/2,
  l_skip_errv/3,
  
  l_key_val2p_rev/3,
  l_key_val2p/3,

%%  l_key_val_stopmap2p_rev/4,
  l_key_val_stopmap2p_rev/5,
  l_stopmap_rev/4,
  
  jsx_is_obj/1,
  jsx_obj2p/1,
  jsx_p2obj/1,

  is_re/2,
  is_re_bin/2]).


%% proplists %%%%%%%%%%%%%%%%%%%%%%%

-spec pget(any(), list()) -> any().
pget(Key, List) ->
  case lists:keyfind(Key, 1, List) of
    {_, Val} -> Val;
    false -> undefined
%%    ;_ -> undefined % found tuple with size =/= 2
  end.

-spec pget(any(), list(), any()) -> any().
pget(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    {_, Val} -> Val;
    false -> Default
%%    ;_ -> Default % found tuple with size =/= 2
  end.

-spec pput(any(), any(), list()) -> list().
pput(Key, Val, List) ->
  lists:keystore(Key, 1, List, {Key, Val}).


%% map %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec mget(any(), map()) -> any().
mget(Key, Map) ->
  maps:get(Key, Map, undefined).

-spec mget(any(), map(), any()) -> any().
% duplicate maps:get/3 - if you want to use ut:m...
mget(Key, Map, Default) ->
  maps:get(Key, Map, Default).

-spec mput(any(), map(), any()) -> any().
% duplicate maps:put/3 - if you want to use ut:m...
mput(Key, Val, Map) ->
  maps:put(Key, Val, Map).


%% is_... %%%%%%%%%%%%%%%%%%%%%%%%%%

is_s_byte([X | Tail]) ->
  ?IF(?is_byte(X),
    is_s_byte(Tail),
    false
  );
is_s_byte("") ->
  true;
is_s_byte(_) ->
  false.


%todo: rename?
is_s_unicode([X | Tail]) ->
  ?IF(?is_i_non_neg(X),
    is_s_unicode(Tail),
    false
  );
is_s_unicode("") ->
  true;
is_s_unicode(_) ->
  false.


%% convert %%%%%%%%%%%%%%%%%%%%%%%%%

b(Val) when ?is_b(Val) ->
  Val;
b(Val) ->
  ?err_badarg([Val]).

b(Val, _AdditionalErrorArgs) when ?is_b(Val) ->
  Val;
b(Val, AdditionalErrorArgs) ->
  ?err_badarg([Val | AdditionalErrorArgs]).


%% convert %%%%%%%%%%%%%%%%%%%%%%%%%

ok2({ok, Res}) ->
  Res.





-spec l_skip(non_neg_integer(), list()) -> list().
l_skip(N, List) when ?is_i_non_neg(N) ->
  case N of
    0 ->
      ?IF(?is_l(List),
        List,
        ?err_badarg([N, List])
      );
    1 ->
      case List of
        [_ | Tail] ->
          Tail;
        _ ->
          ?err_badarg([N, List])
      end;
    2 ->
      case List of
        [_, _ | Tail] ->
          Tail;
        _ ->
          ?err_badarg([N, List])
      end;
    3 ->
      case List of
        [_, _, _ | Tail] ->
          Tail;
        _ ->
          ?err_badarg([N, List])
      end;
    4 ->
      case List of
        [_, _, _, _ | Tail] ->
          Tail;
        _ ->
          ?err_badarg([N, List])
      end;
    5 ->
      case List of
        [_, _, _, _, _ | Tail] ->
          Tail;
        _ ->
          ?err_badarg([N, List])
      end;
    6 ->
      case List of
        [_, _, _, _, _, _ | Tail] ->
          Tail;
        _ ->
          ?err_badarg([N, List])
      end;
    7 ->
      case List of
        [_, _, _, _, _, _, _ | Tail] ->
          Tail;
        _ ->
          ?err_badarg([N, List])
      end;
    _ -> % 8 or more
      case List of
        [_, _, _, _, _, _, _, _ | Tail] ->
          case l_skip_(N - 8, Tail) of
            badarg ->
              ?err_badarg([N, List]);
            Res ->
              Res
          end;
        _ ->
          ?err_badarg([N, List])
      end
  end.

%require ?is_i_non_neg(N), ?is_l(List)
l_skip_(N, List) ->
  case N of
    0 ->
      List;
    1 ->
      case List of
        [_ | Tail] ->
          Tail;
        _ ->
          badarg
      end;
    2 ->
      case List of
        [_, _ | Tail] ->
          Tail;
        _ ->
          badarg
      end;
    3 ->
      case List of
        [_, _, _ | Tail] ->
          Tail;
        _ ->
          badarg
      end;
    4 ->
      case List of
        [_, _, _, _ | Tail] ->
          Tail;
        _ ->
          badarg
      end;
    5 ->
      case List of
        [_, _, _, _, _ | Tail] ->
          Tail;
        _ ->
          badarg
      end;
    6 ->
      case List of
        [_, _, _, _, _, _ | Tail] ->
          Tail;
        _ ->
          badarg
      end;
    7 ->
      case List of
        [_, _, _, _, _, _, _ | Tail] ->
          Tail;
        _ ->
          badarg
      end;
    _ -> % 8 or more
      case List of
        [_, _, _, _, _, _, _, _ | Tail] ->
          l_skip_(N - 8, Tail);
        _ ->
          badarg
      end
  end.


-spec l_skip_errv(non_neg_integer(), list(), any()) -> list() | any().
l_skip_errv(N, List, ErrVal) when ?is_i_non_neg(N) ->
  case N of
    0 ->
      ?IF(?is_l(List),
        List,
        ErrVal
      );
    1 ->
      case List of
        [_ | Tail] ->
          Tail;
        _ ->
          ErrVal
      end;
    2 ->
      case List of
        [_, _ | Tail] ->
          Tail;
        _ ->
          ErrVal
      end;
    3 ->
      case List of
        [_, _, _ | Tail] ->
          Tail;
        _ ->
          ErrVal
      end;
    4 ->
      case List of
        [_, _, _, _ | Tail] ->
          Tail;
        _ ->
          ErrVal
      end;
    5 ->
      case List of
        [_, _, _, _, _ | Tail] ->
          Tail;
        _ ->
          ErrVal
      end;
    6 ->
      case List of
        [_, _, _, _, _, _ | Tail] ->
          Tail;
        _ ->
          ErrVal
      end;
    7 ->
      case List of
        [_, _, _, _, _, _, _ | Tail] ->
          Tail;
        _ ->
          ErrVal
      end;
    _ -> % 8 or more
      case List of
        [_, _, _, _, _, _, _, _ | Tail] ->
          case l_skip_(N - 8, Tail) of
            badarg ->
              ErrVal;
            Res ->
              Res
          end;
        _ ->
          ErrVal
      end
  end.


l_key_val2p_rev([K | KTail], [V | VTail], Acc) ->
  l_key_val2p_rev(KTail, VTail, [{K, V} | Acc]);
l_key_val2p_rev([], [], Acc) ->
  Acc.

%%l_key_val_stopmap2p_rev([K | KTail], [V | VTail], Acc, StopMapFun) ->
%%  KV = {K, V},
%%  case StopMapFun(KV) of
%%    {ok, KV2} ->
%%      l_key_val_stopmap2p_rev(KTail, VTail, [KV2 | Acc], StopMapFun);
%%    {stop, StopRes} ->
%%      StopRes
%%  end;
%%l_key_val_stopmap2p_rev([], [], Acc, _StopMapFun) ->
%%  Acc.

l_key_val_stopmap2p_rev([K | KTail], [V | VTail], Acc, StopMapFun, FunAcc) ->
  KV = {K, V},
  case StopMapFun(KV, FunAcc) of
    {ok, KV2, FunAcc2} ->
      l_key_val_stopmap2p_rev(KTail, VTail, [KV2 | Acc], StopMapFun, FunAcc2);
    {stop, _StopRes, _FunAcc2} = Stop ->
      Stop
  end;
l_key_val_stopmap2p_rev([], [], Acc, _StopMapFun, FunAcc) ->
  {ok, Acc, FunAcc}.

l_stopmap_rev([Elem | Tail], Acc, StopMapFun, FunAcc) ->
  case StopMapFun(Elem, FunAcc) of
    {ok, Elem2, FunAcc2} ->
      l_stopmap_rev(Tail, [Elem2 | Acc], StopMapFun, FunAcc2);
    {stop, _StopRes, _FunAcc2} = Stop ->
      Stop
  end;
l_stopmap_rev([], Acc, _StopMapFun, FunAcc) ->
  {ok, Acc, FunAcc}.



l_key_val2p([K | KTail], [V | VTail], Acc) ->
  l_key_val2p_rev(KTail, VTail, [{K, V} | Acc]);
l_key_val2p([], [], Acc) ->
  ?l_rev(Acc).






jsx_is_obj(X) ->
  case X of
    [{}] ->
      true;
    [{_, _} | _] ->
      true;
    _ ->
      false
  end.

jsx_obj2p([{}]) ->
  [];
jsx_obj2p(Other) ->
  Other.

jsx_p2obj([]) ->
  [{}];
jsx_p2obj(Other) ->
  Other.


is_re(X, Regexp) ->
  case re:run(X, Regexp, [{capture, none}, unicode]) of
    match ->
      true;
    nomatch ->
      false
  end.

is_re_bin(X, Regexp) when is_binary(X) ->
  case re:run(X, Regexp, [{capture, none}, unicode]) of
    match ->
      true;
    nomatch ->
      false
  end;
is_re_bin(_X, _Regexp) ->
  false.


