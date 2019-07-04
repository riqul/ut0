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
  
  is_s/1,
%%  is_s_byte/1,
  
  is_su/1,
%%  is_s_unicode/1,
  
  is_s_digits/1,
  is_s_digits/2,
  is_b_digits/1,
  is_b_digits/2,
  
  b/1,
  b/2,
  
  ok2/1,
  
  l_skip/2,
  l_skip_safe/3,
  l_skip_errv/3,
  
  l_push_rep/3,
  
  l_key_val2p_rev/3,
  l_key_val2p/3,

%%  l_key_val_stopmap2p_rev/4,
  l_key_val_stopmap2p_rev/5,
  l_stopmap_rev/4,
  
  jsx_is_obj/1,
  jsx_obj2p/1,
  jsx_p2obj/1,
  
  is_re/2,
  is_re_b/2,
%%  is_re_bin/2,
  
  s2s_hex_rev/2,
  s2s_hex/1,
  s2s_hex/2,
  b2b_hex/1
]).


%% proplists %%%%%%%%%%%%%%%%%%%%%%%

-spec pget(any(), list()) -> any().
pget(Key, List) ->
  case lists:keyfind(Key, 1, List) of
    {_, Val} -> Val;
    ?f -> ?u
%%    ;_ -> undefined % found tuple with size =/= 2
  end.

-spec pget(any(), list(), any()) -> any().
pget(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    {_, Val} -> Val;
    ?f -> Default
%%    ;_ -> Default % found tuple with size =/= 2
  end.

-spec pput(any(), any(), list()) -> list().
pput(Key, Val, List) ->
  lists:keystore(Key, 1, List, {Key, Val}).


%% map %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec mget(any(), map()) -> any().
mget(Key, Map) ->
  maps:get(Key, Map, ?u).

-spec mget(any(), map(), any()) -> any().
% duplicate maps:get/3 - if you want to use ut:m...
mget(Key, Map, Default) ->
  maps:get(Key, Map, Default).

-spec mput(any(), map(), any()) -> any().
% duplicate maps:put/3 - if you want to use ut:m...
mput(Key, Val, Map) ->
  maps:put(Key, Val, Map).


%% is_... macro %%%%%%%%%%%%%%%%%%%%

%% is_... no macro %%%%%%%%%%%%%%%%%

is_s([X | Tail]) ->
  ?IF(?is_byte(X),
    is_s(Tail),
    ?f
  );
is_s("") ->
  ?t;
is_s(_) ->
  ?f.


%%is_s_byte(Arg) -> %todo: deprecated
%%  is_s(Arg).



is_su([X | Tail]) ->
  ?IF(?is_i_non_neg(X),
    is_su(Tail),
    ?f
  );
is_su("") ->
  ?t;
is_su(_) ->
  ?f.

%%is_s_unicode(Arg) -> %todo: deprecated
%%  is_su(Arg).

is_s_digits([X | Tail]) ->
  ?IF(?is_i_digit(X),
    is_s_digits(Tail, ?t),
    ?f
  );
is_s_digits(_) ->
  ?f.

%%is_s_digits(List, IsEmptyNoError) when ?is_bool(IsEmptyNoError) ->
%%  is_s_digits__no_verify_(List, IsEmptyNoError).

is_s_digits([X | Tail], _IsEmptyNoError) ->
  ?IF(?is_i_digit(X),
    is_s_digits(Tail, ?t),
    ?f
  );
is_s_digits("", IsEmptyNoError) ->
  IsEmptyNoError =:= ?t;
is_s_digits(_, _IsEmptyNoError) ->
  ?f.


is_b_digits(X) when ?is_b(X) ->
  is_s_digits(?b2s(X));
is_b_digits(_) ->
  ?f.

is_b_digits(X, IsEmptyNoError) when ?is_b(X) ->
  is_s_digits(?b2s(X), IsEmptyNoError);
is_b_digits(_, _) ->
  ?f.


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
          case l_skip__no_verify_(N - 8, Tail) of
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
l_skip__no_verify_(N, List) ->
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
          l_skip__no_verify_(N - 8, Tail);
        _ ->
          badarg
      end
  end.


-spec l_skip_safe(non_neg_integer(), list(), any()) -> list() | any().
l_skip_safe(N, List, ErrVal) when ?is_i_non_neg(N) ->
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
          case l_skip__no_verify_(N - 8, Tail) of
            badarg ->
              ErrVal;
            Res ->
              Res
          end;
        _ ->
          ErrVal
      end
  end.


l_skip_errv(N, List, ErrVal) -> %todo: deprecated
  l_skip_safe(N, List, ErrVal).




l_push_rep(N, E, List)
  when ?is_i_non_neg(N), ?is_l(List) ->
  case N of
    0 ->
      List;
    1 ->
      [E | List];
    2 ->
      [E, E | List];
    3 ->
      [E, E, E | List];
    4 ->
      [E, E, E, E | List];
    5 ->
      [E, E, E, E, E | List];
    6 ->
      [E, E, E, E, E, E | List];
    7 ->
      [E, E, E, E, E, E, E | List];
    _ ->
      l_push_rep__no_verify_(N - 8, E, [E, E, E, E, E, E, E, E | List])
  end.

% require ?is_i_non_neg(N), ?is_l(List) ->
l_push_rep__no_verify_(N, E, List) ->
  case N of
    0 ->
      List;
    1 ->
      [E | List];
    2 ->
      [E, E | List];
    3 ->
      [E, E, E | List];
    4 ->
      [E, E, E, E | List];
    5 ->
      [E, E, E, E, E | List];
    6 ->
      [E, E, E, E, E, E | List];
    7 ->
      [E, E, E, E, E, E, E | List];
    _ ->
      l_push_rep__no_verify_(N - 8, E, [E, E, E, E, E, E, E, E | List])
  end.



l_key_val2p_rev([K | KTail], [V | VTail], Acc) ->
  l_key_val2p_rev(KTail, VTail, [{K, V} | Acc]);
l_key_val2p_rev([], [], Acc) ->
  Acc.

l_key_val2p([K | KTail], [V | VTail], Acc) ->
  l_key_val2p(KTail, VTail, [{K, V} | Acc]);
l_key_val2p([], [], Acc) ->
  ?l_rev(Acc).


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

l_key_val_stopmap2p_rev([K | KTail], [V | VTail], Acc, StopMapFun2, Arg2FunAcc) ->
  KV = {K, V},
  case StopMapFun2(KV, Arg2FunAcc) of
    {ok, KV2, Arg2FunAcc2} ->
      l_key_val_stopmap2p_rev(KTail, VTail, [KV2 | Acc], StopMapFun2, Arg2FunAcc2);
    {stop, _StopRes, _Arg2FunAcc2} = Stop ->
      Stop
  end;
l_key_val_stopmap2p_rev([], [], Acc, _StopMapFun, Arg2FunAcc) ->
  {ok, Acc, Arg2FunAcc}.

l_stopmap_rev([Elem | Tail], Acc, StopMapFun2, Arg2FunAcc) ->
  case StopMapFun2(Elem, Arg2FunAcc) of
    {ok, Elem2, Arg2FunAcc2} ->
      l_stopmap_rev(Tail, [Elem2 | Acc], StopMapFun2, Arg2FunAcc2);
    {stop, _StopRes, _Arg2FunAcc2} = Stop ->
      Stop
  end;
l_stopmap_rev([], Acc, _StopMapFun, FunAcc) ->
  {ok, Acc, FunAcc}.






jsx_is_obj(X) ->
  if
    ?is_m(X) ->
      ?t;
    ?is_l(X) ->
      case X of
        [{_, _} | _] ->
          ?t;
        [{}] ->
          ?t;
        _ ->
          ?f
      end;
    ?t ->
      ?f
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
      ?t;
    nomatch ->
      ?f
  end.

is_re_b(X, Regexp) when is_binary(X) ->
  case re:run(X, Regexp, [{capture, none}, unicode]) of
    match ->
      ?t;
    nomatch ->
      ?f
  end;
is_re_b(_X, _Regexp) ->
  ?f.


%%is_re_bin(X, Regexp) -> % deprecated
%%  is_re_b(X, Regexp).

%todo in hrl ?
-define(HEX_DIG(N), ?IF(N < 10, $0 + N, ($a - 10) + N)).
-define(BYTE_L(N), (N band 16#f)).
-define(BYTE_H(N), (N bsr 4)).


s2s_hex_rev([A1, A2, A3, A4 | Tail], AccRev) ->
  A1H = ?BYTE_H(A1),
  A1L = ?BYTE_L(A1),
  A2H = ?BYTE_H(A2),
  A2L = ?BYTE_L(A2),
  A3H = ?BYTE_H(A3),
  A3L = ?BYTE_L(A3),
  A4H = ?BYTE_H(A4),
  A4L = ?BYTE_L(A4),
  s2s_hex_rev(Tail, [
    ?HEX_DIG(A4L),
    ?HEX_DIG(A4H),
    ?HEX_DIG(A3L),
    ?HEX_DIG(A3H),
    ?HEX_DIG(A2L),
    ?HEX_DIG(A2H),
    ?HEX_DIG(A1L),
    ?HEX_DIG(A1H)
    | AccRev
  ]);
s2s_hex_rev([A1 | Tail], AccRev) ->
  A1H = ?BYTE_H(A1),
  A1L = ?BYTE_L(A1),
  s2s_hex_rev(Tail, [
    ?HEX_DIG(A1L),
    ?HEX_DIG(A1H)
    | AccRev
  ]);
s2s_hex_rev("", AccRev) ->
  AccRev.

s2s_hex(Str) ->
  s2s_hex(Str, "").

s2s_hex(Str, AccRev) ->
  ?l_rev(s2s_hex_rev(Str, AccRev)).

b2b_hex(Bin) ->
  ?s2b(?l_rev(
    s2s_hex_rev(?b2s(Bin), "")
  )).

