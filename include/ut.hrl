
% u - undefined
% n - null

% l - list
% s - string bytes (list of 0..255)
% su - string unicode (list of non_neg_integer)
% p - proplist
% b - binary
% a - atom
% t - tuple AND ?t=true
% r - record
% f - float AND ?f=false
% m - map
% bool - boolean
% num - number
% fun - function
% iol - iolist
% pid - pid
% ref - reference
% port - port
% bits - bitstring

% term - term

% re - regexp
% rev - reverse
% err - error
% exi - existing, exist
% pos - positive
% neg - negative
% dec - decimal

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% shortcuts for atoms

-define(u, undefined).
-define(n, null).
-define(t, true).
-define(f, false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% shortcuts for ?is_...

-define(is_u(X), ((X) == ?u)).
-define(is_n(X), ((X) == ?n)).
-define(is_l(X), is_list(X)).
-define(is_b(X), is_binary(X)).
-define(is_i(X), is_integer(X)).
-define(is_a(X), is_atom(X)).
-define(is_t(X), is_tuple(X)).
-define(is_r(X, Tag), is_record(X, Tag)).
-define(is_r(X, Tag, N), is_record(X, Tag, N)).
-define(is_f(X), is_float(X)).
-define(is_num(X), is_number(X)).
-define(is_m(X), is_map(X)).
-define(is_bool(X), is_boolean(X)).
-define(is_fun(X), is_function(X)).
-define(is_fun(X, N), is_function(X, N)).
-define(is_pid(X), is_pid(X)).
-define(is_ref(X), is_reference(X)).
-define(is_port(X), is_port(X)).
-define(is_bits(X), is_bitstring(X)).

-define(is_bool_t(X), (X =:= true)).
-define(is_bool_f(X), (X =:= false)).

-define(is_i(X, From, To), (
    ?is_i(X) andalso ((X) =< To) andalso ((X) >= From)
)).
-define(is_i_min(X, Min), (
    ?is_i(X) andalso ((X) >= Min)
)).
-define(is_i_max(X, Max), (
    ?is_i(X) andalso ((X) =< Max)
)).
-define(is_i_pos(X), (
    ?is_i(X) andalso ((X) > 0)
)).
-define(is_i_neg(X), (
    ?is_i(X) andalso ((X) < 0)
)).
-define(is_i_non_pos(X), ?is_i_max(X, 0)).
-define(is_i_non_neg(X), ?is_i_min(X, 0)).

-define(is_byte(X), ?is_i(X, 0, 255)).
-define(is_i_ascii(X), ?is_i(X, 0, 127)).

-define(is_i_digit(X), ?is_i(X, $0, $9)).
-define(is_i_eng_letter_small(X), ?is_i(X, $a, $z)).
-define(is_i_eng_letter_big(X), ?is_i(X, $A, $Z)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% shortcut for ?IF...

-define(IF(Condition, Then, Else),
  (case (Condition) of true -> (Then); false -> (Else) end)
).
-define(IF_u(X, Then, Else),
  (case (Condition) of ?u -> (Then); _ -> (Else) end)
).
-define(IF_n(X, Then, Else),
  (case (X) of ?n -> (Then); _ -> (Else) end)
).
-define(IF_l(X, Then, Else),
  ?IF(?is_l(X), Then, Else)
).
-define(IF_b(X, Then, Else),
  ?IF(?is_b(X), Then, Else)
).
-define(IF_t(X, Then, Else),
  ?IF(?is_t(X), Then, Else)
).
-define(IF_bool(X, Then, Else),
  ?IF(?is_bool(X), Then, Else)
).
-define(IF_i(X, Then, Else),
  ?IF(?is_i(X), Then, Else)
).
-define(IF_a(X, Then, Else),
  ?IF(?is_a(X), Then, Else)
).
-define(IF_num(X, Then, Else),
  ?IF(?is_num(X), Then, Else)
).
-define(IF_m(X, Then, Else),
  ?IF(?is_m(X), Then, Else)
).


%%-define(NOT_UNDEF(X, ElseExpr),
%%  ?IF(X =/= undefined, X, ElseExpr)
%%).
%%-define(NOT_NULL(X, ElseExpr),
%%  ?IF(X =/= null, X, ElseExpr)
%%).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(catch_safe(Expr, CatchExpr), (try Expr catch _:_ -> CatchExpr end)).
-define(catch_safe_fun3(Expr, CatchFun3, ExprArg3), (try Expr catch E:R -> (CatchFun3)(E, R, ExprArg3) end)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

-define(t2l(X), tuple_to_list(X)).
-define(l2t(X), list_to_tuple(X)).

-define(s2b(X), list_to_binary(X)).
-define(b2s(X), binary_to_list(X)).
%%-define(l2b(X), list_to_binary(X)). % deprecated: better use string macro
%%-define(b2l(X), binary_to_list(X)). % deprecated: better use string macro
-define(b2s(X, Start, Stop), binary_to_list(X, Start, Stop)).

-define(i2s(X), integer_to_list(X)).
-define(i2s(X, Base), integer_to_list(X, Base)).
-define(s2i(X), list_to_integer(X)).
-define(s2i(X, Base), list_to_integer(X, Base)).
%%-define(i2l(X), integer_to_list(X)).  % deprecated: better use string macro
%%-define(i2l(X, Base), integer_to_list(X, Base)).  % deprecated: better use string macro
%%-define(l2i(X), list_to_integer(X)).  % deprecated: better use string macro
%%-define(l2i(X, Base), list_to_integer(X, Base)).  % deprecated: better use string macro

-define(s2i_safe(X, ErrExpr), ?catch_safe(?s2i(X), ErrExpr)).
-define(s2i_safe(X, Base, ErrExpr), ?catch_safe(?s2i(X, Base), ErrExpr)).
-define(s2i_safe_err(X), ?s2i_safe(X, error)).
-define(s2i_safe_err(X, Base), ?s2i_safe(X, Base, error)).

-define(i2b(X), integer_to_binary(X)).
-define(b2i(X), binary_to_integer(X)).
-define(i2b(X, Base), integer_to_binary(X, Base)).
-define(b2i(X, Base), binary_to_integer(X, Base)).

-define(b2i_safe(X, ErrExpr), ?catch_safe(?b2i(X), ErrExpr)).
-define(b2i_safe(X, Base, ErrExpr), ?catch_safe(?b2i(X, Base), ErrExpr)).
-define(b2i_safe_err(X), ?b2i_safe(X, error)).
-define(b2i_safe_err(X, Base), ?b2i_safe(X, Base, error)).

-define(f2s(X), float_to_list(X)).
-define(f2s(X, Opts), float_to_list(X, Opts)).
-define(f2s_dec(X, Dec), ?f2s(X, [{decimals, Dec}])).
-define(f2s_dec_c(X, Dec), ?f2s(X, [{decimals, Dec}, compact])).

-define(f2s_dec0(X), ?f2s_dec(X, 0)).
-define(f2s_dec1(X), ?f2s_dec(X, 1)).
-define(f2s_dec2(X), ?f2s_dec(X, 2)).
-define(f2s_dec3(X), ?f2s_dec(X, 3)).
-define(f2s_dec4(X), ?f2s_dec(X, 4)).
-define(f2s_dec5(X), ?f2s_dec(X, 5)).
-define(f2s_dec6(X), ?f2s_dec(X, 6)).

-define(f2s_dec1c(X), ?f2s_dec_c(X, 1)).
-define(f2s_dec2c(X), ?f2s_dec_c(X, 2)).
-define(f2s_dec3c(X), ?f2s_dec_c(X, 3)).
-define(f2s_dec4c(X), ?f2s_dec_c(X, 4)).
-define(f2s_dec5c(X), ?f2s_dec_c(X, 5)).
-define(f2s_dec6c(X), ?f2s_dec_c(X, 6)).

-define(s2f(X), list_to_float(X)).
%%-define(f2l(X), float_to_list(X)). % deprecated: better use string macro
%%-define(l2f(X), list_to_float(X)). % deprecated: better use string macro

-define(s2f_safe(X, Err), ?catch_safe(?s2f(X), Err)).
-define(s2f_safe_err(X), ?s2f_safe(X, error)).

-define(f2b(X), float_to_binary(X)).
-define(f2b(X, Opts), float_to_binary(X, Opts)).
-define(f2b_dec(X, Dec), ?f2b(X, [{decimals, Dec}])).
-define(f2b_dec_c(X, Dec), ?f2b(X, [{decimals, Dec}, compact])).

-define(f2b_dec0(X), ?f2b_dec(X, 0)).
-define(f2b_dec1(X), ?f2b_dec(X, 1)).
-define(f2b_dec2(X), ?f2b_dec(X, 2)).
-define(f2b_dec3(X), ?f2b_dec(X, 3)).
-define(f2b_dec4(X), ?f2b_dec(X, 4)).
-define(f2b_dec5(X), ?f2b_dec(X, 5)).
-define(f2b_dec6(X), ?f2b_dec(X, 6)).

-define(f2b_dec1c(X), ?f2b_dec_c(X, 1)).
-define(f2b_dec2c(X), ?f2b_dec_c(X, 2)).
-define(f2b_dec3c(X), ?f2b_dec_c(X, 3)).
-define(f2b_dec4c(X), ?f2b_dec_c(X, 4)).
-define(f2b_dec5c(X), ?f2b_dec_c(X, 5)).
-define(f2b_dec6c(X), ?f2b_dec_c(X, 6)).

-define(b2f(X), binary_to_float(X)).
-define(b2f_safe(X, Err), ?catch_safe(?b2f(X), Err)).
-define(b2f_safe_err(X), ?b2f_safe(X, error)).


-define(a2s(X), atom_to_list(X)).
-define(s2a(X), list_to_atom(X)).
%%-define(a2l(X), atom_to_list(X)). % deprecated: better use string macro
%%-define(l2a(X), list_to_atom(X)). % deprecated: better use string macro
-define(s2exi_a(X), list_to_existing_atom(X)).

-define(a2b(X), ?a2b(X, latin1)).
-define(b2a(X), ?b2a(X, latin1)).
-define(a2b(X, Enc), atom_to_binary(X, Enc)).
-define(b2a(X, Enc), binary_to_atom(X, Enc)).
-define(b2exi_a(X), ?b2exi_a(X, latin1)).
-define(b2exi_a(X, Enc), binary_to_existing_atom(X, Enc)).
%%-define(b2a_e(X), ?b2a_e(X, latin1)). % deprecated
%%-define(b2a_e(X, Enc), binary_to_existing_atom(X, Enc)). % deprecated

-define(pid2s(X), pid_to_list(X)).
-define(s2pid(X), list_to_pid(X)).
%%-define(pid2l(X), pid_to_list(X)). % better use string macro
%%-define(l2pid(X), list_to_pid(X)). % better use string macro

-define(pid2b(X), ?s2b(?pid2s(X))).
-define(b2pid(X), ?s2pid(?b2s(X))).

-define(term2b(X), term_to_binary(X)).
-define(b2term(X), binary_to_term(X)).
-define(term2b(X, Opts), term_to_binary(X, Opts)).
-define(b2term(X, Opts), binary_to_term(X, Opts)).


-define(iol2b(X), iolist_to_binary(X)).
-define(iol2b_rev(X), ?iol2b(?l_rev(X))).

-define(m2p(X), maps:to_list(X)).
-define(p2m(X), maps:from_list(X)).
%%-define(m2l(X), maps:to_list(X)). % better use proplist macro
%%-define(l2m(X), maps:from_list(X)). % better use proplist macro

-define(l_rev(X), lists:reverse(X)).
-define(l_rev(X, Tail), lists:reverse(X, Tail)).


-define(err_badarg(), erlang:error(badarg)).
-define(err_badarg(Args), erlang:error(badarg, Args)).


-define(p_sort(X), lists:ukeysort(1, X)).
-define(p_merge_sort(X1, X2), lists:ukeymerge(1, X2, X1)). % reverse order because X2 must have maximum priority
