
-module(mymatch).

-export([run/4]).

-define(match, '$MY_MATCH').
-define(mismatch, '$MY_MISMATCH').

-include("mymatch.hrl").

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
run(Module, Line, ExpectedValue, RealValue) ->
    case ExpectedValue == RealValue of
        true ->
            ?match;
        false ->
            fp("Match failed at line ~p~n", [Line]),
            Format = format_match(ExpectedValue, RealValue),
            print_match_format(Module, Format),
            exit(my_match_failure)
    end.

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
format_match(A, B)
  when is_list(A),
       is_list(B),
       length(A) == length(B) ->
    format_list_match(A, B);
format_match(A, B)
  when is_tuple(A),
       is_tuple(B),
       tuple_size(A) == tuple_size(B) ->
    format_tuple_match(A, B);
format_match(A, B) ->
    format_atomic_match(A, B).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
format_atomic_match(A, B) ->
    case A == B of
        true ->
            {?match, A};
        false ->
            {?mismatch, A, B}
    end.

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
format_list_match(A, B) ->
    case is_printable(A) andalso is_printable(B) of
        true ->
            format_atomic_match(A, B);
        false ->
            format_list_match_loop(A, B, [])
    end.

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
format_list_match_loop([], [], Result) ->
    lists:reverse(Result);
format_list_match_loop([H1 | T1], [H2 | T2], Result) ->
    format_list_match_loop(T1, T2, [format_match(H1, H2) | Result]).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
format_tuple_match(A, B) ->
    LA = tuple_to_list(A),
    LB = tuple_to_list(B),
    Result = format_list_match_loop(LA, LB, []),
    list_to_tuple(Result).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
print_match_format(Module, Format) ->
    Records = caes rr:run(Module) of
        RecordList when is_list(RecordList) ->
            RecordList;
        _ ->
            []
    end,
    print_match_format(Records, 0, [], Format).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
print_match_format(_Records, Indention, Tag, {?match, V}) ->
    fp("~s~s~n", [make_indented_tag(Indention, Tag), term2string(V)]);
print_match_format(_Records, Indention, Tag, {?mismatch, V1, V2}) ->
    Indention_Tag = make_indented_tag(Indention, Tag),
    Padding = [$\s || _ <- Indention_Tag],
    fp("~sEXPECTED = ~s~n", [Indention_Tag, term2string(V1)]),
    fp("~sREAL VAL = ~s~n", [Padding,        term2string(V2)]);
print_match_format(Records, Indention, Tag, L) when is_list(L) ->
    print_list_match_format(Records, Indention, Tag, L);
print_match_format(Records, Indention, Tag, T) when is_tuple(T) ->
    print_tuple_match_format(Records, Indention, Tag, T).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
print_list_match_format(Records, Indention, Tag, L) ->
    fp("~s[...]~n", [make_indented_tag(Indention, Tag)]),
    TagList = make_list_tag_list(length(L)),
    print_list_match_format_loop(Records, Indention + 1, ew(TagList), L).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
print_tuple_match_format(Records, Indention, Tag, T) ->
    L = tuple_to_list(T),
    Indention_Tag = make_indented_tag(Indention, Tag),
    case make_tuple_tag_list(Records, T) of
        {list, TagList} ->
            fp("~s{...}~n", [Indention_Tag]),
            print_list_match_format_loop(Records, Indention+1, ew(TagList), L);
        {tuple, [RecordTag | TagList]} ->
            fp("~s#~s{...}~n", [Indention_Tag, RecordTag]),
            [_ | R] = L,
            print_list_match_format_loop(Records, Indention+1, ew(TagList), R)
    end.

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
print_list_match_format_loop(_Records, _Indention, _TagList, []) ->
    ok;
print_list_match_format_loop(Records, Indention, [Tag | RestTags], [H | T]) ->
    print_match_format(Records, Indention, Tag, H),
    print_list_match_format_loop(Records, Indention, RestTags, T).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
make_tuple_tag_list(Records, T) when is_tuple(T) ->
    make_tuple_tag_list(Records, tuple_to_list(T));
make_tuple_tag_list(Records, [{?match, Tag} | FieldValues]) when is_atom(Tag) ->
    case get_record_definition(Records, Tag) of
        undefined ->
            {list, make_list_tag_list(length(FieldValues) + 1)};
        FieldTags ->
            {tuple, [term2string(E) || E <- [Tag | FieldTags]]}
    end;
make_tuple_tag_list(_Records, L) when is_list(L) ->
    {list, make_list_tag_list(length(L))}.

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
get_record_definition(Records, Tag) ->
    case lists:keysearch(Tag, 1, Records) of
        {value, {Tag, Fields}} ->
            Fields;
        false ->
            undefined
    end.

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
make_list_tag_list(N) ->
    IntegerList = lists:seq(1, N),
    [term2string(I) || I <- IntegerList].

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
make_indented_tag(0, _Tag) ->
    [];
make_indented_tag(Indention, Tag) ->
    make_indention(Indention, []) ++ Tag ++ ": ".

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
make_indention(0, Str) ->
    Str;
make_indention(N, Str) ->
    make_indention(N-1, [$\s, $\s, $\s, $\s | Str]).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
is_printable(L) when is_list(L) ->
    Pred = fun(C) ->
        C > 8 andalso C < 127
    end,
    lists:all(Pred, L).

%% -----------------------------------------------------------------------------
%% make equivalent width string list
%% -----------------------------------------------------------------------------
ew(StrList) ->
    MaxFun = fun(Str, Max) -> erlang:max(length(Str), Max) end,
    Width = lists:foldl(MaxFun, 0, StrList),
    FormatStr = "~-" ++ term2string(Width) ++ "s",
    [lists:flatten(io_lib:format(FormatStr, [S])) || S <- StrList].

%% -----------------------------------------------------------------------------
%% function: term2string(term()) -> string()
%%
%% convert term to string
%% -----------------------------------------------------------------------------
term2string(T) ->
  lists:flatten(io_lib:format("~100000p", [T])).
  %% re:replace(Str, "\\n", "\\\\n", [global, {return, list}]).

%% -----------------------------------------------------------------------------
%% formated print
%% -----------------------------------------------------------------------------
fp(String) ->
    io:put_chars(String),
    io:put_chars(standard_error, String).

fp(FormatStr, ArgList) ->
    fp(io_lib:format(FormatStr, ArgList)).

