
-ifndef(MOCKYMOCKERSON_PRIVATE_HRL_).
-define(MOCKYMOCKERSON_PRIVATE_HRL_, true).

-define(undef, undefined).

-define(evaluate, '$MOCKYMOCKERSON_EVALUATE').
-define(exception, '$MOCKYMOCKERSON_EXCEPTION').
-define(excep(Detail), {?exception, Detail}).

%% a mocker of module:function/arity
-record(mock, {
    mfa,                       %% mocule:function/arity
    tester,                    %% test code module
    line,                      %% line in test code
    result,                    %% return value
    mocker,                    %% the function mocker. aka stub
    expArgs                    %% expected arg list
    }).

%% a call to the mocked module:function/arity
-record(exec, {
    mfa,
    realArgs
    }).

%% the loop data of a mocker worker
-record(mocker_state, {
     used_list = []  :: list()
    ,mock_list = []  :: list()
}).

-define(p(Str), io:put_chars(standard_error, Str)).
-define(fp(Fmt, Args), ?p(io_lib:format(Fmt, Args))).
-endif.

