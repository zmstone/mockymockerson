
-ifndef(MOCKYMOCKERSON_PRIVATE_HRL_).
-define(MOCKYMOCKERSON_PRIVATE_HRL_, true).

-define(undef, undefined).
-define(SERVER, mockymockerson).

-define(excep(Detail), {mockymockerson_exception, Detail}).

%% a mocker of module:function/arity
-record(mock, {
    mfa,                       %% mocule:function/arity
    tester,                    %% test code module
    line,                      %% line in test code
    result,                    %% return value
    mocker,                    %% the function mocker. aka stub
    expArgs                    %% expected arg list
    }).

%% a call the to mocked module:function/arity
-record(mock_call, {
    mfa,
    realArgs
    }).

-endif.

