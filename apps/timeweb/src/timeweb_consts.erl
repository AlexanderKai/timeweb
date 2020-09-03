-module(timeweb_consts).

-compile([export_all]).

code(1) -> <<"Unknown error">>;
code(2) -> <<"Invalid request">>;
code(3) -> <<"Not authorized">>;
code(4) -> <<"Internal db error">>;
code(5) -> <<"Permission is not granted">>;
code(10) -> <<"User can't login">>;
code(11) -> <<"User can't register">>;
code(12) -> <<"User is not found">>;
code(13) -> <<"Wrong login or password">>;
code(14) -> <<"Same password">>;
code(15) -> <<"Invalid current password">>.
