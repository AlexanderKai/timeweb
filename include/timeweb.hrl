-define(USER_SESSION_TIMEOUT, 1 * 60).

-define(DESCRIPTION(Code), timeweb_consts:code(Code)).

-define(ERROR(Code), #{code => Code, description => ?DESCRIPTION(Code)}).

-define(E_UNKNOWN_ERROR, 1).
-define(E_INVALID_REQUEST, 2).
-define(E_NOT_AUTHORIZED, 3).
-define(E_INTERNAL_DB_ERROR, 4).
-define(E_PERMISSION_ISNT_GRANTED, 5).
-define(E_CANNOT_CREATE_USER_SESSION, 10).
-define(E_CANNOT_REGISTER, 11).
-define(E_USER_IS_NOT_FOUND, 12).
-define(E_WRONG_LOGIN_OR_PASSWORD, 13).
-define(E_CANNOT_CHANGE_SAME_PASSWORD, 14).
-define(E_INVALID_OLD_PASSWORD, 15).

