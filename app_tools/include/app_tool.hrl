%% app_tool header
%% directory
-define(DIR_DEF, [
    {protocol_erl, "../app_server/protocol/erl/"},
    {protocol_txt, "../app_server/protocol/txt/"},
    {header, "../app_server/include/gen/"},
    {out, "../app_server/src/gen/"}
]).

%% display
-define(MSG(S), io:format(S)).
-define(MSG(S, A), io:format(S, A)).

%% exit
-define(EXIT(E), erlang:exit(E)).
-define(EXITP(E), erlang:exit({parse_error, E})).
-define(EXITG(E), erlang:exit({generate_error, E})).

%% transform
-define(A2L(A), erlang:atom_to_list(A)).
-define(T2L(T), erlang:tuple_to_list(T)).
-define(T2B(T), erlang:term_to_binary(T)).
-define(A2B(A), erlang:atom_to_binary(A, utf8)).
-define(B2L(B), erlang:binary_to_list(B)).
-define(B2I(B), erlang:list_to_integer(?B2L(B))).
-define(I2L(I), erlang:integer_to_list(I)).
-define(L2B(S), erlang:list_to_binary(S)).

%% character
-define(TUP(S), string:to_upper(S)).
-define(TLW(S), string:to_lower(S)).

%% dict
-define(SDF(N, V), erlang:put(N, V)).
-define(GDF(N), erlang:get(N)).
-define(EDF(N), erlang:erase(N)).

%% file
-define(FOPEN(F, M), file:open(F, M)).
-define(FCLOSE(F), file:close(F)).
-define(FREAD(F), file:read_file(F)).
-define(FWRITE(F, S), file:pwrite(F, cur, ?L2B(S))).
-define(FWRITE(F, S, O), file:pwrite(F, {cur, O}, ?L2B(S))).

%% db
-define(DB_SRV, "192.168.1.73").
-define(DB_USR, "root").
-define(DB_PSW, "ybybyb").
-define(GAMEDB, "gamedb").

-define(MACRO_LIST, [
    #macro{table = "ingot_log_type", id = "id",
        sign = "sign", name = "name", prefix = "ILT_"}
]).

%% record
-record(doc, {
    pro_list = []
}).

-record(protocol, {
    name = "",
    file = "",
    id = 0,
    enum_list = [],
    list_list = [],
    class_list = [],
    action_list = []
}).

-record(enum, {
    name = "",
    val = 0
}).

-record(list, {
    name = "",
    class = "",
    field = []
}).

-record(class, {
    name = "",
    base = "",
    field = []
}).

-record(action, {
    name = "",
    id = 0,
    field_in = [],
    field_out = []
}).

-record(field, {
    name = "",
    type = nil, %% byte/short/int/long/enum/string/list/class
    class = "",
    field = []
}).

-record(db, {
    table_list = []
}).

-record(table, {
    name = "",
    column = []
}).

-record(column, {
    name,
    type,
    val,
    key,
    extra,
    comment,
    format_name
}).

-record(macro, {
    table,
    id,
    sign,
    name,
    prefix
}).