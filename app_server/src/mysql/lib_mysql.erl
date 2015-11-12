-module(lib_mysql). 
-export([
    insert/2,
    get_rows/1,
    get_rows/4,
    get_field/2,
    update_row/4,
    condition_string/1,
    mysql_update_string/1,
	mysql_binary_to_list/1
        ]).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-include("mysql.hrl").

insert(PoolId, Sql) ->
    case mysql:fetch(PoolId, Sql) of
        {updated,Result} ->
            Result #mysql_result.insert_id;
        _ ->
            0
    end.

%%--------------------------------------------------------------------
%% Function: get_rows(MysqlResult)
%%           MysqlResult= mysql_result(),    mysql:fetch/3，返回的结果集
%% Descrip.: 根据结果集，返回带字段描述的行列表
%% Returns : [{FieldName, Value}] | nomatch
%%           FieldName  = atom(), 字段名
%%           Value      = [integer()] | string(), 字段值
%% Todo    : 返回结果值的类型进行转换
%%           Mysql类别    返回值类别
%%           VARCHAR        string()
%%           INTEGER        integer()
%%           FLOAT          float()
%% Usage    :
%%
%%--------------------------------------------------------------------
get_rows(#mysql_result{fieldinfo = FieldInfo, rows=AllRows}) ->
    case AllRows of
        [] ->
            [];
        _ ->
            [readble_row(Row, FieldInfo) || Row <- AllRows]
    end.

readble_row(Row, FieldInfo) ->
     SeqList = lists:seq(1, length(Row)),
     [readble_column(Seq, Row, FieldInfo)|| Seq <- SeqList].

readble_column(N, Row, FieldInfo) ->
    {_, FieldName, _, _FieldType} = lists:nth(N, FieldInfo),
    Value = lists:nth(N, Row),
    %ReturnValue = case FieldType of
    %    _  when Value == null->
    %        null;
    %    'LONG'  ->      %% INT
    %        list_to_integer(Value);
    %    'LONGLONG'  ->  %% INT
    %        list_to_integer(Value);
    %    'FLOAT'  ->     %% FLOAT
    %        case lists:member( $. , Value) of
    %            true ->
    %                list_to_float(Value);
    %            false ->
    %                list_to_float(Value ++ ".0")
    %        end;
    %    'TINY'  ->      %% TINYINT
    %        list_to_integer(Value);
    %    'SHORT'  ->      %% SMALLINT
    %        list_to_integer(Value);
    %    'INT24'  ->      %% MEDIUMINT
    %        list_to_integer(Value);
    %    _       ->
    %        Value
    %end,
    %{list_to_atom(FieldName), Value}.
	{FieldName, Value}.
%%--------------------------------------------------------------------
%% Function: get_field(ReadableRow, Field)
%%           ReadableRow    [{Field, Value}]
%% Descrip.: 根据记录的字段，返回字段值
%% Returns : FieldValue
%%           FieldValue = integer() | string() | float()
%% Usage    :
%%
%%--------------------------------------------------------------------
get_field(ReadableRow, Field) ->
    {_, Value} = lists:keyfind(Field, 1, ReadableRow),
    Value.

%%--------------------------------------------------------------------
%% Function: get_rows(PoolId, Table, Condition, Fields)
%%           PoolId     = atom()        mysql pool id
%%           Table      = string()      数据库表名
%%           Conditions = string()      查询条件,见 condition_string/1
%%           Fields     = string()      数据库字段名
%% Descrip.: 条件查询模板数据
%% Returns : ReadableRows | false  出错时返回 false
%%           Condition  = string()      根据传入的Condition转换的WHERE子句，可以作为ets的key
%%           ReadableRows               参见 get_rows/1,作为ets的值
%% Example  :
%% (server_00@localhost)20> lib_mysql:get_data(mysql_pool, "test", "idtest = 1", "*").
%% [[{"idtest",1},{"tiny",2},{"small",3},{"medium",4}]]
%%
%%--------------------------------------------------------------------
get_rows(PoolId, Table, Condition, Fields) ->
    BinFields = list_to_binary(Fields),
    BinTableName = list_to_binary(Table),
    BinWhere = list_to_binary(Condition),
    Sql = <<
        "SELECT ", BinFields/binary,
        " FROM ", BinTableName/binary,
        " WHERE ", BinWhere/binary
        >>,
    case mysql:fetch(PoolId, [Sql]) of
        {data, Result} ->
            get_rows(Result);
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% Function: get_value_from_readable_row(ReadableRow, Field)
%%           ReadableRow    [{Field, Value}]
%%           Field = string()   字符名称
%%           Value = integer()  匹配的值，暂时只支持integer()
%% Descrip.: 根据记录的字段，返回字段值
%% Returns : Str    = string()  字符串，可以作为ets的key
%% Example :
%%           "id = 5 AND type = 2" = condition_string([{"id", 1}, {"type", 2}])
%%--------------------------------------------------------------------
condition_string(Condition) ->
    List = [ [" AND " , Field , " = " , integer_to_list(Value) ] || {Field, Value} <- Condition ],
    lists:nthtail(5, lists:flatten(List)).

%%--------------------------------------------------------------------
%% Function: update_data(PoolId, Table, Key, Values)
%%           PoolId     = atom()        mysql pool id
%%           Table      = string()      数据库表名
%%           Key        = string()      参见 condition_string/1 的返回值
%%           Values     = ReadableRows  参见 get_rows/1,作为ets的值
%% Descrip.: 根据记录的字段，返回字段值
%% Returns : UpdateRows | false
%%           UpdateRows = integer()     更新的记录数
%% Example :
%%           (server_00@localhost)63> Row1.
%%           [{"idtest",1},{"tiny",2},{"small",3},{"medium",4}]
%%--------------------------------------------------------------------
update_row(PoolId, Table, Key, Values) ->
    BinTableName    = list_to_binary(Table),
    BinUpdateFields = list_to_binary(mysql_update_string(Values)),
    BinWhere        = list_to_binary(Key),
    Sql = <<
        "UPDATE ",  BinTableName/binary,
        " SET ",    BinUpdateFields/binary,
        " WHERE ",  BinWhere/binary
        >>,
    io:format("~p~n", [Sql]),
    case mysql:fetch(PoolId, [Sql]) of
        {updated, Result} ->
            mysql:get_result_affected_rows(Result);
        _ ->
            false
    end.

mysql_update_string(Values) ->
    List = [
        [" , " , Field , " = " , value_to_list(Value), "" ]
        || {Field, Value} <- Values],
    lists:nthtail(3, lists:flatten(List)).

value_to_list(Value) ->
    case Value of
        _ when Value == null ->
            "NULL";
        _ when is_integer(Value) ->
            integer_to_list(Value);
        _ when is_float(Value) ->
            float_to_list(Value);
        _ ->
            mysql:quote(Value)
    end.
mysql_binary_to_list(<<>>)->
	"";
mysql_binary_to_list(undefined)->
	"";
mysql_binary_to_list(Value)->
	binary_to_list(Value).