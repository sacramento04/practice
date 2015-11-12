-module(lib_misc).

-export([
    ensure_ok/2,
    index_of/2,
    get_probability/1,
    get_probability_item/1,
	get_probability_item_by_ratio/1,
    random_number/1,
    ceil/1,
    floor/1,
    half_hour_pos/0,
    get_local_timestamp/0,
    get_timestamp/1,
    random_number_2/2,
	random_low_number_2/2,		%----大概率随机较小的数字----
	random_list/1,				%----列表乱序---
    get_random_one_from_list/1,	%----随机从列表中获取一个----
	is_list_cross/2,
    tc/4,
    test_loop/5,
    
	try_to_lower/1,
	str_to_lower/1,
	str_length/1,
    char_count/1,
    str_replace/2,
    str_replace_logic/3,
    
    try_apply/3,

    list_unique/1,

	is_today/3,
    is_today/2,
    is_today/1,
	is_before_yesterday/1,
	is_last_week/1,
	is_yesterday/1,
    is_now_week/1,
    time_difference/3,
    local_timestamp_to_datatime/1,
    for/3,
    
    try_apply_to_online_player/4,
    try_async_apply_to_online_player/4,
    try_async_apply_to_online_player/5,
    get_socket_info/1,
    
    tcp_send/2,
	today_left_second/0,				%今天还剩多少秒
	get_time_string/1,

    get_socket_long_ip/1,
	cancel_timer/1,                     %---取消计时器,容错
	get_monday_localtime/0,
	get_server_town_number/0,			%----获得跨服城镇编号
    get_next_monday_localtime/0,
    
    set_timer/4
]).

-include("app_server.hrl").


ensure_ok(ok, _) -> ok;
ensure_ok({error, Reason}, ErrorTag) -> throw({error, {ErrorTag, Reason}}).

cancel_timer (Id)->
    try timer:cancel(Id) of
        _ ->
            ok
    catch
        _ : _ ->
			ok
    end.

%% 获取元素在列表中的索引值
index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> 0;
index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [ _ | Tl], Index) -> index_of(Item, Tl, Index + 1).


%% 概率
get_probability (Probability) when Probability =< 0 ->
    false;
get_probability (Probability) when Probability >= 100 ->
    true;
get_probability (Probability) ->
    IntegerProbability = get_limit_integer(Probability),
    Rate = ceil(IntegerProbability / Probability * 100),
    IntegerProbability >= random_number(Rate).
    
    
%% 获取概率项 ProbabilityList = [{a, 20}, {b, 30}, {c, 50}]
get_probability_item (ProbabilityList) ->
    ProbabilityValue = lists:sum([Probability || {_, Probability} <- ProbabilityList]),
    RealProbabilityList = if
        ProbabilityValue > 100 ->
            exit(error_param);
        true ->
            [{Item, Value} || {Item, Value} <- ProbabilityList, Value > 0]
    end,
    
    RealRate = lists:foldl(
        fun({_, Probability}, RealRate) ->
            Rate = ceil(get_limit_integer(Probability) / Probability),
            if
                Rate > RealRate ->
                    Rate;
                true ->
                    RealRate
            end
        end,
        1,
        RealProbabilityList
    ),
    
    RandomNumber = random_number(100 * RealRate),
    
    {_, Random} = lists:foldl(
        fun({Item, Probability}, {Start, Random}) ->
            Number = Probability * RealRate,
            NextStart = Start + Number,
            
            if
                Random =:= null, RandomNumber >= Start, RandomNumber < Start + Number ->
                    {NextStart, {Item, Probability}};
                true ->
                    {NextStart, Random}
            end
        end,
        {1, null},
        RealProbabilityList
    ),
    Random.

%% 按比例获取概率项 ProbabilityList = [{a, 2}, {b, 3}, {c, 5}]
%% 总和不用一定为100,比例为整型
get_probability_item_by_ratio (ProbabilityList) ->
	ProbabilityValue = lists:sum([Probability || {_, Probability} <- ProbabilityList]),
	RealProbabilityList = [{Item, Value} || {Item, Value} <- ProbabilityList, Value > 0],
	RandomNumber = random_number(ProbabilityValue),
    
    {_, Random} = lists:foldl(
        fun({Item, Probability}, {Start, Rand}) ->
            NextStart = Start + Probability,
            
            if
                Rand =:= null, RandomNumber >= Start, RandomNumber < (Start + Probability) ->
                    {NextStart, {Item, Probability}};
                true ->
                    {NextStart, Rand}
            end
        end,
        {1, null},
        RealProbabilityList
    ),
    Random.

get_limit_integer (Number) ->
    UNewNumber = ceil(Number),
    if
        UNewNumber - Number < 0.00000001 ->
            UNewNumber;
        true ->
            get_limit_integer(Number * 10)
    end.

%% 随机数
random_number (Range) when Range =< 1 ->
    Range;
random_number (Range) ->
    crypto:rand_uniform(1, Range + 1).

random_number_2 (Min, Max) ->
    NewRange    = Max - Min + 1 ,
    random_number(NewRange) + Min - 1.
   
%% 大概率随机小的数字
random_low_number_2 (Min, Max) ->
	Ran1 = random_number_2(Min, Max),
	Ran2 = random_number_2(Min, Max),
	Ran3 = random_number_2(Min, Max),
	min(Ran1,min(Ran2,Ran3)).   
	
random_list (List)->
	TotalLen = length(List),
	Loop = lists:seq(1,TotalLen),
	{R,_}=lists:foldl(
		fun(_,{NewList,OldList})->
			Len = length(OldList),
			Index = random_number_2(1,Len),
			Val = lists:nth(Index,OldList),
			{
				[Val|NewList],
				lists:delete(Val,OldList)
			}
		end,
		{[],List},
		Loop
	),
	R.

get_random_one_from_list (List)->
	TotalLen = length(List),
	Index = random_number_2(1,TotalLen),
	Val = lists:nth(Index,List),
	Val.
	
%% 向上取整
ceil(X) when X < 0 ->
    trunc(X);


ceil(X) ->
    T = trunc(X),
    case X - T < 0.00000001 of
        true -> T;
        false -> T + 1
    end.


%% 向下取整
floor(X) when X < 0 ->
    T = trunc(X),
    case X - T < 0.00000001 of
        true -> T;
        false -> T - 1
    end;


floor(X) ->
    trunc(X).


get_local_timestamp() ->
    get_timestamp(erlang:localtime()).


get_timestamp (DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167248000.
    
local_timestamp_to_datatime(LocalTimeStamp) ->
    calendar:gregorian_seconds_to_datetime(LocalTimeStamp + 62167248000).    

% 判断是否是昨天之前
is_before_yesterday(TimeStamp) ->
    {{Y,M,D}, _Time}= erlang:localtime(),	
	BeforeYesterday = get_timestamp({{Y,M,D}, {0,0,0}}) - 86400,
	if TimeStamp < BeforeYesterday ->
			true;
		true ->
			false
	end.
	
% 今天剩余多少秒
today_left_second ()->
	{_, {Hour, Minute, Second}} = erlang:localtime(),
	86400 - (Hour * 60 * 60 + Minute * 60 + Second ).
	
is_yesterday(TimeStamp) ->
    {{Y,M,D}, _Time}= erlang:localtime(),	
	BeforeYesterday = get_timestamp({{Y,M,D}, {0,0,0}}),
	if TimeStamp < BeforeYesterday ->
			true;
		true ->
			false
	end.
	
% 判断是否是今天时间
is_today(TimeStamp, {_Hour, _Minute, _Second} = _Offset) ->
    NowTime     = get_local_timestamp(),
    if (NowTime - TimeStamp) > 86400 ->
        false ;
        true ->
            {NowDate, _Time}= erlang:localtime(),
            NewDate = if (_Time > _Offset) orelse (_Offset == {0 ,0 ,0})  ->
                NowDate;
                true ->
                {Y, M, D} = NowDate,
				if D == 1 ->
					M2 = M - 1,
					if M2 == 1 orelse M2 == 3 orelse M2 == 5 orelse M2 == 7 orelse M2 == 8 orelse M2 == 10 orelse M2 == 12 ->
						{Y, M2, 31};
					M2 == 2 ->
						{Y, M2, 28};
					true ->
						{Y, M2, 30}
					end;
				true ->
					{Y, M, D - 1}
				end
            end,
            StartTime = get_timestamp({NewDate, _Offset}) ,
            EndTime = StartTime + 86400,
            (TimeStamp >= StartTime) andalso (TimeStamp < EndTime)
    end.
    
% 判断是否是今天(注意NowTime要传入当前时间)
is_today (TimeStamp, {_Hour, _Minute, _Second} = _Offset, NowTime) ->  
    if (NowTime - TimeStamp) > 86400 ->
        false ;
        true ->
            {NowDate, _Time}= local_timestamp_to_datatime(NowTime),
            NewDate = if (_Time > _Offset) orelse (_Offset == {0 ,0 ,0})  ->
                NowDate;
                true ->
                {Y, M, D} = NowDate,
				if D == 1 ->
					M2 = M - 1,
					if M2 == 1 orelse M2 == 3 orelse M2 == 5 orelse M2 == 7 orelse M2 == 8 orelse M2 == 10 orelse M2 == 12 ->
						{Y, M2, 31};
					M2 == 2 ->
						{Y, M2, 28};
					true ->
						{Y, M2, 30}
					end;
				true ->
					{Y, M, D - 1}
				end
            end,
            StartTime = get_timestamp({NewDate, _Offset}) ,
            EndTime = StartTime + 86400,
            (TimeStamp >= StartTime) andalso (TimeStamp < EndTime)
    end.    
%% 是否今天(以凌晨5点做为界限)
is_today (TimeStamp) ->
    is_today (TimeStamp, {0, 0, 0}).
   
is_last_week (TimeStamp) ->
	TodayBeginTime = get_timestamp ({date(), {0,0,0}}),
	NowWeekDay = calendar:day_of_the_week(date()),
	NowWeekBeginTime = TodayBeginTime - (NowWeekDay - 1) * 86400,
	PreviousWeekBeginTime = NowWeekBeginTime - 86400 * 7,
	if ((TimeStamp > PreviousWeekBeginTime) and (TimeStamp < NowWeekBeginTime)) ->
			true;
		true ->
			false
	end.
is_now_week (TimeStamp1) ->
    TimeStamp   =  TimeStamp1 + 62167248000,
    {Date, _}   = calendar:gregorian_seconds_to_datetime(TimeStamp),
    {NowDate, _} = erlang:localtime(),
    WeekDay     = calendar:day_of_the_week(Date),
    NowWeekDay  = calendar:day_of_the_week(NowDate),
    if WeekDay > NowWeekDay ->
            false ;
        true ->
            LastDay = calendar:datetime_to_gregorian_seconds({Date,{0,0,0}}),
            NextDay = LastDay + 604800,
            NowTime = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
            NextDay > NowTime
    end.
    
% 获取时间间隔
time_difference(TimeStamp1,TimeStamp2,{Hour, Minute, Second} = _Offset) when 
    TimeStamp2 >= TimeStamp1     
->
      
    {Date1,_} = local_timestamp_to_datatime(TimeStamp1 - (Hour * 3600 + Minute * 60 + Second)),
      
    {Date2,_} = local_timestamp_to_datatime(TimeStamp2 - (Hour * 3600 + Minute * 60 + Second)),
    
    Day1 = calendar:date_to_gregorian_days(Date1),
    
    Day2 = calendar:date_to_gregorian_days(Date2),
    
    Day2 - Day1.
    
% 当前时间位置，每半小时分隔，1~48
half_hour_pos() ->
    {H, M, _S} = erlang:time(),
    H * 2  + case M of
        M when M >=30 ->
            2;
        M when M <30 ->
            1
   end.
   
   
tc(M, F, A, N) when N > 0 ->  
    L = test_loop(M, F, A, N, []),  
    Len = length(L),  
    LSorted = lists:sort(L),  
    Min = lists:nth(1, LSorted),  
    Max = lists:nth(Len, LSorted),  
    Med = lists:nth(round(Len/2), LSorted),  
    Avg = round(lists:foldl(fun(X, Sum) ->  
                    X + Sum end,  
                0,  
                LSorted)/Len),  
    io:format("Range:~b - ~b mics~n"  
          "Median:~b mics ~n"  
          "Average:~b mics ~n",  
          [Min, Max, Med, Avg]),  
    Med.  
  
test_loop(_M, _F, _A, 0, List) ->  
    List;  
test_loop(M, F, A, N, List) ->  
    {T, _R} = timer:tc(M, F, A),  
    test_loop(M, F, A, N-1, [T|List]).


try_to_lower(UChar) when (UChar > 64) andalso (UChar < 91) -> UChar + 32;
try_to_lower(LChar) -> LChar.

str_to_lower(String) when is_list(String) ->
	lists:map(fun(Char) -> 
			try_to_lower(Char)
		end, String);
str_to_lower(Input) -> Input.

str_length([]) ->
	0;
str_length([Char1 | String]) ->
	str_length(Char1, String).
str_length(Char1, String) when Char1 < 16#80 ->
	str_length(String) + 1;
str_length(Char1, String) when Char1 < 16#E0 ->
	[_Char2 | String2] = String,
	str_length(String2) + 2;
str_length(Char1, String) when Char1 < 16#F0 ->
	[_Char2, _Char3 | String2] = String,
	str_length(String2) + 2;
str_length(Char1, String) when Char1 < 16#F8 ->
	[_Char2, _Char3, _Char4 | String2] = String,
	str_length(String2) + 2;
str_length(Char1, String) when Char1 < 16#FC->
	[_Char2, _Char3, _Char4, _Char5 | String2] = String,
	str_length(String2) + 2;
str_length(Char1, String) when Char1 < 16#FE->
	[_Char2, _Char3, _Char4, _Char5, _Char6 | String2] = String,
	str_length(String2) + 2.

char_count([]) ->
	0;
char_count([Char1 | String]) ->
	char_count(Char1, String).
char_count(Char1, String) when Char1 < 16#80 ->
	char_count(String) + 1;
char_count(Char1, String) when Char1 < 16#E0 ->
	[_Char2 | String2] = String,
	char_count(String2) + 1;
char_count(Char1, String) when Char1 < 16#F0 ->
	[_Char2, _Char3 | String2] = String,
	char_count(String2) + 1;
char_count(Char1, String) when Char1 < 16#F8 ->
	[_Char2, _Char3, _Char4 | String2] = String,
	char_count(String2) + 1;
char_count(Char1, String) when Char1 < 16#FC->
	[_Char2, _Char3, _Char4, _Char5 | String2] = String,
	char_count(String2) + 1;
char_count(Char1, String) when Char1 < 16#FE->
	[_Char2, _Char3, _Char4, _Char5, _Char6 | String2] = String,
	char_count(String2) + 1.

str_replace(TemplateMessage,ParameterList) when
    is_list(TemplateMessage),
    is_list(ParameterList)
->
    str_replace_logic(TemplateMessage,ParameterList,"").
    
str_replace_logic(TemplateMessage,ParameterList,Message) when
    is_list(TemplateMessage),
    is_list(ParameterList),
    is_list(Message)
->  
    FilterParameterList = lists:filter(
        fun({Key,_}) ->
            KeyString = "{" ++ erlang:atom_to_list(Key) ++ "}",
            
            KeyStartIndex = string:str(TemplateMessage,KeyString),
             
            KeyStartIndex =/= 0  
        end,
        ParameterList
    ),
    
    case FilterParameterList of
        [] ->
            Message ++ TemplateMessage;
            
        _Other ->
            IndexParameterList = lists:map(
                fun({Key,Value}) ->
                    KeyString = "{" ++ erlang:atom_to_list(Key) ++ "}",
            
                    KeyStartIndex = string:str(TemplateMessage,KeyString),
            
                   {KeyStartIndex,KeyString,Value}
                end,
                FilterParameterList
            ),
            
            SortParameterList = lists:sort(
                fun({Index1,_,_},{Index2,_,_}) ->
                    Index1 < Index2
                end,
                IndexParameterList
            ),
    
            [{SortStartIndex,SortKeyString,SortValue}] = lists:sublist(SortParameterList, 1, 1),
            
            SortValueString = if
                is_integer(SortValue) ->
                    erlang:integer_to_list(SortValue);
                
                is_float(SortValue) ->
                    erlang:float_to_list(SortValue);
                    
                is_atom(SortValue) ->
                    erlang:atom_to_list(SortValue);
                    
                true ->
                    SortValue
                
            end,
    
            if 
                SortStartIndex =:= 1 -> 
            
                    TM1 = string:substr(TemplateMessage, erlang:length(SortKeyString) + 1, erlang:length(TemplateMessage) - erlang:length(SortKeyString)), 
            
                    MS1 = Message ++ SortValueString,
            
                    str_replace_logic(TM1,ParameterList,MS1);
            
                true ->
                    TM1 = string:substr(TemplateMessage, erlang:length(SortKeyString) + SortStartIndex , erlang:length(TemplateMessage) - erlang:length(SortKeyString) -SortStartIndex + 1 ), 
                    
                    Head = string:substr(TemplateMessage, 1, SortStartIndex - 1),
            
                    MS1 = Message ++ Head ++  SortValueString,
            
                    str_replace_logic(TM1,ParameterList,MS1)
            end    
    end.
    
try_apply(M, F, A) ->
    case catch apply(M, F, A) of
        {'EXIT', Reason} -> 
            ?ERROR(
                "try_apply:~n"
                "  {M, F, A} = {~p, ~p, ~p}~n"
                "  Reason    = ~p~n",
                [M, F, A, Reason]
            ),
            try_apply_failed;
        Result ->
            Result
    end.

    
list_unique(List) when is_list(List) ->
    lists:foldl(
        fun(Item, Sum)->
            case lists:member(Item, Sum) of
                false ->
                    [Item | Sum ];
                true  ->
                    Sum
            end
        end,
        [],
        List
    ).
    
%% for(Max,Max,F) ->[F(Max)];
%% for(I,Max,F) -> [F(I) | for(I + 1,Max, F)].

for (I, Max, F) -> for(I, Max, F, []).

for (Max, Max, F, R) -> lists:reverse([F(Max)|R]);
for (I, Max, F, R) -> for(I + 1, Max, F, [F(I)|R]).
    

try_apply_to_online_player (PlayerId, M, F, A) when
    is_number(PlayerId),
    is_atom(M),
    is_atom(F),
    is_list(A)
->
    try mod_online:apply_to_online_player(PlayerId, M, F, A) of
        false ->
            game_worker_sup:do_work(M, F, A);
        Result ->
            Result
    catch
        _ : Reason ->
            {error, Reason}
    end.
    
try_async_apply_to_online_player (PlayerId, M, F, A) ->
    try_async_apply_to_online_player(PlayerId, M, F, A, null).
try_async_apply_to_online_player (PlayerId, M, F, A, CallBack) when
    is_number(PlayerId),
    is_atom(M),
    is_atom(F),
    is_list(A)
->
    TheCallBack = if
        is_tuple(CallBack) -> {self(), CallBack};
        true -> null
    end,
    try mod_online:async_apply_to_online_player(PlayerId, M, F, A, TheCallBack) of
        false ->
            R = game_worker_sup:do_work(M, F, A),
            if
                is_tuple(CallBack) ->
                    {CM, CF, CA} = CallBack,
                    try_apply(CM, CF, [R | CA]);
                true -> R
            end;
        Result ->
            Result
    catch
        _ : Reason ->
            {error, Reason}
    end.

get_socket_info (Socket) ->
    {ok, {PeerAddress, PeerPort}} = inet:peername(Socket),
    {inet_parse:ntoa(PeerAddress), PeerPort}.

get_socket_long_ip (Socket) ->
    {ok, {{Ip1, Ip2, Ip3, Ip4}, _PeerPort}} = inet:peername(Socket),
    trunc(Ip1 * math:pow(256, 3) + Ip2 * math:pow(256, 2) + Ip3 * 256 + Ip4).
   
tcp_send (Socket, Bin) ->
    case gen_tcp:send(Socket, Bin) of
       ok ->
            true;
       {error, Reason} ->
            exit({game_tcp_send_error, Reason})
    end.
	
get_time_string(Time) ->
	{Hour, Minute, _Second} = Time,
	HourString = if Hour < 10 ->
			"0" ++ integer_to_list(Hour);
		true -> integer_to_list(Hour)
	end,
	MinuteString = if Minute < 10 ->
			"0" ++ integer_to_list(Minute);
		true -> integer_to_list(Minute)
	end,
	TimeString = HourString ++ ":" ++ MinuteString,
	TimeString.
	
% 获取本周星期一0点的时间戳
get_monday_localtime () ->
	% 7天 = 604800s
	NowSecond = lib_misc:get_local_timestamp(),
	{Date,Time} = erlang:localtime(),
	
	DayNum = calendar:day_of_the_week(Date),
	FromMonday = DayNum - 1,
	SecondFromToday = calendar:time_to_seconds(Time),
	
	MondayLocalTime = (NowSecond - SecondFromToday) - 86400 * FromMonday,
	MondayLocalTime.
	
% 获取下周一0点的时间戳
get_next_monday_localtime () ->
	get_monday_localtime() + 86400 * 7.
    
get_server_town_number ()->
	%-----获得本服节点------
	NodeName = atom_to_list(node()),
	case re:run(NodeName,"server_town_(.*)@.*",[{capture,[1],list}]) of
		{match, [Bin]}->
			list_to_integer(Bin);
		match ->
			0;
		nomatch ->
			0
	end.
    
set_timer (Time, M, F, A) ->
    {ok, TRef} = timer:apply_after(Time, M, F, A),
    TRef.
	
is_list_cross (L1, L2) ->
    is_list_cross(L1, L2, false).
    
is_list_cross ([], _, Result) ->
    Result;
is_list_cross ([E | L], L2, Result) ->
    if
        Result ->
            Result;
        true ->
            is_list_cross(L, L2, lists:member(E, L2))
    end.
