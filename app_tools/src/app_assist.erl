-module(app_assist).

-export([main/1]).

-include("app_tool.hrl").


main ([Option]) ->
    try
        run(list_to_integer(Option))
    catch
        _ : {parse_error, R} ->
            ?MSG("~p in File ~p, Line ~p.~n", [R, ?GDF(file), ?GDF(line)]);
        _ : {generate_error, R} ->
            ?MSG("~p~n", [R]);
        _ : E ->
            ?MSG("{~p, ~p}~n", [E, erlang:get_stacktrace()])
    end;
main (_) ->
    usage().

usage () ->
    ?MSG("usage: ...~n"),
    halt(1).
    
run (1) ->
    ?MSG("Transform Protocol To Erlang ...~n"),
    check_dir(get_default_dir(protocol_txt)),
    check_dir(get_default_dir(protocol_erl)),
    delete_dir_file(get_default_dir(protocol_erl), ".erl"),
    generate_protocol(file_find(get_default_dir(protocol_txt), ".txt")),
    ok;
run (_) ->
    ok.
    
generate_protocol ([]) ->
    ok;
generate_protocol ([File | Left]) ->
    init_parse_param(File),
    write_to_file(File),
    clean_parse_param(),
    generate_protocol(Left).
    
write_to_file (File) ->
    {ok, Bin} = ?FREAD(File),
    parse_file_bin(Bin),
    write_protocol_erl(File).
    
write_protocol_erl (File) ->
    ?MSG("==> ~p~n", [File]),
    Dir = get_default_dir(protocol_erl),
    {ok, Fd} = ?FOPEN(Dir ++ filename:basename(File, ".txt") ++ ".erl", [write]),
    ?FWRITE(Fd, "%% " ++ filename:basename(File, ".txt") ++ ".erl\n#{\n"),
    read_list_to_write(name, Fd),
    read_list_to_write(id, Fd),
    read_list_to_write(action, Fd),
    read_list_to_write(class, Fd),
    ?FWRITE(Fd, "}."),
    ?FCLOSE(Fd).
    
read_list_to_write (Field, Fd) ->
    lists:foreach(
        fun(L) ->
            case L of
                {String, Depth, Offset} -> 
                    ?FWRITE(Fd, get_depth_prefix(Depth) ++ String, Offset);
                {String, Depth} -> 
                    ?FWRITE(Fd, get_depth_prefix(Depth) ++ String)
            end
        end,
        lists:reverse(?GDF(Field))
    ).
    
get_depth_prefix (Depth) ->
    get_depth_prefix(Depth, "").
    
get_depth_prefix (0, Prefix) ->
    Prefix;
get_depth_prefix (Depth, Prefix) ->
    get_depth_prefix(Depth - 1, [$\t | Prefix]).
    
check_dir (Dir) ->
    case filelib:is_dir(Dir) of
        false -> filelib:ensure_dir(Dir);
        true -> ok
    end.
    
file_find (Dir, Suf) ->
    filelib:wildcard(Dir ++ "*" ++ Suf).
    
delete_dir_file (Dir, Suf) ->
    [ok = file:delete(F) || F <- file_find(Dir, Suf)].
    
init_parse_param (File) ->
    ?SDF(name, []),
    ?SDF(id, []),
    ?SDF(class, []),
    ?SDF(action, []),
    ?SDF(file, filename:basename(File)),
    ?SDF(line, 1).
    
clean_parse_param () ->
    ?EDF(name),
    ?EDF(id),
    ?EDF(class),
    ?EDF(action),
    ?EDF(file),
    ?EDF(line).
    
get_default_dir (T) ->
    {T, Path} = lists:keyfind(T, 1, ?DIR_DEF),
    Path.
    
write (Field, String, Depth) ->
    ?SDF(Field, [{String, Depth} | ?GDF(Field)]).
    
write (Field, String, Depth, Offset) ->
    ?SDF(Field, [{String, Depth, Offset} | ?GDF(Field)]).
    
parse_file_bin (Bin) ->
    write(name, "name => ", 0),
    write(id, "id => ", 0),
    write(class, "class => [  \n", 0),
    write(action, "action => [  \n", 0),
    
    Bin1 = case next_token(Bin) of
        {equal, V1, B1} ->
            case next_token(B1) of
                {lbrace, V2, B2} ->
                    write(name, ?TLW(?B2L(V1)) ++ ",\n", 0),
                    write(id, ?TLW(?B2L(V2)) ++ ",\n\n", 0),
                    B2;
                _ ->
                    ?EXITP("Module define error")
            end;
        _ ->
            ?EXITP("Module define error")
    end,
    
    Bin2 = parse_body(Bin1, 1),
    eof = next_token(Bin2),
    write(action, "\n], \n\n", 0, -3),
    write(class, "\n]\n", 0, -3).
    
parse_body (Bin, Depth) ->
    case next_token(Bin) of
        {class, _V1, B1} ->
            case next_token(B1) of
                {lbrace, V2, B2} ->
                    write(class, "%% class => " ++ ?TLW(?B2L(V2)) ++ "\n", Depth),
                    write(class, "#{\n", Depth),
                    write(class, "name => " ++ ?TLW(?B2L(V2)) ++ ",\n", Depth + 1),
                    write(class, "base => nil,\n\n", Depth + 1),
                    write(class, "field => [  \n", Depth + 1),
                    NBin = parse_class(B2, class, Depth + 2),
                    write(class, "\n", 0, -3),
                    write(class, "]\n", Depth + 1),
                    write(class, "},\n\n", Depth),
                    parse_body(NBin, Depth);
                {colon, V2, B2} ->
                    case next_token(B2) of
                        {lbrace, V3, B3} ->
                            write(class, "%% class => " ++ ?TLW(?B2L(V2)) ++ "\n", Depth),
                            write(class, "#{\n", Depth),
                            write(class, "name => " ++ ?TLW(?B2L(V2)) ++ ",\n", Depth + 1),
                            write(class, "base => '" ++ ?TLW(?B2L(V3)) ++ "',\n\n", Depth + 1),
                            write(class, "field => [  \n", Depth + 1),
                            NBin = parse_class(B3, class, Depth + 2),
                            write(class, "\n", 0, -3),
                            write(class, "]\n", Depth + 1),
                            write(class, "},\n\n", Depth),
                            parse_body(NBin, Depth);
                        _ ->
                            ?EXITP("Class define error")
                    end;
                _ ->
                    ?EXITP("Class define error")
            end;
        {equal, V1, B1} ->
            case next_token(B1)of
                {lbrace, V2, B2} ->
                    write(action, "%% " ++ ?TLW(?B2L(V2)) ++ " => " ++ ?TLW(?B2L(V1)) ++ "\n", Depth),
                    write(action, "#{\n", Depth),
                    write(action, "name => " ++ ?TLW(?B2L(V1)) ++ ",\n", Depth + 1),
                    write(action, "id => " ++ ?TLW(?B2L(V2)) ++ ",\n\n", Depth + 1),
                    NBin = parse_action(B2, action, Depth + 2),
                    write(action, "},\n\n", Depth),
                    parse_body(NBin, Depth);
                _ ->
                    ?EXITP("Action define error")
            end;
        {rbrace, _V1, B1} ->
            B1;
        _ ->
            ?EXITP("Module define error")
    end.
    
parse_action (Bin, Field, Depth) ->
    case next_token(Bin) of
        {in, _V1, B1} ->
            case next_token(B1) of
                {lbrace, _V2, B2} ->
                    write(Field, "in => [  \n", Depth - 1),
                    NBin = parse_action_body(B2, Field, Depth),
                    write(Field, "\n", 0, -3),
                    write(Field, "],\n\n", Depth - 1),
                    parse_action(NBin, Field, Depth);
                _ ->
                    ?EXITP("Action in define error")
            end;
        {out, _V1, B1} ->
            case next_token(B1) of
                {lbrace, _V2, B2} ->
                    write(Field, "out => [  \n", Depth - 1),
                    NBin = parse_action_body(B2, Field, Depth),
                    write(Field, "\n", 0, -3),
                    write(Field, "]\n", Depth - 1),
                    parse_action(NBin, Field, Depth);
                _ ->
                    ?EXITP("Action out define error")
            end;
        {rbrace, _V1, B1} ->
            B1;
        _ ->
            ?EXITP("Action define error")
    end.
    
parse_action_body (Bin, Field, Depth) ->
    case next_token(Bin) of
        {colon, V1, B1} ->
            case field_type(B1) of
                {list, B2} ->
                    case next_token(B2) of
                        {lbrace, _V2, B3} ->
                            write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", list, [  \n", Depth),
                            NBin = parse_list(B3, Field, Depth + 1),
                            write(Field, "]}, \n", Depth),
                            parse_action_body(NBin, Field, Depth);
                        {langular, _V2, B3} ->
                            write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", list, ", Depth),
                            NBin = parse_list(B3, Field, Depth + 1),
                            write(Field, "}, \n", 0),
                            parse_action_body(NBin, Field, Depth);
                        _ ->
                            ?EXITP("List define error")
                    end;
                {typeof, B2} ->
                    case next_token(B2) of
                        {langular, _V2, B3} ->
                            write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", class, ", Depth),
                            {Typeof, NBin} = parse_typeof(B3),
                            write(Field, Typeof ++ "}, \n", 0),
                            parse_action_body(NBin, Field, Depth);
                        _ ->
                            ?EXITP("Typeof define error")
                    end;
                {enum, B2} ->
                    case next_token(B2) of
                        {lbrace, _V2, B3} ->
                            write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", enum, [  \n", Depth),
                            NBin = parse_enum(B3, Field, Depth + 1),
                            write(Field, "\n", 0, -3),
                            write(Field, "]}, \n", Depth),
                            parse_action_body(NBin, Field, Depth);
                        _ ->
                            ?EXITP("Enum define error")
                    end;
                {FT, B2} ->
                    write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", " ++ ?A2L(FT) ++ "}, \n", Depth),
                    parse_action_body(B2, Field, Depth);
                _ ->
                    ?EXITP("Action field define error")
            end;
        {rbrace, _V1, B1} ->
            B1;
        _ ->
            ?EXITP("Action field define error")
    end.
    
parse_class (Bin, Field, Depth) ->
    case next_token(Bin) of
        {colon, V1, B1} ->
            case field_type(B1) of
                {list, B2} ->
                    case next_token(B2) of
                        {lbrace, _V2, B3} ->
                            write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", list, [  \n", Depth),
                            NBin = parse_list(B3, Field, Depth + 1),
                            write(Field, "]}, \n", Depth),
                            parse_class(NBin, Field, Depth);
                        {langular, _V2, B3} ->
                            write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", class, ", Depth),
                            NBin = parse_list(B3, Field, Depth + 1),
                            write(Field, "}, \n", 0),
                            parse_class(NBin, Field, Depth);
                        _ ->
                            ?EXITP("List define error")
                    end;
                {typeof, B2} ->
                    case next_token(B2) of
                        {langular, _V2, B3} ->
                            write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", class, ", Depth),
                            {Typeof, NBin} = parse_typeof(B3),
                            write(Field, Typeof ++ "}, \n", 0),
                            parse_class(NBin, Field, Depth);
                        _ ->
                            ?EXITP("Typeof define error")
                    end;
                {enum, B2} ->
                    case next_token(B2) of
                        {lbrace, _V2, B3} ->
                            write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", enum, [  \n", Depth),
                            NBin = parse_enum(B3, Field, Depth + 1),
                            write(Field, "\n", 0, -3),
                            write(Field, "]}, \n", Depth),
                            parse_class(NBin, Field, Depth);
                        _ ->
                            ?EXITP("Enum define error")
                    end;
                {FT, B2} ->
                    write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", " ++ ?A2L(FT) ++ "}, \n", Depth),
                    parse_class(B2, Field, Depth);
                _ ->
                    ?EXITP("Class field define error")
            end;
        {rbrace, _V1, B1} ->
            B1;
        _ ->
            ?EXITP("Class define error")
    end.
    
parse_list (Bin, Field, Depth) ->
    case next_token(Bin) of
        {colon, V1, B1} ->
            case field_type(B1) of
                {list, B2} ->
                    case next_token(B2) of
                        {lbrace, _V2, B3} ->
                            write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", list, [  \n", Depth),                            
                            NBin = parse_list(B3, Field, Depth + 1),
                            write(Field, "]}, \n", Depth),
                            parse_list(NBin, Field, Depth);
                        {langular, _V2, B3} ->
                            write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", list, ", Depth),
                            NBin = parse_list(B3, Field, Depth + 1),
                            write(Field, "}, \n", 0),
                            parse_list(NBin, Field, Depth);
                        _ ->
                            ?EXITP("List define error")
                    end;
                {typeof, B2} ->
                    case next_token(B2) of
                        {langular, _V2, B3} ->
                            write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", class, ", Depth),
                            {Typeof, NBin} = parse_typeof(B3),
                            write(Field, Typeof ++ "}, \n", 0),
                            parse_list(NBin, Field, Depth);
                        _ ->
                            ?EXITP("Typeof define error")
                    end;
                {enum, B2} ->
                    case next_token(B2) of
                        {lbrace, _V2, B3} ->
                            write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", enum, [  \n", Depth),
                            NBin = parse_enum(B3, Field, Depth + 1),
                            write(Field, "\n", 0, -3),
                            write(Field, "]}, \n", Depth),
                            parse_list(NBin, Field, Depth);
                        _ ->
                            ?EXITP("Enum define error")
                    end;
                {FT, B2} ->
                    write(Field, "{" ++ ?TLW(?B2L(V1)) ++ ", " ++ ?A2L(FT) ++ "}, \n", Depth),
                    parse_list(B2, Field, Depth);
                _ ->
                    ?EXITP("List field define error")
            end;
        {rbrace, _V1, B1} ->
            write(Field, "\n", 0, -3),
            B1;
        {rangular, V1, B1} ->
            write(Field, format_class_name(V1), 0),
            B1;
        _ ->
            ?EXITP("List define error")
    end.
    
parse_typeof (Bin) ->
    case next_token(Bin) of
        {rangular, V1, B1} ->
            {format_class_name(V1), B1};
        _ -> 
            ?EXITP("Typeof define error")
    end.
    
parse_enum (Bin, Field, Depth) ->
    case next_enum(Bin) of
        {rbrace, E, B} ->
            write(Field, "'" ++ ?B2L(E) ++ "', \n", Depth),
            B;
        {rbrace, B} ->
            B;
        {E, B} ->
            write(Field, "'" ++ ?B2L(E) ++ "', \n", Depth),
            parse_enum(B, Field, Depth);
        _ ->
            ?EXITP("Enum define error")
    end.
    
format_class_name (CN) ->
    case re:split(CN, "\\.") of
        [_N] -> 
            ?TLW(?B2L(CN));
        [_P, _N] -> 
            "'" ++ ?TLW(?B2L(CN)) ++ "'";
        _ -> 
            ?EXITG("Class name error")
    end.
    
next_token (Bin) ->
    next_token(Bin, <<>>).
    
%% bom header
next_token (<<239, 187, 191, Bin/binary>>, B) ->
    next_token(Bin, B);
%% //
next_token (<<$/, $/, Bin/binary>>, B) ->
    next_token(ignore_comment(Bin), B);
%% =
next_token (<<$=, Bin/binary>>, B) ->
    {equal, B, Bin};
%% :
next_token (<<$:, Bin/binary>>, B) ->
    {colon, B, Bin};
%% {
next_token (PBin = <<${, Bin/binary>>, B) ->
    case match_keyword(B) of
        nil -> {lbrace, B, Bin};
        KW -> {KW, B, PBin}
    end;
%% }
next_token (<<$}, Bin/binary>>, B) ->
    {rbrace, B, Bin};
%% <
next_token (<<$<, Bin/binary>>, B) ->
    {langular, B, Bin};
%% >
next_token (<<$>, Bin/binary>>, B) ->
    {rangular, B, Bin};
%% space
next_token (<<$ , Bin/binary>>, B) ->
    case match_keyword(B) of
        nil -> next_token(Bin, B);
        KW -> {KW, B, Bin}
    end;
%% \r
next_token (<<$\r, Bin/binary>>, B) ->
    case match_keyword(B) of
        nil -> next_token(Bin, B);
        KW -> {KW, B, Bin}
    end;  
%% \n
next_token (<<$\n, Bin/binary>>, B) ->
    ?SDF(line, (?GDF(line) + 1)),
    
    case match_keyword(B) of
        nil -> next_token(Bin, B);
        KW -> {KW, B, Bin}
    end;
%% \t
next_token (<<$\t, Bin/binary>>, B) ->
    case match_keyword(B) of
        nil -> next_token(Bin, B);
        KW -> {KW, B, Bin}
    end;
%% \0
next_token (<<$\0, Bin/binary>>, B) ->
    case match_keyword(B) of
        nil -> next_token(Bin, B);
        KW -> {KW, B, Bin}
    end;
%% character
next_token (<<C:1/binary, Bin/binary>>, B) ->
    next_token(Bin, <<B/binary, C:1/binary>>);
%% eof
next_token (<<>>, _) ->
    eof.
    
ignore_comment (<<>>) ->
    <<>>;
ignore_comment (<<$\r, Bin/binary>>) ->
    Bin;
ignore_comment (<<$\n, Bin/binary>>) ->
    ?SDF(line, (?GDF(line) + 1)),
    Bin;
ignore_comment (<<$\0, Bin/binary>>) ->
    Bin;
ignore_comment (<<_:1/binary, Bin/binary>>) ->
    ignore_comment(Bin).
    
match_keyword (<<$c, $l, $a, $s, $s>>) ->
    class;
match_keyword (<<$e, $n, $u, $m>>) ->
    enum;
match_keyword (<<$i, $n>>) ->
    in;
match_keyword (<<$o, $u, $t>>) ->
    out;
match_keyword (_) ->
    nil.
    
next_enum (Bin) ->
    next_enum(Bin, <<>>).
    
next_enum (<<$ , Bin/binary>>, E) ->
    case E of
        <<>> -> next_enum(Bin, E);
        _ -> {E, escape_line(Bin)}
    end;
next_enum (<<$=, Bin/binary>>, E) ->
    case E of
        <<>> -> next_enum(Bin, E);
        _ -> {E, escape_line(Bin)}
    end;
next_enum (<<$\r, Bin/binary>>, E) ->
    case E of
        <<>> -> next_enum(Bin, E);
        _ -> {E, escape_line(Bin)}
    end;
next_enum (<<$\n, Bin/binary>>, E) ->
    ?SDF(line, (?GDF(line) + 1)),
    
    case E of
        <<>> -> next_enum(Bin, E);
        _ -> {E, Bin}
    end;
next_enum (<<$\t, Bin/binary>>, E) ->
    case E of
        <<>> -> next_enum(Bin, E);
        _ -> {E, escape_line(Bin)}
    end;
next_enum (<<$/, $/, Bin/binary>>, E) ->
    case E of
        <<>> -> next_enum(ignore_comment(Bin), E);
        _ -> {E, ignore_comment(Bin)}
    end;
next_enum (<<$}, Bin/binary>>, E) ->
    case E of
        <<>> -> {rbrace, Bin};
        _ -> {rbrace, E, Bin}
    end;
next_enum (<<C:1/binary, Bin/binary>>, E) ->
    next_enum(Bin, <<E/binary, C:1/binary>>);
next_enum (<<>>, _) ->
    eof.
    
escape_line (<<>>) ->
    <<>>;
escape_line (<<$\n, Bin/binary>>) ->
    ?SDF(line, (?GDF(line) + 1)),
    Bin;
escape_line (Bin = <<$}, _/binary>>) ->
    Bin;
escape_line (<<_:1/binary, Bin/binary>>) ->
    escape_line(Bin).
    
field_type (<<>>) ->
    nil;
field_type (<<$b, $y, $t, $e, Bin/binary>>) ->
    case check_field(Bin, [<<$}>>]) of
        true -> {byte, Bin};
        _ -> nil
    end;
field_type (<<$s, $h, $o, $r, $t, Bin/binary>>) ->
    case check_field(Bin, [<<$}>>]) of
        true -> {short, Bin};
        _ -> nil
    end;
field_type (<<$i, $n, $t, Bin/binary>>) ->
    case check_field(Bin, [<<$}>>]) of
        true -> {int, Bin};
        _ -> nil
    end;
field_type (<<$l, $o, $n, $g, Bin/binary>>) ->
    case check_field(Bin, [<<$}>>]) of
        true -> {long, Bin};
        _ -> nil
    end;
field_type (<<$s, $t, $r, $i, $n, $g, Bin/binary>>) ->
    case check_field(Bin, [<<$}>>]) of
        true -> {string, Bin};
        _ -> nil
    end;
field_type (<<$l, $i, $s, $t, Bin/binary>>) ->
    case check_field(Bin, [<<${>>, <<$<>>]) of
        true -> {list, Bin};
        _ -> nil
    end;
field_type (<<$t, $y, $p, $e, $o, $f, Bin/binary>>) ->
    case check_field(Bin, [<<$<>>]) of
        true -> {typeof, Bin};
        _ -> nil
    end;
field_type (<<$e, $n, $u, $m, Bin/binary>>) ->
    case check_field(Bin, [<<${>>]) of
        true -> {enum, Bin};
        _ -> nil
    end;
field_type (<<_:1/binary, Bin/binary>>) ->
    field_type(Bin).
    
check_field (Bin, CL) ->
    case is_eow(Bin) of
        true ->
            true;
        _ ->
            <<C:1/binary, _/binary>> = Bin,
            lists:member(C, CL)
    end.
    
is_eow (<<>>) ->
    true;
is_eow (<<$/, $/, _/binary>>) ->
    true;
is_eow (<<$ , _/binary>>) ->
    true;
is_eow (<<$\t, _/binary>>) ->
    true;
is_eow (<<$\r, _/binary>>) ->
    true;
is_eow (<<$\n, _/binary>>) ->
    true;
is_eow (<<$\0, _/binary>>) ->
    true;
is_eow (<<_/binary>>) ->
    false.