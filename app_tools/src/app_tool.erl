-module(app_tool).

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
    ?MSG("Generate Protocol Files...~n"),
    init(),
    check_dir(get_default_dir(protocol_erl)),
    Doc = parse_doc(file_find(get_default_dir(protocol_erl), ".erl")),
    save_to_file(Doc, "Protocol.txt", false),
    generate_doc_file(parse_doc_complete(Doc)),
    clean(),
    ok;
run (2) ->
    ?MSG("Generate Database Files...~n"),
    Db = init_database(),
    save_to_file(Db, "DB.txt", false),
    generate_db_file(Db),
    generate_db_test(Db),
    ok;
run (_) ->
    run(1),
    run(2).
    
save_to_file (_, _, false) ->
    ok;
save_to_file (Data, File, _) ->
    {ok, Fd} = ?FOPEN(File, [write]),
    io:format(Fd, "~p", [Data]),
    ?FCLOSE(Fd).

check_dir (Dir) ->
    case filelib:is_dir(Dir) of
        false -> filelib:ensure_dir(Dir);
        true -> ok
    end.
    
get_default_dir (T) ->
    {T, Path} = lists:keyfind(T, 1, ?DIR_DEF),
    Path.
    
file_find (Dir, Suf) ->
    filelib:wildcard(Dir ++ "*" ++ Suf).
    
parse_doc (FileList) ->
    parse_file(FileList, #doc{}).
    
parse_doc_complete (Doc) when is_record(Doc, doc) ->
    Doc #doc{
        pro_list = lists:reverse(Doc #doc.pro_list)
    }.
    
parse_file ([], Doc) ->
    Doc;
parse_file ([File | Rest], Doc) ->
    ?MSG("==> ~p~n", [File]),
    
    parse_file(Rest, 
        Doc #doc{pro_list = [parse_file(File) | Doc #doc.pro_list]}
    ).
    
parse_file (File) ->
    init(),
    {ok, [Data]} = file:consult(File),
    Pro = parse_protocol(Data, #protocol{file = filename:basename(File)}),
    parse_file_complete(Pro).
    
parse_file_complete (Pro) when is_record(Pro, protocol) ->
    Pro #protocol{
        enum_list = lists:reverse(Pro #protocol.enum_list),
        list_list = lists:reverse(Pro #protocol.list_list),
        class_list = lists:reverse(Pro #protocol.class_list),
        action_list = lists:reverse(Pro #protocol.action_list)
    }.
    
parse_protocol (Data, Pro) when is_map(Data) ->
    #{name := Name, id := Id, action := Action, class := Class} = Data,
    Pro1 = Pro #protocol{name = ?A2L(Name), id = Id},
    Pro2 = parse_class(Class, parse_action(Action, Pro1)),
    Pro2 #protocol{enum_list = ?GDF(enum_list), list_list = ?GDF(list_list)}.
    
parse_action ([], Pro) ->
    Pro;
parse_action ([Data | Rest], Pro) ->
    #{name := Name, id := Id, in := In, out := Out} = Data,
        
    Act = #action{
        name = ?A2L(Name), 
        id = Id,
        field_in = parse_field_list(In),
        field_out = parse_field_list(Out)
    },
    
    parse_action(
        Rest, 
        Pro #protocol{
            action_list = [Act | Pro #protocol.action_list]
        }
    ).
    
parse_class ([], Pro) ->
    Pro;
parse_class ([Data | Rest], Pro) ->
    #{name := Name, base := Base, field := Field} = Data,
    
    Class = case Base of
        nil ->
            #class{
                name = ?A2L(Name),
                base = "",
                field = parse_field_list(Field)
            };
        _ ->
            BaseField = #field{
                name = ?A2L(Name) ++ "_parent",
                type = class,
                class = ?A2L(Base)
            },
            
            #class{
                name = ?A2L(Name),
                base = ?A2L(Base),
                field = [BaseField | parse_field_list(Field)]
            }
    end,
    
    parse_class(
        Rest,
        Pro #protocol{
            class_list = [Class | Pro #protocol.class_list]
        }
    ).
    
parse_field_list (Data) ->
    parse_field_list_complete(parse_field_list(Data, [])).
    
parse_field_list_complete (List) when is_list(List) ->
    lists:reverse(List).
    
parse_field_list ([], FieldList) ->
    FieldList;
parse_field_list ([Field | Rest], FieldList) ->
    parse_field_list(Rest, [parse_field(Field) | FieldList]).
    
parse_field ({Name, byte}) ->
    FName = ?A2L(Name),
    #field{name = FName, type = byte, format_name = format_name(FName)};
parse_field ({Name, short}) ->
    FName = ?A2L(Name),
    #field{name = FName, type = short, format_name = format_name(FName)};
parse_field ({Name, int}) ->
    FName = ?A2L(Name),
    #field{name = FName, type = int, format_name = format_name(FName)};
parse_field ({Name, long}) ->
    FName = ?A2L(Name),
    #field{name = FName, type = long, format_name = format_name(FName)};
parse_field ({Name, string}) ->
    FName = ?A2L(Name),
    #field{name = FName, type = string, format_name = format_name(FName)};
parse_field ({Name, enum, Data}) ->
    ok = insert_enum(Data),
    FName = ?A2L(Name),
    #field{name = FName, type = enum, format_name = format_name(FName)};
parse_field ({Name, list, Data}) ->
    LName = get_list_name(Name),
    FName = ?A2L(Name),
    insert_list(LName, Data),
    #field{name = LName, type = list, format_name = format_name(FName)};
parse_field ({Name, class, Data}) ->
    FName = ?A2L(Name),
    #field{name = FName, type = class, class = ?A2L(Data), format_name = format_name(FName)};
parse_field (_) ->
    exit(invalid_field).

init () ->
    ?SDF(list_counter, 0),
    ?SDF(enum_list, []),
    ?SDF(list_list, []).
    
clean () ->
    ?EDF(list_counter),
    ?EDF(enum_list),
    ?EDF(list_list).
    
insert_enum ([]) ->
    ok;
insert_enum ([Enum | Rest]) ->
    case ?GDF(enum_list) of
        [] ->
            ?SDF(enum_list, [#enum{name = ?A2L(Enum), val = 1}]);
        EL ->
            case lists:keyfind(?A2L(Enum), #enum.name, EL) of
                false ->
                    [Tl | _] = EL,

                    ?SDF(
                        enum_list, 
                        [#enum{name = ?A2L(Enum), val = Tl #enum.val + 1} | EL]
                    );
                _ ->
                    ok
            end
    end,
    
    insert_enum(Rest).
    
get_list_name (Name) when is_atom(Name) ->
    Id = ?SDF(list_counter, ?GDF(list_counter) + 1),
    ?A2L(Name) ++ "_list_" ++ ?I2L(Id);
get_list_name (Name) when is_list(Name) ->
    Id = ?SDF(list_counter, ?GDF(list_counter) + 1),
    Name ++ "_list_" ++ ?I2L(Id).
    
insert_list (Name, Data) when is_atom(Data) ->
    List = #list{name = Name, class = ?A2L(Data),
        field = [#field{name = Name, type = class, class = ?A2L(Data)}]
    },
    ?SDF(list_list, [List | ?GDF(list_list)]);
insert_list (Name, Data) when is_list(Data) ->
    List = #list{name = Name, field = parse_field_list(Data)},
    ?SDF(list_list, [List | ?GDF(list_list)]).
    
delete_dir_file (Dir, Suf) ->
    [ok = file:delete(F) || F <- file_find(Dir, Suf)].
    
field_desc (byte) ->
    {"8", "signed"};
field_desc (short) ->
    {"16", "signed"};
field_desc (int) ->
    {"32", "signed"};
field_desc (long) ->
    {"64", "signed"};
field_desc (enum) ->
    {"8", "unsigned"};
field_desc (_) ->
    nil.
    
generate_doc_file (Doc) ->
    generate_header(Doc),
    generate_out(Doc),
    generate_router(Doc),
    generate_api(Doc),
    generate_mod(Doc).
    
generate_header (Doc) ->
    Dir = get_default_dir(header),
    ok = filelib:ensure_dir(Dir),
    delete_dir_file(Dir, ".hrl"),
    generate_header(Doc #doc.pro_list, Dir).
    
generate_header ([], _) ->
    ok;
generate_header ([Pro | Left], Dir) ->
    case Pro #protocol.enum_list of
        [] -> 
            generate_header(Left, Dir);
        _ ->
            {ok, Fd} = ?FOPEN(
                Dir ++ "api_" ++ Pro #protocol.name ++ ".hrl", 
                [write]
            ),

            write_header_file(Fd, Pro),
            ?FCLOSE(Fd),
            generate_header(Left, Dir)
    end.
    
write_header_file (Fd, Pro) ->
    [
        ?FWRITE(Fd, "-define(" ++ ?TUP(E #enum.name) ++ ", " ++ 
                ?I2L(E #enum.val) ++ ").\n")
        || E <- Pro #protocol.enum_list
    ].
    
generate_out (Doc) ->
    Dir = get_default_dir(out),
    ok = filelib:ensure_dir(Dir),
    delete_dir_file(Dir, ".erl"),
    generate_out(Doc #doc.pro_list, Dir),
    generate_class(Doc, Dir),
    generate_list(Doc, Dir).
    
generate_out ([], _) ->
    ok;
generate_out ([Pro | Left], Dir) ->
    {ok, Fd} = ?FOPEN(
        Dir ++ "api_" ++ Pro #protocol.name ++ "_out.erl",
        [write]
    ),
    
    write_out_file(Fd, Pro),
    ?FCLOSE(Fd),
    generate_out(Left, Dir).
    
write_out_file (Fd, Pro) ->
    ?FWRITE(Fd, 
        "-module(api_" ++ Pro #protocol.name ++ "_out).\n\n"
            ++ "-export([ "
    ),
    
    [
        ?FWRITE(Fd, "\n\t" ++ A #action.name ++ "/1,")
        || A <- Pro #protocol.action_list
    ],
    
    ?FWRITE(Fd, "\n]).", -1),
    
    [
        begin
            ?FWRITE(Fd, "\n\n" ++ A #action.name ++ " ({ "),
            
            [
                ?FWRITE(Fd, "\n\t" ++ format_name(F #field.name) ++ ",")
                || F <- A #action.field_out
            ],
            
            ?FWRITE(Fd, "\n}) ->\n", -1),
            
            [
                begin
                    FN = format_name(F #field.name),
                    
                    case F #field.type of
                        list ->
                            ?FWRITE(Fd, "\tBinList_" ++ FN
                                ++ " = [\n\t\tapi_base_list_out:list_to_bin_" 
                                ++ Pro #protocol.name ++ "_"
                                ++ F #field.name ++ "(" ++ FN ++ "_Item) || "
                                ++ FN ++ "_Item <- " ++ FN ++ "\n\t],\n\n\t"
                                ++ FN ++ "_Len = length(" ++ FN ++ "),\n\t"
                                ++ "Bin_" ++ FN ++ " = list_to_binary("
                                ++ "BinList_" ++ FN ++ "),\n\n"
                            );
                        string ->
                            ?FWRITE(Fd, "\tBin_" ++ FN
                                ++ " = list_to_binary(" ++ FN ++ "),\n\t"
                                ++ "Bin_" ++ FN ++ "_Len = size("
                                ++ "Bin_" ++ FN ++ "),\n\n"
                            );
                        class ->
                            ?FWRITE(Fd, "\t" ++ FN 
                                ++ "_Bin = api_base_class_out:class_to_bin_" 
                                ++ class_name(F #field.class, Pro #protocol.name) 
                                ++ "(" ++ FN ++ "),\n\n"
                            );
                        _ ->
                            ok
                    end
                end
                || F <- A #action.field_out
            ],
            
            ?FWRITE(Fd, "\t<<\n\t\t" ++ ?I2L(Pro #protocol.id) 
                ++ ":8/unsigned,\n\t\t" ++ ?I2L(A #action.id) ++ ":8/unsigned,"
            ),
            
            [
                begin
                    FN = format_name(F #field.name),
                    
                    case F #field.type of
                        list ->
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ "_Len:16/unsigned, "
                                ++ "Bin_" ++ FN ++ "/binary,"
                            );
                        string ->
                            ?FWRITE(Fd, "\n\t\tBin_" ++ FN ++ "_Len:16/unsigned, "
                                ++ "Bin_" ++ FN ++ "/binary,"
                            );
                        class ->
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ "_Bin/binary,");
                        FT ->
                            {FLen, FDes} = field_desc(FT),
                            
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ ":"
                                ++ FLen ++ "/" ++ FDes ++ ","
                            )
                    end
                end
                || F <- A #action.field_out
            ],
            
            ?FWRITE(Fd, "\n\t>>.", -1)
        end
        || A <- Pro #protocol.action_list
    ].
    
generate_class (Doc, Dir) ->
    generate_class_in(Doc, Dir),
    generate_class_out(Doc, Dir).
    
generate_class_in (Doc, Dir) ->
    {ok, Fd} = ?FOPEN(Dir ++ "api_base_class_in.erl", [write]),
    ?FWRITE(Fd, "-module(api_base_class_in).\n\n-compile(export_all)."),
    write_class_in(Doc #doc.pro_list, Fd),
    ?FCLOSE(Fd).
    
write_class_in ([], _) ->
    ok;
write_class_in ([Pro | Left], Fd) ->
    [
        begin
            PN = Pro #protocol.name,
            
            ?FWRITE(Fd, "\n\nclass_parse_" 
                ++ class_name(C #class.name, PN) ++ " (_Args0) ->"
            ),
            
            AC = write_class_field(Fd, C, PN),
            write_class_item(Fd, C),
            
            ?FWRITE(Fd, "\n\t{_" ++ format_name(C #class.name)
                ++ "Item, _Args" ++ ?I2L(AC) ++ "}."
            )
        end
        || C <- Pro #protocol.class_list
    ],
    
    write_class_in(Left, Fd).
    
write_class_field (Fd, C, PN) ->
    write_class_field(C #class.field, [], 0, Fd, PN).

write_class_field ([], [], AC, _, _) ->
    AC;
write_class_field ([], _, AC, Fd, _) ->
    ?FWRITE(Fd, "_Args" ++ ?I2L(AC+1)
        ++ "/binary>> = _Args" ++ ?I2L(AC) ++ ","
    ),
    
    AC + 1;
write_class_field ([F | LF], PF, AC, Fd, PN) ->
    case PF of
        [] -> ?FWRITE(Fd, "\n\t<<");
        _ -> ok
    end,
        
    FN = format_name(F #field.name),
    
    case F #field.type of
        class ->
            ?FWRITE(Fd, 
                FN ++ "Bin/binary>> = _Args" ++ ?I2L(AC) ++ ","
            ),
            
            ?FWRITE(Fd, "\n\t{" ++ FN ++ ", _Args" 
                ++ ?I2L(AC+1) ++ "} = class_parse_" 
                ++ class_name(F #field.class, PN) ++ "("
                ++ FN ++ "Bin),"
            ),
            
            write_class_field(LF, [], AC + 1, Fd, PN);
        list ->
            ?FWRITE(Fd, "Size_" ++ FN ++ ":16/unsigned, "
                ++ FN ++ "Bin/binary>> = _Args" ++ ?I2L(AC) ++ ","
            ),
            
            ?FWRITE(Fd, "\n\t{" ++ FN ++ ", _Args" ++ ?I2L(AC+1)
                ++ "} = api_base_list_in:list_parse_" 
                ++ PN ++ "_" ++ F #field.name 
                ++ "(Size_" ++ FN ++ ", " ++ FN ++ "Bin, []),"
            ),
            
            write_class_field(LF, [], AC + 1, Fd, PN);
        string ->
            ?FWRITE(Fd, "Len_" ++ FN ++ ":16/unsigned, "
                ++ FN ++ ":" ++ "Len_" ++ FN ++ "/binary, "
            ),
            
            write_class_field(LF, [F | PF], AC, Fd, PN);
        FT ->
            {FLen, FDes} = field_desc(FT),
                            
            ?FWRITE(Fd, FN ++ ":" 
                ++ FLen ++ "/" ++ FDes ++ ", "
            ),
            
            write_class_field(LF, [F | PF], AC, Fd, PN)
    end.
    
write_class_item (Fd, C) ->
    ?FWRITE(Fd, "\n\t_" ++ format_name(C #class.name) ++ "Item = {"),
    
    [
        begin
            FN = format_name(F #field.name),
            
            case F #field.type of
                string ->
                    ?FWRITE(Fd, "binary_to_list(" ++ FN ++ "), ");
                _ ->
                    ?FWRITE(Fd, FN ++ ", ")
            end
        end
        || F <- C #class.field
    ],
      
    ?FWRITE(Fd, "},", -2).
    
generate_class_out (Doc, Dir) ->
    {ok, Fd} = ?FOPEN(Dir ++ "api_base_class_out.erl", [write]),
    ?FWRITE(Fd, "-module(api_base_class_out).\n\n-compile(export_all)."),
    write_class_out(Doc #doc.pro_list, Fd, Doc),
    ?FCLOSE(Fd).
    
write_class_out ([], _, _) ->
    ok;
write_class_out ([Pro | Left], Fd, Doc) ->
    [
        begin
            CN = class_name(C #class.name, Pro #protocol.name),
            ?FWRITE(Fd, "\n\nclass_to_bin_" ++ CN ++ " ({ "),
            
            [
                ?FWRITE(Fd, "\n\t" ++ format_name(F #field.name) ++ ",")
                || F <- C #class.field
            ],
            
            ?FWRITE(Fd, "\n}) ->\n", -1),
            
            [
                begin
                    FN = format_name(F #field.name),
                    
                    case F #field.type of
                        list ->
                            ?FWRITE(Fd, "\tBinList_" ++ FN
                                ++ " = [\n\t\tapi_base_list_out:list_to_bin_" 
                                ++ Pro #protocol.name ++ "_"
                                ++ F #field.name ++ "(" ++ FN ++ "_Item) || "
                                ++ FN ++ "_Item <- " ++ FN ++ "\n\t],\n\n\t"
                                ++ FN ++ "_Len = length(" ++ FN ++ "),\n\t"
                                ++ "Bin_" ++ FN ++ " = list_to_binary("
                                ++ "BinList_" ++ FN ++ "),\n\n"
                            );
                        string ->
                            ?FWRITE(Fd, "\tBin_" ++ FN
                                ++ " = list_to_binary(" ++ FN ++ "),\n\t"
                                ++ "Bin_" ++ FN ++ "_Len = size("
                                ++ "Bin_" ++ FN ++ "),\n\n"
                            );
                        class ->
                            check_class(F #field.class, Pro #protocol.name, Doc),
                            
                            ?FWRITE(Fd, "\t" ++ FN ++ "_Bin = class_to_bin_"
                                ++ class_name(F #field.class, Pro #protocol.name) 
                                ++ "(" ++ FN ++ "),\n\n"
                            );
                        _ ->
                            ok
                    end
                end
                || F <- C #class.field
            ],
            
            ?FWRITE(Fd, "\t<< "),
            
            [
                begin
                    FN = format_name(F #field.name),
                    
                    case F #field.type of
                        list ->
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ "_Len:16/unsigned, "
                                ++ "Bin_" ++ FN ++ "/binary,"
                            );
                        string ->
                            ?FWRITE(Fd, "\n\t\tBin_" ++ FN ++ "_Len:16/unsigned, "
                                ++ "Bin_" ++ FN ++ "/binary,"
                            );
                        class ->
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ "_Bin/binary,");
                        FT ->
                            {FLen, FDes} = field_desc(FT),
                            
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ ":"
                                ++ FLen ++ "/" ++ FDes ++ ","
                            )
                    end
                end
                || F <- C #class.field
            ],
            
            ?FWRITE(Fd, "\n\t>>.", -1)
        end
        || C <- Pro #protocol.class_list
    ],

    write_class_out(Left, Fd, Doc).
    
check_class (CN, PN, Doc) ->
    {NPN, NCN} = case re:split(CN, "\\.") of
        [N] -> 
            {PN, ?B2L(N)};
        [P, N] -> 
            {?B2L(P), ?B2L(N)};
        _ -> 
            ?EXITG("Class name error")
    end,
    
    case lists:keyfind(NPN, #protocol.name, Doc #doc.pro_list) of
        false ->
            ?EXITG("Protocol " ++ NPN ++ " not define");
        Pro ->
            case lists:keyfind(NCN, #class.name, Pro #protocol.class_list) of
                false -> 
                    ?EXITG("Class " ++ CN ++ " not define");
                _ ->
                    ok
            end
    end.
    
generate_list (Doc, Dir) ->
    generate_list_in(Doc, Dir),
    generate_list_out(Doc, Dir).
    
generate_list_in (Doc, Dir) ->
    {ok, Fd} = ?FOPEN(Dir ++ "api_base_list_in.erl", [write]),
    ?FWRITE(Fd, "-module(api_base_list_in).\n\n-compile(export_all)."),
    write_list_in(Doc #doc.pro_list, Fd),
    ?FCLOSE(Fd).
    
write_list_in ([], _) ->
    ok;
write_list_in ([Pro | Left], Fd) ->
    [
        begin
            PN = Pro #protocol.name,
            
            ?FWRITE(Fd, "\n\nlist_parse_" ++ PN ++ "_" ++ L #list.name  
                ++ " (0, _Args, _Result) ->\n\t{_Result, _Args};\n"
                ++ "list_parse_" ++ PN ++ "_" ++ L #list.name  
                ++ " (_Count, _Args0, _Result) ->"
            ),
            
            AC = write_list_field(Fd, L, PN),
            write_list_item(Fd, L),
            
            ?FWRITE(Fd, "\n\tlist_parse_" ++ PN ++ "_" ++ L #list.name
                ++ "(_Count - 1, _Args" ++ ?I2L(AC) ++ ", [_"
                ++ format_name(L #list.name) ++ "Item | _Result])."
            )
        end
        || L <- Pro #protocol.list_list
    ],
    
    write_list_in(Left, Fd).
    
write_list_field (Fd, L, PN) ->
    write_list_field(L #list.field, [], 0, Fd, PN).

write_list_field ([], [], AC, _, _) ->
    AC;
write_list_field ([], _, AC, Fd, _) ->
    ?FWRITE(Fd, "_Args" ++ ?I2L(AC+1) 
        ++ "/binary>> = _Args" ++ ?I2L(AC) ++ ","
    ),
    
    AC + 1;
write_list_field ([F | LF], PF, AC, Fd, PN) ->
    case PF of
        [] -> ?FWRITE(Fd, "\n\t<<");
        _ -> ok
    end,
        
    FN = format_name(F #field.name),
    
    case F #field.type of
        class ->
            ?FWRITE(Fd, 
                FN ++ "Bin/binary>> = _Args" ++ ?I2L(AC) ++ ","
            ),
            
            ?FWRITE(Fd, "\n\t{" ++ FN ++ ", _Args" 
                ++ ?I2L(AC+1) ++ "} = api_base_class_in:class_parse_" 
                ++ class_name(F #field.class, PN) ++ "("
                ++ FN ++ "Bin),"
            ),
            
            write_list_field(LF, [], AC + 1, Fd, PN);
        list ->
            ?FWRITE(Fd, "Size_" ++ FN ++ ":16/unsigned, "
                ++ FN ++ "Bin/binary>> = _Args" ++ ?I2L(AC) ++ ","
            ),
            
            ?FWRITE(Fd, "\n\t{" ++ FN ++ ", _Args" ++ ?I2L(AC+1)
                ++ "} = list_parse_" ++ PN ++ "_" ++ F #field.name 
                ++ "(Size_" ++ FN ++ ", " ++ FN ++ "Bin, []),"
            ),
            
            write_list_field(LF, [], AC + 1, Fd, PN);
        string ->
            ?FWRITE(Fd, "Len_" ++ FN ++ ":16/unsigned, "
                ++ FN ++ ":" ++ "Len_" ++ FN ++ "/binary, "
            ),
            
            write_list_field(LF, [F | PF], AC, Fd, PN);
        FT ->
            {FLen, FDes} = field_desc(FT),
                            
            ?FWRITE(Fd, FN ++ ":" 
                ++ FLen ++ "/" ++ FDes ++ ", "
            ),
            
            write_list_field(LF, [F | PF], AC, Fd, PN)
    end.
    
write_list_item (Fd, L) ->
    ?FWRITE(Fd, "\n\t_" ++ format_name(L #list.name) ++ "Item = {"),
    
    [
        begin
            FN = format_name(F #field.name),
            
            case F #field.type of
                string ->
                    ?FWRITE(Fd, "binary_to_list(" ++ FN ++ "), ");
                _ ->
                    ?FWRITE(Fd, FN ++ ", ")
            end
        end
        || F <- L #list.field
    ],
      
    ?FWRITE(Fd, "},", -2).
    
generate_list_out (Doc, Dir) ->
    {ok, Fd} = ?FOPEN(Dir ++ "api_base_list_out.erl", [write]),
    ?FWRITE(Fd, "-module(api_base_list_out).\n\n-compile(export_all)."),
    write_list_out(Doc #doc.pro_list, Fd),
    ?FCLOSE(Fd).
    
write_list_out ([], _) ->
    ok;
write_list_out ([Pro | Left], Fd) ->
    [
        begin
            case L #list.class of
                [] ->
                    ?FWRITE(Fd, "\n\nlist_to_bin_" 
                        ++ Pro #protocol.name ++ "_" ++ L #list.name ++ " ({ "
                    );
                _ ->
                    ?FWRITE(Fd, "\n\nlist_to_bin_" 
                        ++ Pro #protocol.name ++ "_" ++ L #list.name ++ " ( "
                    )
            end,
            
            [
                ?FWRITE(Fd, "\n\t" ++ format_name(F #field.name) ++ ",")
                || F <- L #list.field
            ],
            
            case L #list.class of
                [] ->
                    ?FWRITE(Fd, "\n}) ->\n", -1);
                _ ->
                    ?FWRITE(Fd, "\n) ->\n", -1)
            end,
            
            [
                begin
                    FN = format_name(F #field.name),
                    
                    case F #field.type of
                        list ->
                            ?FWRITE(Fd, "\tBinList_" ++ FN
                                ++ " = [\n\t\tlist_to_bin_" 
                                ++ Pro #protocol.name ++ "_"
                                ++ F #field.name ++ "(" ++ FN ++ "_Item) || "
                                ++ FN ++ "_Item <- " ++ FN ++ "\n\t],\n\n\t"
                                ++ FN ++ "_Len = length(" ++ FN ++ "),\n\t"
                                ++ "Bin_" ++ FN ++ " = list_to_binary("
                                ++ "BinList_" ++ FN ++ "),\n\n"
                            );
                        string ->
                            ?FWRITE(Fd, "\tBin_" ++ FN
                                ++ " = list_to_binary(" ++ FN ++ "),\n\t"
                                ++ "Bin_" ++ FN ++ "_Len = size("
                                ++ "Bin_" ++ FN ++ "),\n\n"
                            );
                        class ->
                            ?FWRITE(Fd, "\t" ++ FN 
                                ++ "_Bin = api_base_class_out:class_to_bin_"
                                ++ class_name(F #field.class, Pro #protocol.name) 
                                ++ "(" ++ FN ++ "),\n\n"
                            );
                        _ ->
                            ok
                    end
                end
                || F <- L #list.field
            ],
            
            ?FWRITE(Fd, "\t<< "),
            
            [
                begin
                    FN = format_name(F #field.name),
                    
                    case F #field.type of
                        list ->
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ "_Len:16/unsigned, "
                                ++ "Bin_" ++ FN ++ "/binary,"
                            );
                        string ->
                            ?FWRITE(Fd, "\n\t\tBin_" ++ FN ++ "_Len:16/unsigned, "
                                ++ "Bin_" ++ FN ++ "/binary,"
                            );
                        class ->
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ "_Bin/binary,");
                        FT ->
                            {FLen, FDes} = field_desc(FT),
                            
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ ":"
                                ++ FLen ++ "/" ++ FDes ++ ","
                            )
                    end
                end
                || F <- L #list.field
            ],
            
            ?FWRITE(Fd, "\n\t>>.", -1)
        end
        || L <- Pro #protocol.list_list
    ],
    
    write_list_out(Left, Fd).
    
generate_router (Doc) ->
    Dir = get_default_dir(out),
    ok = filelib:ensure_dir(Dir),
    {ok, Fd} = ?FOPEN(Dir ++ "game_router.erl", [write]),
    
    ?FWRITE(Fd, "-module(game_router).\n\n-export([route_request/2])."
        "\n\nroute_request "
        "(<<Module:8/unsigned, Action:8/unsigned, Args/binary>>, State) ->"
        "\n\t{_M, _A, NewState} = route_request(Module, Action, Args, State),"
        "\n\tNewState."
    ),
    
    [
        begin
            ?FWRITE(Fd, "\n\nroute_request (" ++ ?I2L(Pro #protocol.id) 
                ++ ", _Action, _Args0, _State) ->\n\tcase _Action of"
            ),
            
            [
                begin
                    ?FWRITE(Fd, "\n\t\t" ++ ?I2L(A #action.id) ++ " ->"),
                    
                    case length(A #action.field_in) of
                        0 ->
                            ok;
                        _ ->
                            write_action_field(Fd, A, Pro #protocol.name)
                    end,
                    
                    write_action_api(Fd, A, Pro #protocol.name),
                    
                    ?FWRITE(Fd, "\n\t\t\t{" ++ Pro #protocol.name 
                        ++ ", " ++ A #action.name ++ ", NewState};"
                    )
                end
                || A <- Pro #protocol.action_list
            ],
            
            ?FWRITE(Fd, "\n\tend;", -1)
        end
        || Pro <- Doc #doc.pro_list
    ],
    
    ?FWRITE(Fd, "\n\nroute_request (_, _, _, State) ->\n\t{nil, nil, State}."),
    ?FCLOSE(Fd).
    
write_action_field (Fd, A, PN) ->
    write_action_field(A #action.field_in, [], 0, Fd, PN).

write_action_field ([], [], _, _, _) ->
    ok;
write_action_field ([], _, AC, Fd, _) ->
    ?FWRITE(Fd, ">> = _Args" ++ ?I2L(AC) ++ ",", -2);
write_action_field ([F | LF], PF, AC, Fd, PN) ->
    case PF of
        [] -> ?FWRITE(Fd, "\n\t\t\t<<");
        _ -> ok
    end,
        
    FN = format_name(F #field.name),
    
    case F #field.type of
        class ->
            ?FWRITE(Fd, FN ++ "Bin/binary>> = _Args" ++ ?I2L(AC) ++ ","),
            
            ?FWRITE(Fd, "\n\t\t\t{" ++ FN ++ ", _Args" 
                ++ ?I2L(AC+1) ++ "} = api_base_class_in:class_parse_" 
                ++ class_name(F #field.class, PN) ++ "("
                ++ FN ++ "Bin),"
            ),
            
            write_action_field(LF, [], AC + 1, Fd, PN);
        list ->
            ?FWRITE(Fd, "Size_" ++ FN ++ ":16/unsigned, "
                ++ FN ++ "Bin/binary>> = _Args" ++ ?I2L(AC) ++ ","
            ),
            
            ?FWRITE(Fd, "\n\t\t\t{" ++ FN ++ ", _Args" ++ ?I2L(AC+1)
                ++ "} = api_base_list_in:list_parse_" 
                ++ PN ++ "_" ++ F #field.name 
                ++ "(Size_" ++ FN ++ ", " ++ FN ++ "Bin, []),"
            ),
            
            write_action_field(LF, [], AC + 1, Fd, PN);
        string ->
            ?FWRITE(Fd, "Len_" ++ FN ++ ":16/unsigned, "
                ++ FN ++ ":" ++ "Len_" ++ FN ++ "/binary, "
            ),
            
            write_action_field(LF, [F | PF], AC, Fd, PN);
        FT ->
            {FLen, FDes} = field_desc(FT),
                            
            ?FWRITE(Fd, FN ++ ":" 
                ++ FLen ++ "/" ++ FDes ++ ", "
            ),
            
            write_action_field(LF, [F | PF], AC, Fd, PN)
    end.
    
write_action_api (Fd, A, PN) ->
    ?FWRITE(Fd, "\n\t\t\tNewState = api_" 
        ++ PN ++ ":" ++ A #action.name ++ "("
    ),
    
    [
        begin
            FN = format_name(F #field.name),
            
            case F #field.type of
                string ->
                    ?FWRITE(Fd, "binary_to_list(" ++ FN ++ "), ");
                _ ->
                    ?FWRITE(Fd, FN ++ ", ")
            end
        end
        || F <- A #action.field_in
    ],
      
    ?FWRITE(Fd, "_State),").
    
generate_api (Doc) ->
    Dir = get_default_dir(api),
    ok = filelib:ensure_dir(Dir),
    % delete_dir_file(Dir, ".erl"),
    generate_api(Doc #doc.pro_list, Dir).
    
generate_api ([], _) ->
    ok;
generate_api ([Pro | Left], Dir) ->
    Filename = Dir ++ "api_" ++ Pro #protocol.name ++ ".erl",
    
    case filelib:file_size(Filename) of
        0 ->
            {ok, Fd} = ?FOPEN(Filename, [write]),
            
            ?FWRITE(Fd, "-module(api_" ++ Pro #protocol.name ++
                ").\n\n-export(["
            ),
            
            lists:foreach(
                fun(A) ->
                    ?FWRITE(Fd, "\n\t" ++ A #action.name ++ "/" ++ 
                        integer_to_list(length(A #action.field_in) + 1) ++
                        ","
                    )
                end,
                Pro #protocol.action_list
            ),
            
            ?FWRITE(Fd, "\n]).\n\n-include(\"app_server.hrl\").\n", -1),
            
            case length(Pro #protocol.enum_list) of
                0 ->
                    ok;
                _ ->
                    ?FWRITE(Fd, "-include(\"gen/api_" ++ 
                        Pro #protocol.name ++ ".hrl\").\n"
                    )
            end,
            
            lists:foreach(
                fun(A) ->
                    ?FWRITE(Fd, "\n\n" ++ A #action.name ++ " ("),
                    
                    lists:foreach(
                        fun(F) ->
                            ?FWRITE(Fd, "\n\t" ++ F #field.format_name ++
                                ","
                            )
                        end,
                        A #action.field_in
                    ),
                    
                    ?FWRITE(Fd, "\n\tState = #client_state{\n\t\t"
                        "player_id = _PlayerId, socket = Socket\n\t}\n) ->"
                        "\n\tResult = mod_" ++ Pro #protocol.name ++ ":" ++
                        A #action.name ++ "(_PlayerId, "
                    ),
                    
                    lists:foreach(
                        fun(F) ->
                            ?FWRITE(Fd, F #field.format_name ++ ", ")
                        end,
                        A #action.field_in
                    ),
                    
                    ?FWRITE(Fd, "),\n\tOutBin = api_" ++ Pro #protocol.name ++
                        ":" ++ A #action.name ++ "(Result),\n\t"
                        "lib_misc:tcp_send(Socket, OutBin),\n\tState.", -2
                    )
                end,
                Pro #protocol.action_list
            ),
            
            ?FCLOSE(Fd);
        _ ->
            ok
    end,

    generate_api(Left, Dir).
    
generate_mod (Doc) ->
    Dir = get_default_dir(mod),
    ok = filelib:ensure_dir(Dir),
    % delete_dir_file(Dir, ".erl"),
    generate_mod(Doc #doc.pro_list, Dir).
    
generate_mod ([], _) ->
    ok;
generate_mod ([Pro | Left], Dir) ->
    Filename = Dir ++ "mod_" ++ Pro #protocol.name ++ ".erl",
    
    case filelib:file_size(Filename) of
        0 ->
            {ok, Fd} = ?FOPEN(Filename, [write]),
            
            ?FWRITE(Fd, "-module(mod_" ++ Pro #protocol.name ++
                ").\n\n-export(["
            ),
            
            lists:foreach(
                fun(A) ->
                    ?FWRITE(Fd, "\n\t" ++ A #action.name ++ "/" ++ 
                        integer_to_list(length(A #action.field_in) + 1) ++
                        ","
                    )
                end,
                Pro #protocol.action_list
            ),
            
            ?FWRITE(Fd, "\n]).\n\n-include(\"app_server.hrl\").\n"
                "-include(\"gen/game_db.hrl\").\n", -1
            ),
            
            case length(Pro #protocol.enum_list) of
                0 ->
                    ok;
                _ ->
                    ?FWRITE(Fd, "-include(\"gen/api_" ++ 
                        Pro #protocol.name ++ ".hrl\").\n"
                    )
            end,
            
            lists:foreach(
                fun(A) ->
                    ?FWRITE(Fd, "\n\n" ++ A #action.name ++ 
                        " (\n\t_PlayerId,"
                    ),
                    
                    lists:foreach(
                        fun(F) ->
                            ?FWRITE(Fd, "\n\t__" ++ F #field.format_name ++
                                ","
                            )
                        end,
                        A #action.field_in
                    ),
                    
                    ?FWRITE(Fd, "\n) ->\n\tok.", -1)
                end,
                Pro #protocol.action_list
            ),
            
            ?FCLOSE(Fd);
        _ ->
            ok
    end,
    
    generate_mod(Left, Dir).
        
format_name ([]) ->
    [];
format_name (FN) ->
    [C | L] = FN,
    
    case (C >= $a andalso C =< $z) of
        true -> 
            format_name(L, [C - 32], C);
        _ -> 
            case (C >= $A andalso C =< $Z) of
                true ->
                    format_name(L, [C], C);
                _ ->
                    ?EXITG("Field name " ++ FN ++ " error")
            end
    end.
        
format_name ([], FN, _) ->
    lists:reverse(FN);
format_name ([C | L], FN, $_) ->
    case (C >= $a andalso C =< $z) of
        true -> 
            format_name(L, [C - 32 | FN], C);
        _ ->
            format_name(L, [C | FN], C)
    end;
format_name ([C | L], FN, _) ->
    format_name(L, [C | FN], C).
    
class_name (CN, PN) ->
    case re:split(CN, "\\.") of
        [N] -> 
            PN ++ "_" ++ ?B2L(N);
        [P, N] -> 
            ?B2L(P) ++ "_" ++ ?B2L(N);
        _ -> 
            ?EXITG("Class name error")
    end.
    
init_database () ->
    mysql:start_link(db, ?DB_SRV, ?DB_USR, ?DB_PSW, "information_schema"),
    mysql:fetch(db, <<"SET NAMES 'utf8'">>),
    
    {data, R} = mysql:fetch(db, 
        <<"SELECT `TABLE_NAME` FROM `TABLES` WHERE `TABLE_SCHEMA` = '", 
            ?DB_NAME, "'">>
    ),
    
    Rows = lib_mysql:get_rows(R),
    init_table_column(init_table_name(Rows)).
    
init_table_name (Rows) ->
    init_table_name(Rows, #db{}).
    
init_table_name ([], Db) ->
    Db;
init_table_name ([R | L], Db) ->
    case lists:keyfind(<<"TABLE_NAME">>, 1, R) of
        {<<"TABLE_NAME">>, <<"db_version">>} ->
             init_table_name(L, Db);
        {<<"TABLE_NAME">>, N} ->
            init_table_name(L,
                Db #db{table_list = 
                    [#table{name = ?B2L(N)} | Db #db.table_list]}
            );
        _ ->
            init_table_name(L, Db)
    end.
    
init_table_column (Db) ->
    init_table_column(Db #db.table_list, #db{}).
      
init_table_column ([], Db) ->
    Db;
init_table_column ([T | L], Db) ->
    {data, R} = mysql:fetch(db,
        ?L2B("SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_KEY` ,"
            " `EXTRA`, `COLUMN_COMMENT` FROM `COLUMNS` WHERE `TABLE_SCHEMA` = '"
            ++ ?DB_NAME ++ "' AND `TABLE_NAME` = '" ++ T #table.name ++ "'")
    ),

    Rows = lib_mysql:get_rows(R),

    init_table_column(L, Db #db{
        table_list = [set_table_column_complete(
            set_table_column(Rows, #table{name = T #table.name}))
            | Db #db.table_list]
    }).
     
set_table_column ([], T) ->
    T;
set_table_column ([CL | L], T) ->
    set_table_column(L, T #table{
        column = [init_column_field(CL) | T #table.column]}
    ).
    
set_table_column_complete (T) ->
    T #table{column = lists:reverse(T #table.column)}.
    
init_column_field (CL) ->
    init_column_field(CL, #column{}).
    
init_column_field ([], C) ->
    C #column{format_name = format_name(C #column.name)};
init_column_field ([{FT, FV} | L], C) ->
    case FT of
        <<"COLUMN_NAME">> ->
            init_column_field(L, C #column{name = field_val(FV)});
        <<"COLUMN_DEFAULT">> ->
            init_column_field(L, C #column{val = field_val(FV)});
        <<"DATA_TYPE">> ->
            init_column_field(L, C #column{type = field_val(FV)});
        <<"COLUMN_KEY">> ->
            init_column_field(L, C #column{key = field_val(FV)});
        <<"EXTRA">> ->
            init_column_field(L, C #column{extra = field_val(FV)});
        <<"COLUMN_COMMENT">> ->
            init_column_field(L, C #column{comment = field_val(FV)})
    end.
    
field_val (V) when is_binary(V) ->
    ?B2L(V);
field_val (V) ->
    V.
    
generate_db_file (Db) ->
    generate_db_header(Db),
    generate_db_init(Db),
    generate_db_save(Db),
    generate_db_dump(Db),
    generate_db_game(Db),
    generate_db_sync(Db).
    
generate_db_header (Db) ->
    Dir = get_default_dir(header),
    {ok, Fd} = ?FOPEN(Dir ++ "game_db.hrl", [write]),
    {ok, [MacroList]} = file:consult(?DB_HRL),
    mysql:fetch(db, <<"USE ", ?DB_NAME>>),
    write_db_macro(MacroList, Fd),
    write_db_record(Db #db.table_list, Fd),
    ?FCLOSE(Fd).
    
write_db_record ([], _) ->
    ok;
write_db_record ([T | L], Fd) ->
    ?FWRITE(Fd, "-record(" ++ T #table.name ++ ", {\n\trow_key, "),
    
    [
        begin
            ?FWRITE(Fd, "\n\t" ++ C #column.name),
            
            case C #column.val of
                undefined -> 
                    ?FWRITE(Fd, " = null,");
                [] ->
                    ?FWRITE(Fd, " = \"\",");
                V ->
                    ?FWRITE(Fd, " = " ++ V ++ ",")
            end,
            
            case C #column.comment of
                [] ->
                    ok;
                Com ->?FWRITE(Fd, " %% " ++ Com)
            end
        end
        || C <- T #table.column
    ],
    
    ?FWRITE(Fd, "\n\trow_ver = 0\n}).\n-record(pk_"
        ++ T #table.name ++ ", { "
    ),
    
    [
        begin
            ?FWRITE(Fd, "\n\t" ++ C #column.name ++ ",")
        end
        || C <- table_primary_key(T)
    ],
    
    ?FWRITE(Fd, "\n}).\n\n", -1),
    write_db_record(L, Fd).
    
write_db_macro ([], _) ->
    ok;
write_db_macro ([M | L], Fd) ->
    #{table := Table, id := Id, sign := Sign, name := Name, prefix := Prefix} = M,
    
    {data, R} = mysql:fetch(db, ?L2B("SELECT `" ++ Id ++ "`, `" ++ Sign ++ 
        "`, `" ++ Name ++ "` FROM " ++ Table)
    ),
    
    Rows = lib_mysql:get_rows(R),
    
    [
        begin
            ?FWRITE(Fd, "-define(" ++ Prefix
                ++ ?TUP(?B2L(lib_mysql:get_field(Row, <<"sign">>)))
                ++ ", " ++ ?I2L(lib_mysql:get_field(Row, <<"id">>))
                ++ "). %% " 
                ++ ?B2L(lib_mysql:get_field(Row, <<"name">>)) ++ "\n"
            )
        end
        || Row <- Rows
    ],
    
    ?FWRITE(Fd, "\n"),
    write_db_macro(L, Fd).
    
generate_db_init (Db) ->
    Dir = get_default_dir(out),
    {ok, Fd} = ?FOPEN(Dir ++ "game_db_init.erl", [write]),
    
    ?FWRITE(Fd, "-module(game_db_init).\n\n-export([\n\t"
        "init/0\n]).\n\n-include(\"gen/game_db.hrl\").\n\n"
        "init () ->\n\t"
        "mysql:fetch(gamedb, [<<\"SET FOREIGN_KEY_CHECKS=0;\">>]),\n\t"
        "ets:new(auto_increment, [public, set, named_table]),\n"
    ),
    
    [
        begin
            ?FWRITE(Fd, "\n\tinit(" ++ T #table.name ++ "),")
        end
        ||
        T <- Db #db.table_list
    ],
    
    ?FWRITE(Fd, "\n\n\tok."),
    write_db_init(Db, Fd),
    write_db_load(Db, Fd),
    ?FCLOSE(Fd).
    
write_db_init (Db, Fd) ->
    write_table_init(Db #db.table_list, Fd).
    
write_table_init ([], Fd) ->
    ?FWRITE(Fd, ".", -1);
write_table_init ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\ninit (" ++ T #table.name ++ ") ->"),
    
    case table_auto_increment(T) of
        {true, C} ->
            ?FWRITE(Fd, "\n\t{data, AutoIncResultId} = mysql:fetch("
                "gamedb, [<<\n\t\t\"SELECT IFNULL(MAX(`"
                ++ C #column.name ++ "`), 0) AS `max_id` FROM `"
                ++ T #table.name ++ "`;\"\n\t>>]),"
            ),
            
            ?FWRITE(Fd, "\n\n\t[AutoIncResult] = lib_mysql:get_rows("
                "AutoIncResultId),\n\t{<<\"max_id\">>, AutoIncStart} = "
                "lists:keyfind(<<\"max_id\">>, 1, AutoIncResult),"
                "\n\ttrue = ets:insert_new(auto_increment, {{"
                ++ T #table.name ++ ", " ++ C #column.name 
                ++ "}, AutoIncStart}),"
            );
        _ ->
            ok
    end,
    
    case table_write_only(T) of
        false ->
            case table_need_split(T) of
                {true, _} ->
                    ?FWRITE(Fd, "\n\t[ets:new(list_to_atom(\"t_" 
                        ++ T #table.name ++ "_\" ++ integer_to_list(I)), "
                        "[public, set, named_table, {keypos, 2}]) || "
                        "I <- lists:seq(0, 99)],"
                    );
                _ ->
                    ?FWRITE(Fd, "\n\tets:new(t_" ++ T #table.name
                        ++ ", [public, set, named_table, {keypos, 2}]),"
                    )
            end,
            
            ?FWRITE(Fd, "\n\tload(" ++ T #table.name ++ "),");
        _ ->
            ok
    end,
    
    ?FWRITE(Fd, "\n\tok;"),
    write_table_init(L, Fd).
    
write_db_load (Db, Fd) ->
    write_table_load(
        [T || T <- Db #db.table_list, table_write_only(T) =:= false],
        Fd
    ).
    
write_table_load ([], Fd) ->
    ?FWRITE(Fd, ".", -1);
write_table_load ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\nload (" ++ T #table.name ++ ") ->"
        "\n\tio:format(\"Loading ==> " ++ T #table.name ++ "~n\"),"
        "\n\t{data, NumResultId} = mysql:fetch(gamedb, [<<\""
        "SELECT COUNT(1) AS `num` FROM `" ++ T #table.name ++ 
        "`\">>]),\n\t[NumberResult] = lib_mysql:get_rows(NumResultId),"
        "\n\t{<<\"num\">>, RecordNumber} = lists:keyfind(<<\"num\">>,"
        " 1, NumberResult),"
    ),
    
    ?FWRITE(Fd, "\n\n\tlists:foreach(fun(Page) ->\n\t\t"
        "Sql = \"SELECT * FROM `" ++ T #table.name ++ "` LIMIT \""
        " ++ integer_to_list((Page - 1) * 100000) ++ \", 100000\",\n\t\t"
        "{data, ResultId} = mysql:fetch(gamedb, [list_to_binary(Sql)]),"
        "\n\t\tRows = lib_mysql:get_rows(ResultId),\n\n\t\t"
    ),
    
    ?FWRITE(Fd, "lists:foreach(\n\t\t\tfun(Row) ->"),
    
    [
        begin
            ?FWRITE(Fd, "\n\t\t\t\t{<<\"" ++ C #column.name ++ "\">>, "
                ++ C #column.format_name ++ "} = lists:keyfind(<<\""
                ++ C #column.name ++ "\">>, 1, Row),"
            )
        end
        || C <- T #table.column
    ],
    
    ?FWRITE(Fd, "\n\n\t\t\t\tRecord = #" ++ T #table.name ++ "{"
        ++ "\n\t\t\t\t\trow_key = {"
    ),
    
    [
        begin
            case C #column.type of
                "varchar" ->
                    ?FWRITE(Fd, "lib_mysql:mysql_binary_to_list("
                        ++ C #column.format_name ++ "), "
                    );
                _ ->
                    ?FWRITE(Fd, C #column.format_name ++ ", ")
            end
        end
        || C <- table_primary_key(T)
    ],
    
    ?FWRITE(Fd, "},", -2),
    
    [
        begin
            ?FWRITE(Fd, "\n\t\t\t\t\t" ++ C #column.name ++ " = "),

            case C #column.type of
                "varchar" ->
                    ?FWRITE(Fd, "lib_mysql:mysql_binary_to_list("
                        ++ C #column.format_name ++ "),"
                    );
                _ ->
                    ?FWRITE(Fd, C #column.format_name ++ ",")
            end
        end
        || C <- T #table.column
    ],
    
    ?FWRITE(Fd, "\n\t\t\t\t},", -1),
    
    case table_need_split(T) of
        {true, SC} ->
            ?FWRITE(Fd, "\n\n\t\t\t\tTabId = integer_to_list(("
                "Record #" ++ T #table.name ++ "."
                ++ SC #column.name ++ ") rem 100),\n\t\t\t\t"
                "EtsTab = list_to_atom(\"t_" ++ T #table.name
                ++ "_\" ++ TabId),\n\t\t\t\t"
                "ets:insert(EtsTab, Record)"
            );
        _ ->
            ?FWRITE(Fd, "\n\n\t\t\t\tets:insert(t_" 
                ++ T #table.name ++ ", Record)"
            )
    end,
    
    ?FWRITE(Fd, "\n\t\t\tend,\n\t\t\tRows\n\t\t) end,\n\t\t"
        "lists:seq(1, lib_misc:ceil(RecordNumber / 100000))\n\t);"
    ),
    
    write_table_load(L, Fd).
    
generate_db_save (Db) ->
    Dir = get_default_dir(out),
    {ok, Fd} = ?FOPEN(Dir ++ "game_db_save.erl", [write]),
    
    ?FWRITE(Fd, "-module(game_db_save).\n\n-export([run/6]).\n\n"
        "-include(\"gen/game_db.hrl\").\n\n"
        "run (Host, Port, User, Password, Database, OnlyPlayerTable) ->\n"
        "\t{ok, Pid} = mysql_conn:start(Host, Port, User, Password, Database, "
        "fun(_, M, A) -> io:format(M, A) end),\n\n"
        "\tmysql_conn:fetch(Pid, [<<\"/*!40101 SET NAMES utf8 */;\\n\">>], self()),\n"
        "\tmysql_conn:fetch(Pid, [<<\"/*!40101 SET SQL_MODE=''*/;\\n\">>], self()),\n"
        "\tmysql_conn:fetch(Pid, [<<\"/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS,"
        "UNIQUE_CHECKS=0 */;\\n\">>], self()),\n"
        "\tmysql_conn:fetch(Pid, [<<\"/*!40014 SET @OLD_FOREIGN_KEY_CHECKS="
        "@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;\\n\">>], self()),\n"
        "\tmysql_conn:fetch(Pid, [<<\"/*!40101 SET @OLD_SQL_MODE=@@"
        "SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;\\n\">>], self()),\n"
        "\tmysql_conn:fetch(Pid, [<<\"/*!40111 SET @OLD_SQL_NOTES=@@"
        "SQL_NOTES, SQL_NOTES=0 */;\\n\\n\">>], self()),\n\n"
    ),
    
    TableList = [T || T <- Db #db.table_list, table_write_only(T) =:= false],
    write_save_dump(TableList, Fd),
    ?FWRITE(Fd, "\n\tmysql_conn:stop(Pid),\n\tok."),
    write_table_dump_save(TableList, Fd),
    
    ?FWRITE(Fd, "\n\nlst_to_bin (null) ->\n\t<<\"NULL\">>;"
        "\nlst_to_bin (List) ->\n\tList2 = escape_str(List, []),"
        "\n\tBin = list_to_binary(List2),"
        "\n\t<<\"'\", Bin/binary, \"'\">>."
        "\n\nint_to_bin (null) ->\n\t<<\"NULL\">>;"
        "\nint_to_bin (Value) ->\n\tlist_to_binary(integer_to_list(Value))."
        "\n\nrel_to_bin (null) ->\n\t<<\"NULL\">>;"
        "\nrel_to_bin (Value) when is_integer(Value) ->"
        "\n\tlist_to_binary(integer_to_list(Value));"
        "\nrel_to_bin (Value) ->\n\tlist_to_binary(float_to_list(Value))."
        "\n\nescape_str ([], Result) ->\n\tlists:reverse(Result);"
        "\nescape_str ([$' | String], Result) ->"
        "\n\tescape_str(String, [$' | [$\\\\ | Result]]);"
        "\nescape_str ([$\" | String], Result) ->"
        "\n\tescape_str(String, [$\" | [$\\\\ | Result]]);"
        "\nescape_str ([$\\\\ | String], Result) ->"
        "\n\tescape_str(String, [$\\ | [$\\\\ | Result]]);"
        "\nescape_str ([$\\n | String], Result) ->"
        "\n\tescape_str(String, [$n | [$\\\\ | Result]]);"
        "\nescape_str ([$\\r | String], Result) ->"
        "\n\tescape_str(String, [$r | [$\\\\ | Result]]);"
        "\nescape_str ([Char | String], Result) ->"
        "\n\tescape_str(String, [Char | Result])."
    ),
    
    ?FCLOSE(Fd).
    
write_save_dump ([], _) ->
    ok;
write_save_dump ([T | L], Fd) ->
    case re:run(T #table.name, "^player_") of
        {match, _} ->
            ?FWRITE(Fd, "\tdump_" ++ T #table.name ++ "(Pid),\n");
        _ ->
            ?FWRITE(Fd, "\tif OnlyPlayerTable -> ok; true -> dump_"
                ++ T #table.name ++ "(Pid) end,\n"
            )
    end,
    
    write_save_dump(L, Fd).
    
write_table_dump_save ([], _) ->
    ok;
write_table_dump_save ([T | L], Fd) ->
    TableName = T #table.name,
    
    ?FWRITE(Fd, "\n\ndump_" ++ T #table.name ++ " (Pid) ->" 
        "\n\tmysql_conn:fetch(Pid, [<<\"DELETE FROM `" ++ 
        TableName ++ "`;\\n\\n\">>], self()),"
    ),
 
    case table_need_split(T) of
        {true, _} ->
            ?FWRITE(Fd, "\n\tSize = lists:foldl(fun(I, S)-> S + "
                "ets:info(list_to_atom(\"t_" ++ TableName ++ "_\""
                " ++ integer_to_list(I)), size) end, 0, lists:seq(0, 99)),"
            ),
            
            ?FWRITE(Fd, "\n\n\tlists:foldl(fun(I, {S1, N1, L1}) ->"
                "\n\t\tets:foldl(fun(Record, {S, N, L}) ->"
            ),
            
            lists:foreach(
                fun(C) ->
                    ?FWRITE(Fd, "\n\t\t\t_" ++ C #column.format_name ++ " = " ++ 
                        get_trans_by_column(C) ++ "(Record #" ++ 
                        TableName ++ "." ++ C #column.name ++ "),"
                    )
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\n\n\t\t\tLast = if N == 100 orelse S + 1 == Size ->"
                " <<\");\\n\\n\">>; true -> <<\"),\\n\">> end,"
                "\n\n\t\t\tL2 = [<<\"(\","
            ),
            
            lists:foreach(
                fun(C) ->
                    ?FWRITE(Fd, "\n\t\t\t\t_" ++ C #column.format_name ++ "/binary, \",\",")
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\n\t\t\t\tLast/binary\n\t\t\t>> | L],"
                "\n\n\t\t\tif N == 100 orelse S + 1 == Size ->"
                "\n\t\t\t\tmysql_conn:fetch(Pid, [<<\"INSERT IGNORE INTO `" ++
                TableName ++ "` (\"", -4
            ),
            
            lists:foreach(
                fun(C) ->
                    ?FWRITE(Fd, "\n\t\t\t\t\t\"`" ++ C #column.name ++ "`, \"")
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\"\n\t\t\t\t\t\") VALUES \">> | lists:reverse(L2)], "
                "self()),\n\n\t\t\t\t{S + 1, 0, []};\n\t\t\ttrue ->"
                "\n\t\t\t\t{S + 1, N + 1, L2}\n\t\t\tend"
                "\n\t\tend, {S1, N1, L1}, list_to_atom(\"t_" ++ 
                TableName ++ "\" ++ integer_to_list(I)))"
                "\n\tend, {0, 0, []}, lists:seq(0, 99)),\n\tok.", -3
            );
        _ ->
            ?FWRITE(Fd, "\n\tSize = ets:info(t_" ++ T #table.name ++ 
                ", size),"
            ),
            
            ?FWRITE(Fd, "\n\n\tets:foldl(fun(Record, {S, N, L}) ->"),
            
            lists:foreach(
                fun(C) ->
                    ?FWRITE(Fd, "\n\t\t_" ++ C #column.format_name ++ " = " ++ 
                        get_trans_by_column(C) ++
                        "(Record #" ++ TableName ++ "." ++ C #column.name ++ "),"
                    )
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\n\n\t\tLast = if N == 100 orelse S + 1 == Size ->"
                " <<\");\\n\\n\">>; true -> <<\"),\\n\">> end,"
                "\n\n\t\tL2 = [<<\"(\","
            ),
            
            lists:foreach(
                fun(C) ->
                    ?FWRITE(Fd, "\n\t\t\t_" ++ C #column.format_name ++ "/binary, \",\",")
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\n\t\t\tLast/binary\n\t\t>> | L],"
                "\n\n\t\tif N == 100 orelse S + 1 == Size ->"
                "\n\t\t\tmysql_conn:fetch(Pid, [<<\"INSERT IGNORE INTO `" ++
                TableName ++ "` (\"", -4
            ),
            
            lists:foreach(
                fun(C) ->
                    ?FWRITE(Fd, "\n\t\t\t\t\"`" ++ C #column.name ++ "`, \"")
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\"\n\t\t\t\t\") VALUES \">> | lists:reverse(L2)], "
                "self()),\n\n\t\t\t{S + 1, 0, []};\n\t\ttrue ->"
                "\n\t\t\t{S + 1, N + 1, L2}\n\t\tend"
                "\n\tend, {0, 0, []}, t_" ++ TableName ++
                "),\n\tok.", -3
            )
    end,
    
    write_table_dump_save(L, Fd).
    
generate_db_dump (Db) ->
    Dir = get_default_dir(out),
    {ok, Fd} = ?FOPEN(Dir ++ "game_db_dump.erl", [write]),
    TableList = [T || T <- Db #db.table_list, table_write_only(T) =:= false],
    
    ?FWRITE(Fd, "-module(game_db_dump)."
        "\n\n-export([\n\trun/0, \n\tbackup/0\n])."
        "\n\n-include(\"gen/game_db.hrl\")."
        "\n\nrun () ->"
        "\n\t%% supervisor:terminate_child(game, socket_server_sup),"
        "\n\t%% mod_online:wait_all_online_player_exit(15),"
        "\n\n\tFileName = \"./game_db.sql\","
        "\n\n\tcase filelib:is_file(FileName) of"
        "\n\t\ttrue -> ok;\n\t\tfalse -> ok = filelib:ensure_dir(FileName)"
        "\n\tend,\n\n\t{ok, File} = file:open(FileName, [write, raw]),"
        "\n\n\tok = file:write(File, <<\"/*!40101 SET NAMES utf8 */;\\n\">>),"
        "\n\tok = file:write(File, <<\"/*!40101 SET SQL_MODE=''*/;\\n\">>),"
        "\n\tok = file:write(File, <<\"/*!40014 SET @OLD_UNIQUE_CHECKS"
        "=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;\\n\">>),"
        "\n\tok = file:write(File, <<\"/*!40014 SET @OLD_FOREIGN_KEY_CHECKS"
        "=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;\\n\">>),"
        "\n\tok = file:write(File, <<\"/*!40101 SET @OLD_SQL_MODE="
        "@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;\\n\">>),"
        "\n\tok = file:write(File, <<\"/*!40111 SET @OLD_SQL_NOTES="
        "@@SQL_NOTES, SQL_NOTES=0 */;\\n\\n\">>),\n"
    ),
    
    lists:foreach(
        fun(T) ->
            ?FWRITE(Fd, "\n\tdump_" ++ T #table.name ++ "(File),")
        end,
        TableList
    ),
    
    ?FWRITE(Fd, "\n\n\tok = file:close(File),\n\tok."
        "\n\nbackup () ->"
        "\n\tFileName = \"./game_db_backup.sql\","
        "\n\n\tcase filelib:is_file(FileName) of"
        "\n\t\ttrue -> ok;\n\t\tfalse -> ok = filelib:ensure_dir(FileName)"
        "\n\tend,\n\n\t{ok, File} = file:open(FileName, [write, raw]),"
        "\n\n\tok = file:write(File, <<\"/*!40101 SET NAMES utf8 */;\\n\">>),"
        "\n\tok = file:write(File, <<\"/*!40101 SET SQL_MODE=''*/;\\n\">>),"
        "\n\tok = file:write(File, <<\"/*!40014 SET @OLD_UNIQUE_CHECKS"
        "=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;\\n\">>),"
        "\n\tok = file:write(File, <<\"/*!40014 SET @OLD_FOREIGN_KEY_CHECKS"
        "=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;\\n\">>),"
        "\n\tok = file:write(File, <<\"/*!40101 SET @OLD_SQL_MODE="
        "@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;\\n\">>),"
        "\n\tok = file:write(File, <<\"/*!40111 SET @OLD_SQL_NOTES="
        "@@SQL_NOTES, SQL_NOTES=0 */;\\n\\n\">>),\n"
    ),
    
    lists:foreach(
        fun(T) ->
            ?FWRITE(Fd, "\n\tdump_" ++ T #table.name ++ "(File),")
        end,
        TableList
    ),
    
    ?FWRITE(Fd, "\n\n\tok = file:close(File),\n\tok."),
    write_table_dump_dump(TableList, Fd),
    
    ?FWRITE(Fd, "\n\nlst_to_bin (null) ->\n\t<<\"NULL\">>;"
        "\nlst_to_bin (List) ->\n\tList2 = escape_str(List, []),"
        "\n\tBin = list_to_binary(List2),"
        "\n\t<<\"'\", Bin/binary, \"'\">>."
        "\n\nint_to_bin (null) ->\n\t<<\"NULL\">>;"
        "\nint_to_bin (Value) ->\n\tlist_to_binary(integer_to_list(Value))."
        "\n\nrel_to_bin (null) ->\n\t<<\"NULL\">>;"
        "\nrel_to_bin (Value) when is_integer(Value) ->"
        "\n\tlist_to_binary(integer_to_list(Value));"
        "\nrel_to_bin (Value) ->\n\tlist_to_binary(float_to_list(Value))."
        "\n\nescape_str ([], Result) ->\n\tlists:reverse(Result);"
        "\nescape_str ([$' | String], Result) ->"
        "\n\tescape_str(String, [$' | [$\\\\ | Result]]);"
        "\nescape_str ([$\" | String], Result) ->"
        "\n\tescape_str(String, [$\" | [$\\\\ | Result]]);"
        "\nescape_str ([$\\\\ | String], Result) ->"
        "\n\tescape_str(String, [$\\ | [$\\\\ | Result]]);"
        "\nescape_str ([$\\n | String], Result) ->"
        "\n\tescape_str(String, [$n | [$\\\\ | Result]]);"
        "\nescape_str ([$\\r | String], Result) ->"
        "\n\tescape_str(String, [$r | [$\\\\ | Result]]);"
        "\nescape_str ([Char | String], Result) ->"
        "\n\tescape_str(String, [Char | Result])."
    ),
    
    ?FCLOSE(Fd).
    
write_table_dump_dump ([], _) ->
    ok;
write_table_dump_dump ([T | L], Fd) ->
    TableName = T #table.name,
    
    ?FWRITE(Fd, "\n\ndump_" ++ T #table.name ++ " (File) ->" 
        "\n\tok = file:write(File, <<\"DELETE FROM `abnormal_record_type`;\\n\\n\">>),"
    ),
 
    case table_need_split(T) of
        {true, _} ->
            ?FWRITE(Fd, "\n\tSize = lists:foldl(fun(I, S)-> S + "
                "ets:info(list_to_atom(\"t_" ++ TableName ++ "_\""
                " ++ integer_to_list(I)), size) end, 0, lists:seq(0, 99)),"
            ),
            
            ?FWRITE(Fd, "\n\n\tlists:foldl(fun(I, {S1, N1, L1}) ->"
                "\n\t\tets:foldl(fun(Record, {S, N, L}) ->"
            ),
            
            lists:foreach(
                fun(C) ->
                    ?FWRITE(Fd, "\n\t\t\t_" ++ C #column.format_name ++ " = " ++ 
                        get_trans_by_column(C) ++
                        "(Record #" ++ TableName ++ "." ++ C #column.name ++ "),"
                    )
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\n\n\t\t\tLast = if N == 100 orelse S + 1 == Size ->"
                " <<\");\\n\\n\">>; true -> <<\"),\\n\">> end,"
                "\n\n\t\t\tL2 = [<<\"(\","
            ),
            
            lists:foreach(
                fun(C) ->
                    ?FWRITE(Fd, "\n\t\t\t\t_" ++ C #column.format_name ++ "/binary, \",\",")
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\n\t\t\t\tLast/binary\n\t\t\t>> | L],"
                "\n\n\t\t\tif N == 100 orelse S + 1 == Size ->"
                "\n\t\t\t\tfile:write(File, [<<\"INSERT IGNORE INTO `" ++
                TableName ++ "` (\"", -4
            ),
            
            lists:foreach(
                fun(C) ->
                    ?FWRITE(Fd, "\n\t\t\t\t\t\"`" ++ C #column.name ++ "`, \"")
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\"\n\t\t\t\t\t\") VALUES \">> | lists:reverse(L2)], "
                "self()),\n\n\t\t\t\t{S + 1, 0, []};\n\t\t\ttrue ->"
                "\n\t\t\t\t{S + 1, N + 1, L2}\n\t\t\tend"
                "\n\t\tend, {S1, N1, L1}, list_to_atom(\"t_" ++ 
                TableName ++ "\" ++ integer_to_list(I)))"
                "\n\tend, {0, 0, []}, lists:seq(0, 99)),"
                "\n\tok = file:write(File, <<\"\\n\">>),\n\tok.", -3
            );
        _ ->
            ?FWRITE(Fd, "\n\tSize = ets:info(t_" ++ T #table.name ++ 
                ", size),"
            ),
            
            ?FWRITE(Fd, "\n\n\tets:foldl(fun(Record, {S, N, L}) ->"),
            
            lists:foreach(
                fun(C) ->
                    ?FWRITE(Fd, "\n\t\t_" ++ C #column.format_name ++ " = " ++ 
                        get_trans_by_column(C) ++
                        "(Record #" ++ TableName ++ "." ++ C #column.name ++ "),"
                    )
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\n\n\t\tLast = if N == 100 orelse S + 1 == Size ->"
                " <<\");\\n\\n\">>; true -> <<\"),\\n\">> end,"
                "\n\n\t\tL2 = [<<\"(\","
            ),
            
            lists:foreach(
                fun(C) ->
                    ?FWRITE(Fd, "\n\t\t\t_" ++ C #column.format_name ++ "/binary, \",\",")
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\n\t\t\tLast/binary\n\t\t>> | L],"
                "\n\n\t\tif N == 100 orelse S + 1 == Size ->"
                "\n\t\t\tok = file:write(File, [<<\"INSERT IGNORE INTO `" ++
                TableName ++ "` (\"", -4
            ),
            
            lists:foreach(
                fun(C) ->
                    ?FWRITE(Fd, "\n\t\t\t\t\"`" ++ C #column.name ++ "`, \"")
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\"\n\t\t\t\t\") VALUES \">> | lists:reverse(L2)], "
                "self()),\n\n\t\t\t{S + 1, 0, []};\n\t\ttrue ->"
                "\n\t\t\t{S + 1, N + 1, L2}\n\t\tend"
                "\n\tend, {0, 0, []}, t_" ++ TableName ++
                "),\n\tok = file:write(File, <<\"\\n\">>),\n\tok.", -3
            )
    end,
    
    write_table_dump_dump(L, Fd).
    
generate_db_game (Db) ->
    Dir = get_default_dir(out),
    {ok, Fd} = ?FOPEN(Dir ++ "game_db.erl", [write]),
    TableList = [T || T <- Db #db.table_list, table_write_only(T) =:= false],
    
    ?FWRITE(Fd, "-module(game_db)."
        "\n\n-export([\n\tdirty_select/3,\n\tdirty_select/2,"
        "\n\tdirty_read/1,\n\tselect/3,\n\tselect/2,\n\tread/1,\n\twrite/1,"
        "\n\tdelete/1,\n\tdelete_all/1,\n\tdelete_select/3,\n\tdelete_select/2,"
        "\n\ttable/1,\n\ttable/2,\n\tets/1,\n\tets/2,\n\tcount/1,\n\tmemory/0,"
        "\n\tmemory/1,\n\tfetch/1,\n\tdo/1\n])."
        "\n\n-include(\"gen/game_db.hrl\")."
        "\n\n-define(ENSURE_TRAN, ensure_tran())."
        "\n\nensure_tran () ->"
        "\n\tcase get(tran_action_list) of"
        "\n\t\tundefined -> exit(need_gamedb_tran);"
        "\n\t\t_ -> ok\n\tend."
        "\n\ndirty_select (Table, PlayerId, MatchSpec) ->"
        "\n\tselect(Table, PlayerId, MatchSpec)."
        "\n\ndirty_select (Table, MatchSpec) ->"
        "\n\tselect(Table, MatchSpec)."
        "\n\ndirty_read (Key) ->\n\tread(Key)."
    ),
    
    write_db_select(TableList, Fd),
    write_db_read(TableList, Fd),
    write_db_write(Db #db.table_list, Fd),
    write_db_delete(TableList, Fd),
    write_db_delete_all(TableList, Fd),
    write_db_delete_select(TableList, Fd),
    write_db_table_1(TableList, Fd),
    write_db_table_2(TableList, Fd),
    write_db_ets(TableList, Fd),
    write_db_v_insert(Db #db.table_list, Fd),
    write_db_v_update(Db #db.table_list, Fd),
    write_db_count(TableList, Fd),
    write_db_memory_0(TableList, Fd),
    write_db_memory_1(TableList, Fd),
    write_db_fetch(Fd),
    write_db_do(Fd),
    write_db_other(Fd),
    
    ?FCLOSE(Fd).
 
write_db_select (L, Fd) ->
    write_db_select(L, Fd, [], []).
    
write_db_select ([], Fd, S2L, S3L) ->
    write_db_select_2(lists:reverse(S2L), Fd),
    write_db_select_3(lists:reverse(S3L), Fd);
write_db_select ([T | L], Fd, S2L, S3L) ->
    case table_need_split(T) of
        {true, _} ->
            write_db_select(L, Fd, S2L, [T | S3L]);
        _ ->
            write_db_select(L, Fd, [T | S2L], S3L)
    end.
    
write_db_select_2 ([], Fd) ->
    ?FWRITE(Fd, "\n\nselect (_, _) ->\n\texit(bad_match).");
write_db_select_2 ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\nselect (" ++ T #table.name ++ 
        ", MatchSpec) ->\n\tets:select(t_" ++
        T #table.name ++ ", MatchSpec);"
    ),
    
    write_db_select_2(L, Fd).
    
write_db_select_3 ([], Fd) ->
    ?FWRITE(Fd, "\n\nselect (_, _, _) ->\n\texit(bad_match).");
write_db_select_3 ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\nselect (" ++ T #table.name ++
        ", ModeOrFragId, MatchSpec) ->"
        "\n\tcase ModeOrFragId of"
        "\n\t\tslow ->\n\t\t\tfetch_select(\"t_" ++
        T #table.name ++ "_\", MatchSpec);"
        "\n\t\tFragId ->\n\t\t\tets:select(list_to_atom(\"t_" ++
        T #table.name ++ 
        "_\" ++ integer_to_list(FragId rem 100)), MatchSpec)\n\tend;"
    ),
    
    write_db_select_3(L, Fd).
    
write_db_read ([], Fd) ->
    ?FWRITE(Fd, ".", -1);
write_db_read ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\nread (#pk_" ++ T #table.name ++ "{"),
    PK = table_primary_key(T),
    
    lists:foreach(
        fun(K) ->
            ?FWRITE(Fd, K #column.name ++ " = " ++ K #column.format_name ++ ", ")
        end,
        PK
    ),
    
    ?FWRITE(Fd, "}) ->", -2),
    
    case table_need_split(T) of
        {true, SC} ->
            case lists:member(SC, PK) of
                true ->
                    ?FWRITE(Fd, "\n\tets:lookup(list_to_atom(\"t_" ++ T #table.name ++ 
                        "_\" ++ integer_to_list(" ++ SC #column.format_name ++ " rem 100)), {"
                    );
                _ ->
                    ?FWRITE(Fd, "\n\tfetch_lookup(\"t_" ++ T #table.name ++ "_\", {")
            end,
            
            lists:foreach(
                fun(K) ->
                    ?FWRITE(Fd, K #column.format_name ++ ", ")
                end,
                PK
            ),
            
            ?FWRITE(Fd, "});", -2);
        _ ->
            ?FWRITE(Fd, "\n\tets:lookup(t_" ++ T #table.name ++ ", {"),
            
            lists:foreach(
                fun(K) ->
                    ?FWRITE(Fd, K #column.format_name ++ ", ")
                end,
                PK
            ),
            
            ?FWRITE(Fd, "});", -2)
    end,
    
    write_db_read(L, Fd).
    
write_db_write ([], Fd) ->
    ?FWRITE(Fd, ".", -1);
write_db_write ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\nwrite (Record) when is_record(Record, " ++
        T #table.name ++ ") ->\n\t?ENSURE_TRAN,"
    ),
    
    case table_need_split(T) of
        {true, SC} ->
            ?FWRITE(Fd, "\n\tEtsTable = list_to_atom(\"t_" ++
                T #table.name ++ "_\" ++ integer_to_list(Record #" ++
                T #table.name ++ "." ++ SC #column.name ++ " rem 100)),"
            );
        _ ->
            ?FWRITE(Fd, "\n\tEtsTable = t_" ++ T #table.name ++ ",")
    end,
    
    ?FWRITE(Fd, "\n\n\tcase Record #" ++ T #table.name ++ ".row_key of"
        "\n\t\tundefined ->\n\t\t\tvalidate_for_insert(Record),"
    ),
    
    PK = table_primary_key(T),
    
    case table_auto_increment(T) of
        {true, AC} ->
            ?FWRITE(Fd, "\n\t\t\tNewId = ets:update_counter(auto_increment, {" ++
                T #table.name ++ ", " ++ AC #column.name ++ "}, 1),"
                "\n\n\t\t\tNewRecord = Record #" ++ T #table.name ++ 
                "{\n\t\t\t\t" ++ AC #column.name ++ " = NewId,\n\t\t\t\trow_key = {"
            ),
            
            lists:foreach(
                fun(K) ->
                    case K =:= AC of
                        true ->
                            ?FWRITE(Fd, "\n\t\t\t\t\tNewId,");
                        _ ->
                            ?FWRITE(Fd, "\n\t\t\t\t\tRecord #" ++
                                T #table.name ++ "." ++ K #column.name ++ ","
                            )
                    end
                end,
                PK
            ),
            
            ?FWRITE(Fd, "\n\t\t\t\t}\n\t\t\t},", -1);
        _ ->
            ?FWRITE(Fd, "\n\n\t\t\tNewRecord = Record #" ++ 
                T #table.name ++ "{\n\t\t\t\trow_key = {"
            ),
            
            lists:foreach(
                fun(K) ->
                    ?FWRITE(Fd, "\n\t\t\t\t\tRecord #" ++
                        T #table.name ++ "." ++ K #column.name ++ ","
                    )
                end,
                PK
            ),
            
            ?FWRITE(Fd, "\n\t\t\t\t}\n\t\t\t},", -1)
    end,
    
    case table_write_only(T) of
        false ->
            ?FWRITE(Fd, "\n\n\t\t\ttrue = ets:insert_new(EtsTable, NewRecord),"
                "\n\t\t\tadd_tran_log({insert, EtsTable, NewRecord #" ++ 
                T #table.name ++ ".row_key}),"
            );
        _ ->
            ?FWRITE(Fd, "\n")
    end,
    
    ?FWRITE(Fd, "\n\t\t\tadd_tran_action({" ++ T #table.name ++
        ", insert, NewRecord}),\n\t\t\t{ok, NewRecord};\n\t\t_ ->"
        "\n\t\t\tvalidate_for_update(Record),"
        "\n\t\t\t[OldRecord] = ets:lookup(EtsTable, Record #" ++ T #table.name ++
        ".row_key),\n\t\t\tif OldRecord #" ++ T #table.name ++ 
        ".row_ver =:= Record #" ++ T #table.name ++ ".row_ver -> ok end,"
        "\n\t\t\tChanges = get_changes(" ++ 
        ?I2L(length(T #table.column) + 2) ++ ", Record, OldRecord),"
        "\n\t\t\tRealNewRecord = Record #" ++ T #table.name ++ 
        "{row_ver = Record #" ++ T #table.name ++ ".row_ver + 1},"
        "\n\t\t\tets:insert(EtsTable, RealNewRecord),"
        "\n\t\t\tadd_tran_log({update, EtsTable, OldRecord}),"
        "\n\t\t\tadd_tran_action({" ++ T #table.name ++
        ", update, Record, Changes}),\n\t\t\t{ok, RealNewRecord}\n\tend;"
    ),
    
    write_db_write(L, Fd).
    
write_db_delete ([], Fd) ->
    ?FWRITE(Fd, ".", -1);
write_db_delete ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\ndelete (#" ++ T #table.name ++ 
        "{row_key = RowKey} = Record) ->\n\t?ENSURE_TRAN,"
    ),
    
    case table_need_split(T) of
        {true, SC} ->
            ?FWRITE(Fd, "\n\tEtsTable = list_to_atom(\"t_" ++ 
                T #table.name ++ "_\" ++ integer_to_list(Record #" ++ 
                T #table.name ++ "." ++ SC #column.name ++ " rem 100)),"
            );
        _ ->
            ?FWRITE(Fd, "\n\tEtsTable = t_" ++ T #table.name ++ ",")
    end,
    
    ?FWRITE(Fd, "\n\tets:delete(EtsTable, RowKey),"
        "\n\tadd_tran_log({delete, EtsTable, Record}),\n\tadd_tran_action({" ++ 
        T #table.name ++ ", delete, Record}),\n\tok;"
    ),
    
    write_db_delete(L, Fd).
    
write_db_delete_all ([], Fd) ->
    ?FWRITE(Fd, ".", -1);
write_db_delete_all ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\ndelete_all (" ++ T #table.name ++ 
        ") ->\n\t?ENSURE_TRAN,"
    ),
    
    case table_need_split(T) of
        {true, _} ->
            ?FWRITE(Fd, "\n\t[ets:delete_all_objects(list_to_atom(\"t_" ++
                T #table.name ++ 
                "_\" ++ integer_to_list(Id))) || Id <- lists:seq(0, 99)],"
                "\n\tadd_tran_action({" ++ T #table.name ++ 
                ", sql, \"DELETE FROM `" ++ T #table.name ++ "`;\"}),\n\tok;"
            );
        _ ->
            ?FWRITE(Fd, "\n\tets:delete_all_objects(t_" ++ T #table.name ++ 
                "),\n\tadd_tran_action({" ++ T #table.name ++ 
                ", sql, \"DELETE FROM `" ++ T #table.name ++ "`;\"}),\n\tok;"
            )
    end,
    
    write_db_delete_all(L, Fd).
    
write_db_delete_select (L, Fd) ->
    write_db_delete_select(L, Fd, [], []).
    
write_db_delete_select ([], Fd, S2L, S3L) ->
    write_db_delete_select_2(lists:reverse(S2L), Fd),
    write_db_delete_select_3(lists:reverse(S3L), Fd);
write_db_delete_select ([T | L], Fd, S2L, S3L) ->
    case table_need_split(T) of
        {true, _} ->
            write_db_delete_select(L, Fd, S2L, [T | S3L]);
        _ ->
            write_db_delete_select(L, Fd, [T | S2L], S3L)
    end.
    
write_db_delete_select_2 ([], Fd) ->
    ?FWRITE(Fd, "\n\ndelete_select (_, _) ->\n\texit(bad_match).");
write_db_delete_select_2 ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\ndelete_select (" ++ T #table.name ++ 
        ", MatchSpec) ->\n\t?ENSURE_TRAN,"
        "\n\n\tcase select(" ++ T #table.name ++
        ", MatchSpec) of\n\t\t[] ->\n\t\t\t{ok, 0};"
        "\n\t\tRows when is_list(Rows) ->"
        "\n\t\t\tNum = lists:foldl(fun(Row, Count) ->"
        "\n\t\t\t\tdelete(Row),\n\t\t\t\tCount + 1"
        "\n\t\t\tend, 0, Rows),\n\n\t\t\t{ok, Num}\n\tend;"
    ),
    
    write_db_delete_select_2(L, Fd).
    
write_db_delete_select_3 ([], Fd) ->
    ?FWRITE(Fd, "\n\ndelete_select (_, _, _) ->\n\texit(bad_match).");
write_db_delete_select_3 ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\ndelete_select (" ++ T #table.name ++
        ", ModeOrFragKey, MatchSpec) ->\n\t?ENSURE_TRAN,"
        "\n\n\tcase select(" ++ T #table.name ++ 
        ", ModeOrFragKey, MatchSpec) of\n\t\t[] ->\n\t\t\t{ok, 0};"
        "\n\t\tRows when is_list(Rows) ->"
        "\n\t\t\tNum = lists:foldl(fun(Row, Count) ->"
        "\n\t\t\t\tdelete(Row),\n\t\t\t\tCount + 1"
        "\n\t\t\tend, 0, Rows),\n\n\t\t\t{ok, Num}\n\tend;"
    ),
    
    write_db_delete_select_3(L, Fd).
    
write_db_table_1 (L, Fd) ->   
    write_db_table_1(L, Fd, true).
    
write_db_table_1 ([], Fd, _) ->
    ?FWRITE(Fd, ".", -1);
write_db_table_1 ([T | L], Fd, Head) ->
    case Head of
        true -> ?FWRITE(Fd, "\n");
        _ -> ok
    end,
    
    ?FWRITE(Fd, "\ntable (" ++ T #table.name ++ 
        ") -> ets:table(t_" ++ T #table.name ++ ");"
    ),
    
    write_db_table_1(L, Fd, false).
    
write_db_table_2 (L, Fd) ->   
    write_db_table_2(L, Fd, true).
    
write_db_table_2 ([], Fd, _) ->
    ?FWRITE(Fd, ".", -1);
write_db_table_2 ([T | L], Fd, Head) ->
    case Head of
        true -> ?FWRITE(Fd, "\n");
        _ -> ok
    end,
    
    ?FWRITE(Fd, "\ntable (" ++ T #table.name ++ 
        ", Options) -> ets:table(" ++ T #table.name ++ ", Options);"
    ),
    
    write_db_table_2(L, Fd, false).
    
write_db_ets (L, Fd) ->
    write_db_ets(L, Fd, [], []).
    
write_db_ets ([], Fd, S1L, S2L) ->
    write_db_ets_1(lists:reverse(S1L), Fd, true),
    write_db_ets_2(lists:reverse(S2L), Fd, true);
write_db_ets ([T | L], Fd, S1L, S2L) ->
    case table_need_split(T) of
        {true, _} ->
            write_db_ets(L, Fd, S1L, [T | S2L]);
        _ ->
            write_db_ets(L, Fd, [T | S1L], S2L)
    end.
    
write_db_ets_1 ([], Fd, Head) ->
    case Head of
        true -> ?FWRITE(Fd, "\n");
        _ -> ok
    end,
    
    ?FWRITE(Fd, "\nets (_) -> exit(bad_match).");
write_db_ets_1 ([T | L], Fd, Head) ->
    case Head of
        true -> ?FWRITE(Fd, "\n");
        _ -> ok
    end,
    
    ?FWRITE(Fd, "\nets (" ++ T #table.name ++ ") -> t_" ++
        T #table.name ++ ";"
    ),
    
    write_db_ets_1(L, Fd, false).
    
write_db_ets_2 ([], Fd, Head) ->
    case Head of
        true -> ?FWRITE(Fd, "\n");
        _ -> ok
    end,
    
    ?FWRITE(Fd, "\nets (_, _) -> exit(bad_match).");
write_db_ets_2 ([T | L], Fd, Head) ->
    case Head of
        true -> ?FWRITE(Fd, "\n");
        _ -> ok
    end,
    
    ?FWRITE(Fd, "\nets (" ++ T #table.name ++ 
        ", FragId) -> list_to_atom(\"t_" ++ T #table.name ++ 
        "_\" ++ integer_to_list(FragId rem 100));"
    ),
    
    write_db_ets_2(L, Fd, false).
    
write_db_v_insert ([], Fd) ->
    ?FWRITE(Fd, ".", -1);
write_db_v_insert ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\nvalidate_for_insert (Record) when is_record(Record, " ++
        T #table.name ++ ") ->"
    ),
    
    ACL = case table_auto_increment(T) of
        {true, AC} -> [AC];
        _ -> []
    end,
    
    lists:foreach(
        fun(C) ->
            ?FWRITE(Fd, "\n\tif Record #" ++ T #table.name ++ "." ++
                C #column.name ++ " == null -> throw({null_column, insert, " ++
                T #table.name ++ ", " ++ C #column.name ++ "}); true -> ok end,"
            )
        end,
        T #table.column -- ACL
    ),
    
    ?FWRITE(Fd, "\n\tok;"),
    write_db_v_insert(L, Fd).
    
write_db_v_update ([], Fd) ->
    ?FWRITE(Fd, ".", -1);
write_db_v_update ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\nvalidate_for_update (Record) when is_record(Record, " ++
        T #table.name ++ ") ->"
    ),
    
    lists:foreach(
        fun(C) ->
            ?FWRITE(Fd, "\n\tif Record #" ++ T #table.name ++ "." ++
                C #column.name ++ " == null -> throw({null_column, update, " ++
                T #table.name ++ ", " ++ C #column.name ++ "}); true -> ok end,"
            )
        end,
        T #table.column
    ),
    
    ?FWRITE(Fd, "\n\tok;"),
    write_db_v_update(L, Fd).
    
write_db_count (L, Fd) ->
    write_db_count(L, Fd, true).
    
write_db_count ([], Fd, _) ->
    ?FWRITE(Fd, ".", -1);
write_db_count ([T | L], Fd, Head) ->
    case Head of
        true -> ?FWRITE(Fd, "\n");
        _ -> ok
    end,
    
    ?FWRITE(Fd, "\ncount (" ++ T #table.name ++ 
        ") -> {size, Size} = lists:keyfind(size, 1, ets:info(t_" ++ 
        T #table.name ++ ")), Size;"
    ),
    
    write_db_count(L, Fd, false).
    
write_db_memory_0 (L, Fd) ->
    ?FWRITE(Fd, "\n\nmemory () ->"),
    
    lists:foreach(
        fun(T) ->
            ?FWRITE(Fd, "\n\tmemory(" ++ T #table.name ++ ") +")
        end,
        L
    ),
    
    ?FWRITE(Fd, ".", -2).
    
write_db_memory_1 (L, Fd) ->
    write_db_memory_1(L, Fd, true).
    
write_db_memory_1 ([], Fd, _) ->
    ?FWRITE(Fd, ".", -1);
write_db_memory_1 ([T | L], Fd, Head) ->
    case Head of
        true -> ?FWRITE(Fd, "\n");
        _ -> ok
    end,
    
    ?FWRITE(Fd, "\nmemory (" ++ T #table.name ++ 
        ") -> {memory, Memory} =lists:keyfind(memory, 1, ets:info(t_" ++
        T #table.name ++ ")), Memory;"
    ),
    
    write_db_memory_1(L, Fd, false).
    
write_db_fetch (Fd) ->
    ?FWRITE(Fd, "\n\nfetch (Sql) ->"
        "\n\t{data, ResultId} = mysql:fetch(gamedb, Sql),"
        "\n\tlib_mysql:get_rows(ResultId)."
    ).
    
write_db_do (Fd) ->
    ?FWRITE(Fd, "\n\ndo (Tran) ->"
        "\n\tcase get(tran_action_list) of"
        "\n\t\tundefined ->"
        "\n\t\t\tput(tran_log, []),"
        "\n\t\t\tput(tran_action_list, []),"
        "\n\t\t\tput(tran_action_list2, []),"
        "\n\n\t\t\tcase catch Tran() of"
        "\n\t\t\t\t{'EXIT', {aborted, Reason}} ->"
        "\n\t\t\t\t\trollback(get(tran_log)),"
        "\n\t\t\t\t\terase(tran_log),"
        "\n\t\t\t\t\terase(tran_action_list),"
        "\n\t\t\t\t\terase(tran_action_list2),"
        "\n\t\t\t\t\texit(Reason);"
        "\n\t\t\t\t{'EXIT', Reason} ->"
        "\n\t\t\t\t\trollback(get(tran_log)),"
        "\n\t\t\t\t\terase(tran_log),"
        "\n\t\t\t\t\terase(tran_action_list),"
        "\n\t\t\t\t\terase(tran_action_list2),"
        "\n\t\t\t\t\texit(Reason);"
        "\n\t\t\t\tResult ->"
        "\n\t\t\t\t\terase(tran_log),"
        "\n\t\t\t\t\tTranActionList = erase(tran_action_list),"
        "\n\n\t\t\t\t\tcase TranActionList of"
        "\n\t\t\t\t\t\t[] -> ok;"
        "\n\t\t\t\t\t\t_ -> game_db_sync_proc ! {sync, TranActionList}"
        "\n\t\t\t\t\tend,"
        "\n\n\t\t\t\t\t{atomic, Result}"
        "\n\t\t\tend;\n\t\t_ ->\n\t\t\t{atomic, Tran()}\n\tend."
    ).
    
write_db_other (Fd) ->
    ?FWRITE(Fd, "\n\nadd_tran_log (Data) ->"
        "\n\tTranLogList = get(tran_log),"
        "\n\tput(tran_log, [Data | TranLogList])."
        "\n\nadd_tran_action (TranAction) ->"
        "\n\tTranActionList = get(tran_action_list),"
        "\n\tput(tran_action_list, [TranAction | TranActionList])."
        "\n\nrollback ([]) ->\n\tok;"
        "\nrollback ([Data | Term]) ->"
        "\n\tcase Data of"
        "\n\t\t{insert, Table, RowKey} ->"
        "\n\t\t\tets:delete(Table, RowKey);"
        "\n\t\t{update, Table, Row} ->"
        "\n\t\t\tets:insert(Table, Row);"
        "\n\t\t{delete, Table, Row} ->"
        "\n\t\t\tets:insert(Table, Row)"
        "\n\tend,\n\trollback(Term)."
        "\n\nget_changes(N, NewRecord, OldRecord) ->"
        "\n\tget_changes(N, NewRecord, OldRecord, [])."
        "\n\nget_changes(2, _, _, Changes) ->"
        "\n\tChanges;"
        "\nget_changes(N, NewRecord, OldRecord, Changes) ->"
        "\n\tcase element(N, NewRecord) =:= element(N, OldRecord) of"
        "\n\t\ttrue -> get_changes(N - 1, NewRecord, OldRecord, Changes);"
        "\n\t\tfalse -> get_changes(N - 1, NewRecord, OldRecord, [N | Changes])"
        "\n\tend."
        "\n\nfetch_lookup(TablePrefix, Key) ->"
        "\n\tfetch_lookup(TablePrefix, Key, 0)."
        "\n\nfetch_lookup(_, _, 100) ->"
        "\n\t[];\nfetch_lookup(TablePrefix, Key, N) ->"
        "\n\tcase ets:lookup(list_to_atom(TablePrefix ++ integer_to_list(N)), Key) of"
        "\n\t\t[] -> fetch_lookup(TablePrefix, Key, N + 1);"
        "\n\t\tR -> R\n\tend."
        "\n\nfetch_select(TablePrefix, MatchSpec) ->"
        "\n\tfetch_select(TablePrefix, MatchSpec, 0, [])."
        "\n\nfetch_select(_, _, 100, Result) ->"
        "\n\tlists:concat(Result);"
        "\nfetch_select(TablePrefix, MatchSpec, N, Result) ->"
        "\n\tfetch_select(TablePrefix, MatchSpec, N + 1, ["
        "\n\t\tets:select(list_to_atom(TablePrefix ++ integer_to_list(N)), MatchSpec)"
        " | Result\n\t])."
    ).
    
generate_db_sync (Db) ->
    Dir = get_default_dir(out),
    {ok, Fd} = ?FOPEN(Dir ++ "game_db_sync.erl", [write]),
    
    ?FWRITE(Fd, "-module(game_db_sync)."
        "\n\n-export([\n\tstart_proc/0,\n\tsync_proc_init/0,"
        "\n\n\tstart_worker0/0,\n\tstart_worker1/0,"
        "\n\n\tsync_worker0_init/0,\n\tsync_worker1_init/0,"
        "\n\n\twait_for_all_data_sync0/1,"
        "\n\twait_for_all_data_sync1/1,"
        "\n\n\tcount_work0/0,\n\tcount_work1/0\n])."
        "\n\n-include(\"gen/game_db.hrl\")."
    ),
    
    write_sync_action(Db #db.table_list, Fd),
    write_sync_generate(Db #db.table_list, Fd),
    write_sync_other(Fd),
    
    ?FCLOSE(Fd).
    
write_sync_action ([], Fd) ->
    ?FWRITE(Fd, ".", -1);
write_sync_action ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\ntran_action_to_sql ({" ++ T #table.name ++
        ", sql, Sql}) ->\n\tlist_to_binary(Sql);"
        "\n\ntran_action_to_sql ({" ++ T #table.name ++
        ", insert, Record}) ->"
    ),
    
    lists:foreach(
        fun(C) ->
            ?FWRITE(Fd, "\n\t" ++ C #column.format_name ++ " = " ++
                get_trans_by_column(C) ++ "(Record #" ++
                T #table.name ++ "." ++ C #column.name ++ "),"
            )
        end,
        T #table.column
    ),
    
    ?FWRITE(Fd, "\n\n\t<<\n\t\"INSERT IGNORE INTO `" ++ 
        T #table.name ++ "` SET \""
    ),
    
    lists:foldl(
        fun(C, First) ->
            case First of
                true ->
                    ?FWRITE(Fd, "\n\t\"`" ++ C #column.name ++ "` = \", " ++
                        C #column.format_name ++ "/binary,"
                    );
                _ ->
                    ?FWRITE(Fd, "\n\t\", `" ++ C #column.name ++ "` = \", " ++
                        C #column.format_name ++ "/binary,"
                    )
            end,
            
            false
        end,
        true,
        T #table.column
    ),
    
    ?FWRITE(Fd, "\n\t\";\\n\"\n\t>>;\n\ntran_action_to_sql ({" ++ 
        T #table.name ++ ", delete, Record}) ->"
    ),
    
    PK = table_primary_key(T),
    
    lists:foreach(
        fun(K) ->
            ?FWRITE(Fd, "\n\t" ++ K #column.format_name ++ " = " ++
                get_trans_by_column(K) ++ "(Record #" ++
                T #table.name ++ "." ++ K #column.name ++ "),"
            )
        end,
        PK
    ),
    
    ?FWRITE(Fd, "\n\n\t<<\n\t\"DELETE FROM `" ++
        T #table.name ++ "` WHERE\","
    ),
    
    lists:foreach(
        fun(K) ->
            ?FWRITE(Fd, "\n\t\" `" ++ K #column.name ++ "` = \", " ++
                K #column.format_name ++ "/binary, \" AND \","
            )
        end,
        PK
    ),
    
    ?FWRITE(Fd, "\n\t\";\\n\"\n\t>>;"
        "\n\ntran_action_to_sql ({" ++ T #table.name ++ 
        ", update, _, []}) ->\n\tnone;\ntran_action_to_sql ({" ++
        T #table.name ++ ", update, Record, Changes}) ->"
        "\n\tSql = generate_update_sql(" ++ T #table.name ++ 
        ", Record, Changes, [<<\"UPDATE `" ++ T #table.name ++ 
        "` SET \">>]),\n\tlist_to_binary(lists:reverse(Sql));", -9
    ),
    
    write_sync_action(L, Fd).
    
write_sync_generate ([], Fd) ->
    ?FWRITE(Fd, ".", -1);
write_sync_generate ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\ngenerate_update_sql (" ++ T #table.name ++
        ", Record, [], Sql) ->"
    ),
    
    PK = table_primary_key(T),
    
    lists:foreach(
        fun(K) ->
            ?FWRITE(Fd, "\n\t" ++ K #column.format_name ++ " = " ++ 
                get_trans_by_column(K) ++ "(Record #" ++
                T #table.name ++ "." ++ K #column.name ++ "),"
            )
        end,
        PK
    ),
    
    ?FWRITE(Fd, "\n\t[<<\" WHERE \", "),
    
    lists:foreach(
        fun(K) ->
            ?FWRITE(Fd, "\" `" ++ K #column.name ++ "` = \", " ++
                K #column.format_name ++ "/binary, \" AND \", "
            )
        end,
        PK
    ),
    
    ?FWRITE(Fd, "\";\\n\">> | Sql];", -9),
    
    AIC = case table_auto_increment(T) of
        {true, C} -> C;
        _ -> nil
    end,
    
    lists:foldl(
        fun(C, CI) ->
            case C =:= AIC of
                true -> 
                    ok;
                _ ->
                    ?FWRITE(Fd, "\n\ngenerate_update_sql (" ++
                        T #table.name ++ ", Record, [" ++
                        ?I2L(CI + 1) ++ " | Changes], Sql) ->\n\t" ++
                        C #column.format_name ++ " = " ++
                        get_trans_by_column(C) ++ "(Record #" ++
                        T #table.name ++ "." ++ C #column.name ++ 
                        "),\n\tSql2 = case length(Sql) of 1 -> [<<\"`" ++
                        C #column.name ++ "` = \", " ++
                        C #column.format_name ++ 
                        "/binary>> | Sql]; _ -> [<<\",`" ++
                        C #column.name ++ "` = \", " ++
                        C #column.format_name ++ "/binary>> | Sql] end,"
                        "\n\tgenerate_update_sql(" ++
                        T #table.name ++ ", Record, Changes, Sql2);"
                    )
            end,
            
            CI + 1
        end,
        2,
        T #table.column
    ),
    
    write_sync_generate(L, Fd).
    
write_sync_other (Fd) ->
    ?FWRITE(Fd, "\n\n
count_work0 () ->
	{message_queue_len, Len} = process_info(whereis(game_db_sync_worker0), message_queue_len),
    Len.

count_work1 () ->
	{message_queue_len, Len} = process_info(whereis(game_db_sync_worker1), message_queue_len),
    Len.

wait_for_all_data_sync0 (TimeOut) ->
	wait_for_all_data_sync0(TimeOut, 0).
	
wait_for_all_data_sync0 (TimeOut, TimeOut) ->
    case io:get_chars(\"Time out, continue? [Y/n] : \", 1) of
        \"n\" ->
	        io:format(\"wait for all player data sync (0) ... time out\"),
	        time_out;
        _ ->
	        wait_for_all_data_sync0(TimeOut, 0)
    end;
wait_for_all_data_sync0 (TimeOut, Time) ->
	io:format(\"wait for all player data sync (0) ... \"),
	receive
	after 1000 ->
        case count_work0() of
            0 -> io:format(\"done~n\"), ok;
			N -> io:format(\"~p~n\", [N]), wait_for_all_data_sync0(TimeOut, Time + 1)
		end
	end.

wait_for_all_data_sync1 (TimeOut) ->
	wait_for_all_data_sync1(TimeOut, 0).
	
wait_for_all_data_sync1 (TimeOut, TimeOut) ->
    case io:get_chars(\"Time out, continue? [Y/n] : \", 1) of
        \"n\" ->
	        io:format(\"wait for all player data sync (1) ... time out\"),
	        time_out;
        _ ->
	        wait_for_all_data_sync1(TimeOut, 0)
    end;
wait_for_all_data_sync1 (TimeOut, Time) ->
	io:format(\"wait for all player data sync (1) ... \"),
	receive
	after 1000 ->
        case count_work1() of
            0 -> io:format(\"done~n\"), ok;
			N -> io:format(\"~p~n\", [N]), wait_for_all_data_sync1(TimeOut, Time + 1)
		end
	end.

start_proc () ->
    proc_lib:start_link(?MODULE, sync_proc_init, []).

sync_proc_init () ->
    register(game_db_sync_proc, self()),
    proc_lib:init_ack({ok, self()}),
    sync_proc_loop().

sync_proc_loop() ->
    receive
        {sync, TranActionList} ->
            case catch tran_action_list_to_sql_list(TranActionList) of
                [] -> 
                    sync_proc_loop();
                SqlList when is_list(SqlList) ->
                    game_db_sync_worker0 ! {work, SqlList},
                    game_db_sync_worker1 ! {work, SqlList},
                    sync_proc_loop();
                Error -> 
                    io:format(\"sync_proc_loop:  TranActionList = ~p~n  Error = ~p~n\", [TranActionList, Error]),
                    sync_proc_loop()
            end;
        {apply, From, M, F, A} ->
            From ! (catch apply(M, F, A)),
            sync_proc_loop();
        _ ->
            sync_proc_loop()
    end.

start_worker0 () ->
    proc_lib:start_link(?MODULE, sync_worker0_init, []).
    
sync_worker0_init () ->
    register(game_db_sync_worker0, self()),
    proc_lib:init_ack({ok, self()}),
    {{Y, M, D}, {H, MM,SS}} = erlang:localtime(),
    Time = {Y, M, D, H},
	{ok, LogFile} = get_log_file(Time),
	erlang:send_after((3600 - (MM * 60 + SS)) * 1000, self(), change_file),
    sync_worker0_loop(Time, LogFile).

sync_worker0_loop (Time, LogFile) ->
    receive
        {work, SqlList} ->
            case catch file:write(LogFile, [<<\"\\n\">> | SqlList]) of
                ok -> ok;
                Result -> io:format(\"sync_worker0_loop:  Result = ~p~n\", [Result])
            end,
            sync_worker0_loop(Time, LogFile);
		change_file ->
            ok = file:close(LogFile), 
			{{Y, M, D}, {H, MM, SS}} = erlang:localtime(),
			Time2 = {Y, M, D, H},
			{ok, LogFile2} = get_log_file(Time2),
			erlang:send_after((3600 - (MM * 60 + SS)) * 1000, self(), change_file),
			sync_worker0_loop(Time2, LogFile2);
        {apply, From, M, F, A} ->
            From ! (catch apply(M, F, A)),
            sync_worker0_loop(Time, LogFile);
        _ ->
            sync_worker0_loop(Time, LogFile)
    end.

start_worker1() ->
    proc_lib:start_link(?MODULE, sync_worker1_init, []).

sync_worker1_init () ->
    register(game_db_sync_worker1, self()),
    proc_lib:init_ack({ok, self()}),
    sync_worker1_loop().

sync_worker1_loop () ->
    receive
        {work, SqlList} ->
            case catch sync_sql_list1(SqlList) of
                {ok, _} -> ok;
                Result -> io:format(\"sync_worker1_loop:  SqlList = ~p~n  Result = ~p~n\", [SqlList, Result])
            end,
            sync_worker1_loop();
        {apply, From, M, F, A} ->
            From ! (catch apply(M, F, A)),
            sync_worker1_loop();
        _ ->
            sync_worker1_loop()
    end.

sync_sql_list1 (SqlList) ->
    {ok, mysql:fetch(gamedb, [SqlList], infinity)}.

tran_action_list_to_sql_list (TranActions) ->
    tran_action_list_to_sql_list(TranActions, []).
    
tran_action_list_to_sql_list ([], SqlList) ->
    SqlList;
tran_action_list_to_sql_list ([TranAction | Tail], SqlList) ->
    case tran_action_to_sql(TranAction) of
        none -> tran_action_list_to_sql_list(Tail, SqlList);
        Sql  -> tran_action_list_to_sql_list(Tail, [Sql | SqlList])
    end.
    
lst_to_bin (null) ->
	<<\"NULL\">>;
lst_to_bin (List) ->
	List2 = escape_str(List, []),
	Bin = list_to_binary(List2),
	<<\"'\", Bin/binary, \"'\">>.
	
int_to_bin (null) ->
    <<\"NULL\">>;
int_to_bin (Value) ->
    list_to_binary(integer_to_list(Value)).

rel_to_bin (null) ->
    <<\"NULL\">>;
rel_to_bin (Value) when is_integer(Value) ->
    list_to_binary(integer_to_list(Value));
rel_to_bin (Value) ->
    list_to_binary(float_to_list(Value)).

escape_str ([], Result) ->
	lists:reverse(Result);
escape_str ([$' | String], Result) ->
	escape_str(String, [$' | [$\\\\ | Result]]);
escape_str ([$\" | String], Result) ->
	escape_str(String, [$\" | [$\\\\ | Result]]);
escape_str ([$\\\\ | String], Result) ->
	escape_str(String, [$\\\\ | [$\\\\ | Result]]);
escape_str ([Char | String], Result) ->
	escape_str(String, [Char | Result]).

get_log_file({Y, M, D, H}) ->
    FileName = \"data/\" ++ integer_to_list(Y) ++ \"_\" ++ integer_to_list(M) ++ \"_\" ++ integer_to_list(D) ++ \"/\" ++ integer_to_list(H) ++ \".sql\",
    case filelib:is_file(FileName) of
        true -> ok;
        false -> ok = filelib:ensure_dir(FileName)
    end,
	{ok, File} = file:open(FileName, [append, raw, {delayed_write, 1024, 1000}]),
    ok = file:write(File, <<\"/*!40101 SET NAMES utf8 */;\\n\">>),
    ok = file:write(File, <<\"/*!40101 SET SQL_MODE=''*/;\\n\">>),
    ok = file:write(File, <<\"/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;\\n\">>),
    ok = file:write(File, <<\"/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;\\n\">>),
    ok = file:write(File, <<\"/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;\\n\">>),
    ok = file:write(File, <<\"/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;\\n\\n\">>),
    {ok, File}."
    ).
    
generate_db_test (Db) ->
    Dir = get_default_dir(out),
    {ok, Fd} = ?FOPEN(Dir ++ "game_db_test.erl", [write]),
    TableList = [T || T <- Db #db.table_list, table_write_only(T) =:= false],
    
    ?FWRITE(Fd, "-module(game_db_test)."
        "\n\n-export([\n\tinsert/2\n])."
        "\n\n-include(\"gen/game_db.hrl\")."
    ),
    
    write_test_insert(TableList, Fd),
    write_test_other(Fd),
    
    ?FCLOSE(Fd).
    
write_test_insert ([], Fd) ->
    ?FWRITE(Fd, ".", -1);
write_test_insert ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\ninsert (" ++ T #table.name ++ ", InsertNum) ->"),
    
    case table_auto_increment(T) of
        {true, AC} ->
            ?FWRITE(Fd, "\n\tT = fun() ->\n\t\tlists:foreach(\n\t\t\t"
                "fun(_) ->\n\t\t\t\tgame_db:write(\n\t\t\t\t\t#" ++ 
                T #table.name ++ "{"
            ),
            
            lists:foreach(
                fun(C) ->
                    ?FWRITE(Fd, "\n\t\t\t\t\t\t" ++ C #column.name ++
                        " = random_column_value(" ++ 
                        C #column.type ++ "),"
                    )
                end,
                T #table.column -- [AC]
            ),
            
            ?FWRITE(Fd, "\n\t\t\t\t\t}\n\t\t\t\t)\n\t\t\tend,"
                "\n\t\t\tlists:seq(1, InsertNum)\n\t\t)\n\tend,"
                "\n\tgame_db:do(T);", -1
            );
        _ ->
            PK = table_primary_key(T),
            
            ?FWRITE(Fd, "\n\tT = fun() ->\n\t\t"
                "lists:foreach(\n\t\t\tfun(_) ->"
            ),
            
            lists:foreach(
                fun(K) ->
                    ?FWRITE(Fd, "\n\t\t\t\t" ++ K #column.format_name ++
                        " = random_column_value(" ++ 
                        K #column.type ++ "),"
                    )
                end,
                PK
            ),
            
            ?FWRITE(Fd, "\n\n\t\t\t\tcase game_db:read(#pk_" ++
                T #table.name ++ "{"
            ),
            
            lists:foreach(
                fun(K) ->
                    ?FWRITE(Fd, K #column.name ++ " = " ++
                        K #column.format_name ++ ", "
                    )
                end,
                PK
            ),
            
            ?FWRITE(Fd, "}) of\n\t\t\t\t\t[_] ->\n\t\t\t\t\t\tok;"
                "\n\t\t\t\t\t_ ->\n\t\t\t\t\t\tgame_db:write("
                "\n\t\t\t\t\t\t\t#" ++ T #table.name ++ "{", -2
            ),
            
            lists:foreach(
                fun(K) ->
                    ?FWRITE(Fd, "\n\t\t\t\t\t\t\t\t" ++ K #column.name ++
                        " = " ++ K #column.format_name ++ ","
                    )
                end,
                PK
            ),
            
            lists:foreach(
                fun(C) ->
                    ?FWRITE(Fd, "\n\t\t\t\t\t\t\t\t" ++ C #column.name ++
                        " = random_column_value(" ++ 
                        C #column.type ++ "),"
                    )
                end,
                T #table.column -- PK
            ),
            
            ?FWRITE(Fd, "\n\t\t\t\t\t\t\t}\n\t\t\t\t\t\t)"
                "\n\t\t\t\tend\n\t\t\tend,\n\t\t\tlists:seq(1, InsertNum)"
                "\n\t\t)\n\tend,\n\tgame_db:do(T);", -1
            )
    end,
    
    write_test_insert(L, Fd).
            
write_test_other (Fd) ->
    ?FWRITE(Fd, "\n\n
random_column_value (int) ->
    lib_misc:random_number(99999);
random_column_value (bigint) ->
    lib_misc:random_number(99999);
random_column_value (tinyint) ->
    lib_misc:random_number(99999);
random_column_value (mediumint) ->
    lib_misc:random_number(99999);
random_column_value (float) ->
    lib_misc:random_number(99999);
random_column_value (varchar) ->
    random_string(10, []);
random_column_value (text) ->
    random_string(10, []);
random_column_value (char) ->
    random_string(10, []).
    
random_string (0, Str) ->
    Str;
random_string (Len, Str) ->
    random_string(Len - 1, [lib_misc:random_number_2($a, $z) | Str])."
    ).
    
get_trans_by_column (C) ->
    case C #column.type of
        "int" -> "int_to_bin";
        "bigint" -> "int_to_bin";
        "tinyint" -> "int_to_bin";
        "mediumint" -> "int_to_bin";
        "float" -> "rel_to_bin";
        "varchar" -> "lst_to_bin";
        "text" -> "lst_to_bin";
        "char" -> "lst_to_bin"
    end.
    
table_auto_increment (T) ->
    case lists:keyfind("auto_increment", #column.extra, T #table.column) of
        C when is_record(C, column) ->
            {true, C};
        _ ->
            false
    end.
    
table_need_split (T) ->
    case re:run(T #table.name, "^player_") of
        {match, _} ->
            case lists:keyfind("player_id", #column.name, T #table.column) of
                C when is_record(C, column) ->
                    {true, C};
                _ ->
                    false
            end;
        _ ->
            false
    end.
    
table_write_only (T) ->
    case re:run(T #table.name, "_log$") of
        {match, _} ->
            true;
        _ ->
            false
    end.
    
table_primary_key (T) ->
    table_primary_key(T #table.column, []).
    
table_primary_key ([], KL) ->
    lists:reverse(KL);
table_primary_key ([C | L], KL) ->
    case C #column.key of
        "PRI" ->
            table_primary_key(L, [C | KL]);
        _ ->
            table_primary_key(L, KL)
    end.