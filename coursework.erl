-module(coursework).
-export([main/0, fileloop/4, start/4, stop/1, collect_results/3, prettify/2, 
        find_pair/2, get_pair/2, remove_duplicates/1, build_char_map/2, update_map/2, 
        return_lines/1, return_charlist/2, filterSpaces/1]).

%% MAIN FUNCTION
% Call this to execute the whole program.
main() ->
    Contents = return_lines("input.txt"),
    {G,_} = string:to_integer(lists:nth(1, Contents)),
    {K,_} = string:to_integer(lists:nth(2, Contents)),
    % List of the names of the files we want to process.
    Files = lists:nthtail(2,Contents),
    Main_PID = spawn(coursework,collect_results,[[],K,0]),
    fileloop(Files, Main_PID, G, 0).

%% Function for going through all the given files.
%% Spawns a process for each file.
%% The parameter "Lines" is the total number of lines in all of the files,
%% and it is updated on each iteration.
fileloop(Files,Main_PID,G,Lines) ->
    case length(Files) > 0 of
        true -> spawn(coursework,start,[Files,Main_PID,G,Lines]);
        false -> spawn(coursework,stop,[Main_PID])
    end.

%% Handles a single file. The parameters are the same as for fileloop.
start(Files,Main_PID,G,L) ->
    Lines = return_lines(lists:nth(1,Files)),
    Result = return_charlist(Lines,G),
    Main_PID ! [Result,L+length(Lines)],
    fileloop(lists:nthtail(1,Files), Main_PID,G,L+length(Lines)).

%% Sends the main process a message that all is finished.
stop(Main_PID) ->
    Main_PID ! all_done.

%% Collects the character pair -results from the files.
%% Outputs final result to the output file when all files have been processed.
%% Parameters: the list of character pairs and their values (each file updates this),
%% the number of most common pairs defined in the input file, and the total
%% number of lines in all the files.
collect_results(Charlist,K,Lines) ->
    receive
        all_done ->
            Newmap = maps:new(),
            Final = build_char_map(Charlist,Newmap),
            List = maps:to_list(Final),
            F = fun({{_,_},V1}, {{_,_},V2}) -> V1 > V2 end,
            Sorted = lists:sort(F,List),
            Output = [lists:append(prettify(lists:sublist(Sorted,K), Lines))],
            file:write_file("output.txt", io_lib:fwrite("~s~n", Output));
        Result ->
            collect_results(Charlist ++ hd(Result),K,lists:last(Result))
    end.
    
%% Builds a list of strings where each item is the original character pair in
%% form "c1 c2 v".
prettify([],_) -> [];
prettify(Charlist,Lines) ->
    % One line of characters
    Listitem = hd(Charlist),
    C1 = lists:droplast(tuple_to_list(hd(tuple_to_list(Listitem)))),
    C2 = tl(tuple_to_list(hd(tuple_to_list(Listitem)))),
    V = integer_to_list(lists:last(tuple_to_list(Listitem))),
    String = C1 ++ " " ++ C2 ++ " " ++ V ++ " " ++ integer_to_list(Lines) ++ "\n",
    [ String | prettify(tl(Charlist),Lines)].
    

%% Find character pairs of a given gap on a single line.
%% Returns a list of tuples {c1,c2}.
find_pair(Line,G) ->
    case length(Line) > 1 of
        true -> get_pair(Line,G+1) ++ find_pair(lists:nthtail(1,Line),G);
        false -> []
    end.

get_pair(_,0) -> [];
get_pair(Line,G) ->
    case length(Line) > G of
        true -> [{hd(Line),lists:nth(G+1,Line)} | get_pair(Line,G-1)];
        false -> [{hd(Line),lists:nth(length(Line),Line)} | get_pair(Line,length(Line)-2)]
    end.

%% Remove duplicate elements from a list.
%% Called from each line so that the final value for each character pair
%% will be the number of lines they appeared on.
remove_duplicates(List) ->
    lists:usort(List).


%% Build the character map from the list of character pairs.
build_char_map([],Charmap) -> Charmap;
build_char_map(Charlist,Charmap) -> build_char_map(lists:nthtail(1,Charlist),
                                    update_map(lists:nth(1,Charlist),Charmap)).

%% If the character pair is already in the list, increase its value by one.
%% Otherwise add a new character pair with value 1 to the list.
update_map(Key,Charmap) ->
    case maps:is_key(Key,Charmap) of
        true -> maps:update(Key,maps:get(Key,Charmap,1)+1,Charmap);
        false -> maps:put(Key,1,Charmap)
    end.
    
    
%% Return the lines of a file in a list.
return_lines(Filename) -> 
    {_, Contents} = file:read_file(Filename),
    string:tokens(binary:bin_to_list(Contents), "\n").

%% Return the list of character pairs on a list of lines.
return_charlist([],_) -> [];
return_charlist(Contents,G) ->
    Line = lists:nth(1, Contents),
    % If you want to include spaces as characters, remove Nospace
    % and replace it with Line.
    Nospace = filterSpaces(Line),
    Uniques = remove_duplicates(find_pair(Nospace,G)),
    Uniques ++ return_charlist(lists:nthtail(1,Contents),G).

%% Filters the spaces on a line, so that there won't be character pairs such as {"c1", " "}
%% and {" ", "c2"} in the final result. 
filterSpaces(Line) ->
    F = fun(C) -> C =/= 32 end,
    lists:filter(F, Line).
    
