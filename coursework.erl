-module(coursework).
-export([main/0, fileloop/4, start/4, stop/1, collect_results/3, find_pair/2, get_pair/2, remove_duplicates/1, build_char_map/2, update_map/2, return_lines/1, return_charlist/2]).

%% MAIN FUNCTION

main() ->
	Contents = return_lines("input.txt"),
	{G,_} = string:to_integer(lists:nth(1, Contents)),
	{K,_} = string:to_integer(lists:nth(2, Contents)),
	Files = lists:nthtail(2,Contents),
	Main_PID = spawn(coursework,collect_results,[[],K,0]),
	fileloop(Files, Main_PID, G, 0).
	
fileloop(Files,Main_PID,G,Lines) ->
	case length(Files) > 0 of
		true -> spawn(coursework,start,[Files,Main_PID,G,Lines]);
		false -> spawn(coursework,stop,[Main_PID])
	end.

start(Files,Main_PID,G,L) ->
	Lines = return_lines(lists:nth(1,Files)),
	Result = return_charlist(Lines,G),
	Main_PID ! [Result,L+length(Lines)],
	fileloop(lists:nthtail(1,Files), Main_PID,G,L+length(Lines)).
	
stop(Main_PID) ->
	Main_PID ! all_done.

collect_results(Charlist,K,Lines) ->
	receive
		all_done ->
			Newmap = maps:new(),
			Final = build_char_map(Charlist,Newmap),
			List = maps:to_list(Final),
			F = fun({{_,_},V1}, {{_,_},V2}) -> V1 > V2 end,
			Finallist = lists:sort(F,List),
			%io:format("~p~n", [lists:sublist(Finallist,K)]);
			%prettify(hd(Finallist));
			io:format("~p~n", [prettify(lists:sublist(Finallist,K))]),
			io:format("~p~n", [Lines]);
			%file:write_file("output.txt", io_lib:fwrite("~s~n",[lists:sublist(Finallist,K)]));
		Result ->
			collect_results(Charlist ++ hd(Result),K,lists:last(Result))
	end.
	
% Builds a list of strings where each item is the original character pair in
% form "c1 c2 v"
prettify([]) -> [];
prettify(Charlist) ->
	Listitem = hd(Charlist),
	C1 = lists:droplast(tuple_to_list(hd(tuple_to_list(Listitem)))),
	C2 = tl(tuple_to_list(hd(tuple_to_list(Listitem)))),
	%V = lists:concat(tl(tuple_to_list(Listitem))),
	String = C1 ++ " " ++ C2,
	[ String | prettify(tl(Charlist))].
	%file:write_file("output.txt", io_lib:fwrite("~p~n",[tuple_to_list(hd(tuple_to_list(Listitem)))])).
	

%% ALREADY CALLED FROM INSIDE OTHER FUNCTIONS

% Find character pairs of a given gap on a single line
find_pair(Line,G) ->
	case length(Line) > 1 of
		true -> get_pair(Line,G) ++ find_pair(lists:nthtail(1,Line),G);
		false -> []
	end.

get_pair(_,0) -> [];
get_pair(Line,G) ->
	case length(Line) > G of
		true -> [{hd(Line),lists:nth(G+1,Line)} | get_pair(Line,G-1)];
		false -> [{hd(Line),lists:nth(length(Line),Line)} | get_pair(Line,length(Line)-2)]
	end.

% Remove duplicate elements from a list
% Called from each line
remove_duplicates(List) ->
	lists:usort(List).


%% CALLED AFTER ALL FILES PROCESSED

% Build the character map from the list of character pairs
build_char_map([],Charmap) -> Charmap;
build_char_map(Charlist,Charmap) -> build_char_map(lists:nthtail(1,Charlist),update_map(lists:nth(1,Charlist),Charmap)).

update_map(Key,Charmap) ->
	case maps:is_key(Key,Charmap) of
		true -> maps:update(Key,maps:get(Key,Charmap,1)+1,Charmap);
		false -> maps:put(Key,1,Charmap)
	end.
	
	
%% EVERY FILE CALLS THESE FUNCTIONS

% Return the lines of a file in a list
return_lines(Filename) -> 
	{_, Contents} = file:read_file(Filename),
	string:tokens(binary:bin_to_list(Contents), "\n").

% Return the list of character pairs on a list of lines
return_charlist([],_) -> [];
return_charlist(Contents,G) ->
	Line = lists:nth(1, Contents),
	Uniques = remove_duplicates(find_pair(Line,G)),
	Uniques ++ return_charlist(lists:nthtail(1,Contents),G).

