-module(coursework).
-export([main/0, fileloop/3, start/3, stop/1, collect_results/2, find_pair/2, get_pair/2, remove_duplicates/1, build_char_map/2, update_map/2, return_lines/1, return_charlist/2]).

%% MAIN FUNCTION

main() ->
	Contents = return_lines("input.txt"),
	{G,Rest} = string:to_integer(lists:nth(1, Contents)),
	{K,Rest2} = string:to_integer(lists:nth(2, Contents)),
	Files = lists:nthtail(2,Contents),
	Main_PID = spawn(coursework,collect_results,[[],K]),
	fileloop(Files, Main_PID, G).
	
fileloop(Files,Main_PID,G) ->
	case length(Files) > 0 of
		true -> spawn(coursework,start,[Files,Main_PID,G]);
		false -> spawn(coursework,stop,[Main_PID])
	end.

start(Files,Main_PID,G) ->
	Result = return_charlist(return_lines(lists:nth(1,Files)),G),
	Main_PID ! Result,
	fileloop(lists:nthtail(1,Files), Main_PID,G).
	
stop(Main_PID) ->
	Main_PID ! all_done.

collect_results(Charlist,K) ->
	receive
		all_done ->
			Newmap = maps:new(),
			Final = build_char_map(Charlist,Newmap),
			List = maps:to_list(Final),
			F = fun({{X1,Y1},V1}, {{X2,Y2},V2}) -> V1 > V2 end,
			Finallist = lists:sort(F,List),
			io:format("~p~n", [lists:sublist(Finallist,K)]);
		Result ->
			collect_results(Charlist ++ Result,K)
	end.
	

%% ALREADY CALLED FROM INSIDE OTHER FUNCTIONS

% Find character pairs of a given gap on a single line
find_pair(Line,G) ->
	case length(Line) > 1 of
		true -> get_pair(Line,G) ++ find_pair(lists:nthtail(1,Line),G);
		false -> []
	end.

get_pair(Line,0) -> [];
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
	{Response, Contents} = file:read_file(Filename),
	string:tokens(binary:bin_to_list(Contents), "\n").

% Return the list of character pairs on a list of lines
return_charlist([],G) -> [];
return_charlist(Contents,G) ->
	Line = lists:nth(1, Contents),
	Uniques = remove_duplicates(find_pair(Line,G)),
	Uniques ++ return_charlist(lists:nthtail(1,Contents),G).

