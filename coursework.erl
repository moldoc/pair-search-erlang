-module(coursework).
-export([find_pair/2, get_pair/2, remove_duplicates/1, build_char_map/2, update_map/2]).

%% Find character pairs of a given gap on a single line
find_pair(Line,G) ->
	case length(Line) > 1 of
		true -> get_pair(Line,G) ++ find_pair(lists:nthtail(1,Line),G);
		false -> []
	end.

get_pair(Line,0) -> [];
get_pair(Line,G) ->
	case length(Line) > G of
		true -> [{{hd(Line),lists:nth(G+1,Line)},1} | get_pair(Line,G-1)];
		false -> [{{hd(Line),lists:nth(length(Line),Line)},1} | get_pair(Line,length(Line)-2)]
	end.

%% Removes duplicate elements from a list
%% Called from each line
remove_duplicates(List) ->
	lists:usort(List).

%% Build the character map from the list of character pairs
build_char_map([],Charmap) -> Charmap;
build_char_map(Charlist,Charmap) -> build_char_map(lists:nthtail(1,Charlist),update_map(lists:nth(1,Charlist),Charmap)).

update_map(Key,Charmap) ->
	case maps:is_key(Key,Charmap) of
		true -> maps:update(Key,maps:get(Key,Charmap,1)+1,Charmap);
		false -> maps:put(Key,1,Charmap)
	end.
