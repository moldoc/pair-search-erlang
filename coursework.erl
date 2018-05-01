-module(coursework).
-export([find_pair/2, get_pair/2]).

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


