%% Copyright (C) 2010 Babacar Ndiaye.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Created: Feb 7, 2010
%% Description: IO routines
%%

-module(parser).
-author('Babacar Ndiaye <babacar@ndiaye.name>').

-include("config.hrl").

-export([read/1]).

read(Filename) ->
	case file:open(Filename, [read, compressed]) of
        {ok, Device} -> Points = parse(Device, [], []), file:close(Device), Points;
		{error, Reason} -> {error, Reason}
    end.

parse(Device, Points, Segment) -> 
	case io:get_line(Device, "") of
        eof -> Points;
        Line -> 
			case parse(Line) of
				[R] -> 
					if 
						R < ?SCREEN_MAP_MAX_DETAIL_LEVEL ->
							case Segment of
								[] -> parse(Device, Points, []);
								_ -> parse(Device, Points ++ [{R, Segment}], [])
							end;
						true -> parse(Device, Points, [])
					end;
				Point -> parse(Device, Points, Segment ++ [Point])
			end
    end.

parse(Line) ->
	case re:split(Line,"[ \n\r\t]", [{return, list}, trim]) of
		[_, Lon, Lat] -> 
			{X, []} = string:to_float(Lat), 
			{Y, []} = string:to_float(Lon), 
			#vector{lat=X, lon=Y};
		[_, _, _, _, Rank, _, _, _] -> 
			{R, []} = string:to_integer(Rank),
			[R]
	end.
