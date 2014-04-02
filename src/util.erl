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
%% Description: Geometry operations
%%

-module(util).
-author('Babacar Ndiaye <babacar@ndiaye.name>').

-include("config.hrl").

-export([rad_to_deg/1, deg_to_rad/1, norm/1]).
-export([calibrate/1, calibrate/2]).
-export([now/0]).
-export([abs/1]).

rad_to_deg(Radian) -> 
	(Radian * 180) / math:pi().

deg_to_rad(Degree) -> 
	(Degree / 180) * math:pi().

norm(Item) ->
	case Item of
		#point{x=X, y=Y} -> 
			math:sqrt(math:pow(X, 2) + math:pow(Y, 2));
		#vector{lat=Lat, lon=Lon} -> 
			math:sqrt(math:pow(Lat, 2) + math:pow(Lon, 2))
	end.

calibrate(Vector) ->
	#vector{lat=Lat, lon=Lon} = Vector,
	#vector{lat=calibrate(Lat, 180.0), lon=calibrate(Lon, 90.0)}.

calibrate(Value, Max) ->
	if
		Value > Max -> calibrate(Value - 2 * Max, Max);
		Value < -Max -> calibrate(Value + 2 * Max, Max);
		true -> Value
	end.

now() ->
	{Mega, Sec, Micro} = erlang:now(),
	((Mega * 1000000.0 + Sec) * 1000000.0 + Micro) / 1000000.0.

abs(A) ->
    case (A < 0) of
    	true -> -1 * A;
    	false -> A
    end.
