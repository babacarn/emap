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
%% Created: Mar 6, 2010
%% Description: Air traffic controller
%%

-module(atc).
-author('Babacar Ndiaye <babacar@ndiaye.name>').

-include("config.hrl").

-export([new/0, loop/1]).

new() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(atc, loop, [[]]),
	timer:send_interval(?ATC_UPDATE_RATE, Pid, {update}),
	Pid.

loop(Aircrafts) ->
	receive
		{register, NewAircrafts} ->
			loop(NewAircrafts ++ Aircrafts);
		
		{update} ->
			lists:foreach(
			  fun(Aircraft) -> collision_detector(Aircraft, Aircrafts) end, 
			  Aircrafts),
			loop(Aircrafts)
	end.

collision_detector(H, []) ->
	H ! {update, status, safe};

collision_detector(H, [O | T]) -> 
	HRef = make_ref(),
	H ! {status, self(), HRef}, 
	receive {HRef, HAircraftStatus} -> 
		ORef = make_ref(),
		O ! {status, self(), ORef}, 
		receive {ORef, OAircraftStatus} ->
			#aircraft{squawk=HId, position=#vector{lat=HPositionlat, lon=HPositionlon}} = HAircraftStatus,
			#aircraft{squawk=OId, position=#vector{lat=OPositionlat, lon=OPositionlon}} = OAircraftStatus,
			case string:equal(HId, OId) of
				false ->
					Distance = util:norm(#vector{lat=OPositionlat - HPositionlat, lon=OPositionlon - HPositionlon}),
					case Distance < ?ATC_DANGER_DISTANCE of
						true -> 
							H ! {update, status, danger},
							O ! {update, status, danger};
						false -> collision_detector(H, T)
					end;
				true -> 
					collision_detector(H, T)
			end		
		end		
	end.

