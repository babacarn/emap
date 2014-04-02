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
%% Created: Feb 13, 2010
%% Description: Representation of an aircraft
%%

-module(aircraft).
-author('Babacar Ndiaye <babacar@ndiaye.name>').

-include("config.hrl").

-export([new/5, loop/2]).

new(Squawk, Position, Heading, Acceleration, ClimbRate) ->
	process_flag(trap_exit, true),
	Pid = spawn_link(aircraft, loop, [
		#aircraft{
			squawk=Squawk, 
			heading=Heading, 
			position=Position,
			speed=#vector{lat=2, lon=2},
			acceleration=Acceleration,
			climb_rate=ClimbRate, 
			altitude=0.0,
			status=safe
		}, util:now()]),
	timer:send_interval(?AIRCRAFT_UPDATE_RATE, Pid, {update, internals}),
	Pid.

loop(Aircraft, LastRun) ->
	receive
		{update, status, NewStatus} ->
			#aircraft{
				squawk=Squawk, 
				position=Position, 
				heading=Heading, 
				acceleration=Acceleration,
				speed=Speed, 
				climb_rate=ClimbRate, 
				altitude=Altitude
			} = Aircraft,
			
			loop(#aircraft{
				squawk=Squawk, 
				position=Position,
				heading=Heading, 
				acceleration=Acceleration,
				speed=Speed,
				climb_rate=ClimbRate, 
				altitude=Altitude,
				status=NewStatus
			}, LastRun);
		
		{update, internals} ->
			Now = util:now(),
			Delta = Now - LastRun,
			
			#aircraft{
				squawk=Squawk, 
				position=#vector{lat=Positionlat, lon=Positionlon}, 
				heading=Heading, 
				acceleration=Acceleration,
				speed=Speed, 
				climb_rate=ClimbRate, 
				altitude=Altitude,
				status=Status
			} = Aircraft,
			
			DeltaSpeedlat = Acceleration * Delta * math:sin(util:deg_to_rad(Heading)),
			DeltaSpeedlon = Acceleration * Delta * math:cos(util:deg_to_rad(Heading)),
			
			NewSpeedlat = Speed#vector.lat + DeltaSpeedlat,
			NewSpeedlon = Speed#vector.lon + DeltaSpeedlon,
			
			NewPositionlat = Positionlat + NewSpeedlat * Delta,
			NewPositionlon = Positionlon + NewSpeedlon * Delta,
			
			NewAltitude = Altitude + ClimbRate * Delta,
			
			loop(#aircraft{
				squawk=Squawk, 
				position=util:calibrate(#vector{lat=NewPositionlat, lon=NewPositionlon}),
				heading=Heading, 
				acceleration=Acceleration,
				speed=#vector{lat=NewSpeedlat, lon=NewSpeedlon},
				climb_rate=ClimbRate, 
				altitude=NewAltitude,
				status=Status
			}, Now);
		
		{status, From, Ref} ->
			From ! {Ref, Aircraft},
			loop(Aircraft, LastRun)
	end.

