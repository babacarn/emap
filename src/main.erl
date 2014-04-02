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
%% Created: Feb 8, 2010
%% Description: Application entry point
%%

-module(main).
-author('Babacar Ndiaye <babacar@ndiaye.name>').

-include_lib("wx/include/wx.hrl").

-include("config.hrl").

-export([start/0]).

start() ->
	Aircrafts = [
	    %aircraft:new(Squawk, Position, Heading, Acceleration, ClimbRate)
		aircraft:new("2311", #vector{lat=-180.0, lon=0.0}, 90.0, 0.0, 100),
		aircraft:new("2312", #vector{lat=40.0, lon=60.0}, 150.0, 0.0, 100),
		aircraft:new("2313", #vector{lat=60.0, lon=40.0}, 195.0, 0.0, 100),
		aircraft:new("2314", #vector{lat=90.0, lon=10.0}, 225.0, 0.0, 100)
	],
	
	Atc = atc:new(),
	Atc ! {register, Aircrafts},
	
	Console = console:new(),
	Console ! {register, aircrafts, Aircrafts},
	%Console ! {register, segments, parser:read("e-worldmap/data/asia-cil.txt.gz")},
	%Console ! {register, segments, parser:read("e-worldmap/data/asia-riv.txt.gz")},
	%Console ! {register, segments, parser:read("e-worldmap/data/namer-cil.txt.gz")},
	%Console ! {register, segments, parser:read("e-worldmap/data/africa-riv.txt.gz")},
	%Console ! {register, segments, parser:read("e-worldmap/data/samer-riv.txt.gz")},
	%Console ! {register, segments, parser:read("e-worldmap/data/samer-cil.txt.gz")},
	%Console ! {register, segments, parser:read("e-worldmap/data/namer-riv.txt.gz")},
	%Console ! {register, segments, parser:read("e-worldmap/data/europe-cil.txt.gz")},
	%Console ! {register, segments, parser:read("e-worldmap/data/africa-cil.txt.gz")},
	%Console ! {register, segments, parser:read("e-worldmap/data/europe-riv.txt.gz")},
	%Console ! {register, segments, parser:read("e-worldmap/data/africa-bdy.txt.gz")},
	%Console ! {register, segments, parser:read("e-worldmap/data/asia-bdy.txt.gz")},
	%Console ! {register, segments, parser:read("e-worldmap/data/samer-bdy.txt.gz")},
	Console ! {register, segments, parser:read("e-worldmap/data/namer-pby.txt.gz")},
	%Console ! {register, segments, parser:read("e-worldmap/data/europe-bdy.txt.gz")},
	Console ! {register, segments, parser:read("e-worldmap/data/namer-bdy.txt.gz")},
	%Console ! {register, segments, parser:read("e-worldmap/data/test.txt.gz")},
			   
	loop().

loop() ->
	receive 
		{'EXIT', Pid, Reason} -> 
			io:format("EXIT for ~p because of ~p ~n", [Pid, Reason]), 
			exit(normal), ok
	end.


