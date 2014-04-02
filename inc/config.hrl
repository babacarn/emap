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
%% Description: Constants
%%

-author('Babacar Ndiaye <babacar@ndiaye.name>').

-record(vector, {lat, lon}).
-record(point, {x, y}).
-record(aircraft, {squawk, position, heading, acceleration, speed, climb_rate, altitude, status}).

-define(SCREEN_BG_COLOR, ?wxBLACK).
-define(SCREEN_REFRESH_RATE, 100).

-define(SCREEN_MAP_MAX_DETAIL_LEVEL, 2).
-define(SCREEN_MAP_BRUSH, wxBrush:new({0, 255, 0})).
-define(SCREEN_MAP_PEN, wxPen:new({0, 255, 0})).

-define(SCREEN_AIRCRAFT_CLEAR_BRUSH, wxBrush:new({0, 0, 0})).
-define(SCREEN_AIRCRAFT_CLEAR_PEN, wxPen:new({255, 255, 0})).
-define(SCREEN_AIRCRAFT_DANGER_BRUSH, wxBrush:new({0, 0, 0})).
-define(SCREEN_AIRCRAFT_DANGER_PEN, wxPen:new({255, 0, 0})).
-define(SCREEN_AIRCRAFT_SIZE, 7).
-define(SCREEN_AIRCRAFT_VECTOR_SIZE, 20).
-define(SCREEN_AIRCRAFT_INFO_FONT, wxFont:new(8, ?wxFONTFAMILY_MODERN, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_LIGHT)).
-define(SCREEN_AIRCRAFT_INFO_COLOR, {255, 255, 0}).

-define(SCREEN_GRID_BRUSH, wxBrush:new({60, 60, 60})).
-define(SCREEN_GRID_PEN, wxPen:new({60, 60, 60})).
-define(SCREEN_GRID_HORIZONTAL_SPACE, 4).
-define(SCREEN_GRID_VERTICAL_SPACE, 4).

-define(AIRCRAFT_UPDATE_RATE, 100).

-define(ATC_UPDATE_RATE, 100).
-define(ATC_DANGER_DISTANCE, 30).







