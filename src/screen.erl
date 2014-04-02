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
%% Created: Feb 10, 2010
%% Description: TODO: Add description to wm_screen
%%

-module(screen).
-author('Babacar Ndiaye <babacar@ndiaye.name>').

-include_lib("wx/include/wx.hrl").

-include("config.hrl").

-export([new/1, draw_vertical_grids/1, draw_horizontal_grids/1, draw_segments/2, draw_aircraft/2]).

new(Frame) ->
	Screen = wxPanel:new(Frame),
	wxPanel:setBackgroundColour(Screen, ?SCREEN_BG_COLOR),
	Screen.

draw_horizontal_grids(Dc) ->
	wxPaintDC:setBrush(Dc, ?SCREEN_GRID_BRUSH),
	wxPaintDC:setPen(Dc, ?SCREEN_GRID_PEN),
	{W, H} = wxPaintDC:getSize(Dc),
	
	HorizontalSep = trunc(180 / ?SCREEN_GRID_HORIZONTAL_SPACE),
	Horizontals = lists:seq(-90 + HorizontalSep, 90 - HorizontalSep, HorizontalSep),
	lists:map(
  		fun(Horizontal) ->
			XL = 0,
			XR = 360,
			Y = 90 - Horizontal,
			wxPaintDC:drawLine(
			  	Dc,
				{trunc(XL * W / 360.0), trunc(Y * H / 180.0)}, 
				{trunc(XR * W / 360.0), trunc(Y * H / 180.0)}
			)
		end, 
		Horizontals).

draw_vertical_grids(Dc) ->
	wxPaintDC:setBrush(Dc, ?SCREEN_GRID_BRUSH),
	wxPaintDC:setPen(Dc, ?SCREEN_GRID_PEN),
	{W, H} = wxPaintDC:getSize(Dc),
	
	VerticalSep = trunc(360 / ?SCREEN_GRID_VERTICAL_SPACE),
	Verticals = lists:seq(-180 + VerticalSep, 180 - VerticalSep, VerticalSep),
	lists:map(
  		fun(Vertical) ->
			X = 180.0 + Vertical,
			YT = 0,
			YB = 180,
			wxPaintDC:drawLine(
			  	Dc,
				{trunc(X * W / 360.0), trunc(YT * H / 180.0)}, 
				{trunc(X * W / 360.0), trunc(YB * H / 180.0)}
			)
		end, 
		Verticals).

draw_segments(Dc, Segments) ->
	case Segments of
		[] -> ok;
		[{_, Points} | Rest] ->
			wxPaintDC:setBrush(Dc, ?SCREEN_MAP_BRUSH),
			wxPaintDC:setPen(Dc, ?SCREEN_MAP_PEN),
			{W, H} = wxPaintDC:getSize(Dc),
			wxPaintDC:drawLines(Dc, lists:map(
				fun(P) ->
					X = 180.0 + P#vector.lat, 
					Y = 90.0 - P#vector.lon,
					{trunc(X * W / 360.0), trunc(Y * H / 180.0)}
				end, 
			Points)),
			draw_segments(Dc, Rest)
	end.

draw_aircraft(Dc, Aircraft) ->
	{ScreenWidth, ScreenHeight} = wxPaintDC:getSize(Dc),
	Position = Aircraft#aircraft.position,
	X = trunc((180 + Position#vector.lat) * ScreenWidth / 360.0), 
	Y = trunc((90 - Position#vector.lon) * ScreenHeight / 180.0),
	
	case Aircraft#aircraft.status of
		safe ->
			wxPaintDC:setBrush(Dc, ?SCREEN_AIRCRAFT_CLEAR_BRUSH),
			wxPaintDC:setPen(Dc, ?SCREEN_AIRCRAFT_CLEAR_PEN);
		danger ->
			wxPaintDC:setBrush(Dc, ?SCREEN_AIRCRAFT_DANGER_BRUSH),
			wxPaintDC:setPen(Dc, ?SCREEN_AIRCRAFT_DANGER_PEN)
	end,
	
	wxPaintDC:setFont(Dc, ?SCREEN_AIRCRAFT_INFO_FONT),
	wxPaintDC:setTextForeground(Dc, ?SCREEN_AIRCRAFT_INFO_COLOR),
	
	wxPaintDC:drawCircle(Dc, {X, Y}, ?SCREEN_AIRCRAFT_SIZE),
	
	Speed = Aircraft#aircraft.speed,
	SpeedNorm = util:norm(Speed),
	if 
		SpeedNorm > 0.0 ->
			VX = trunc(?SCREEN_AIRCRAFT_VECTOR_SIZE * Speed#vector.lat / SpeedNorm),
			VY = trunc(?SCREEN_AIRCRAFT_VECTOR_SIZE * Speed#vector.lon / SpeedNorm),
			wxPaintDC:drawLine(Dc, {X, Y}, {X + VX, Y - VY});
		true -> ok
	end,
	
	{_, FontHeight} = wxPaintDC:getTextExtent(Dc, "A"),
	wxPaintDC:drawText(Dc, Aircraft#aircraft.squawk, {X + FontHeight, Y + FontHeight}),
	wxPaintDC:drawText(Dc, io_lib:format("~.2f deg",[Aircraft#aircraft.heading]), {X + FontHeight, Y + 2 * FontHeight}),
	wxPaintDC:drawText(Dc, io_lib:format("lat=~.2f lon=~.2f",[Position#vector.lat, Position#vector.lon]), {X + FontHeight, Y + 3 * FontHeight}),
	wxPaintDC:drawText(Dc, io_lib:format("~.2f deg/s",[SpeedNorm]), {X + FontHeight, Y + 4 * FontHeight}),
	wxPaintDC:drawText(Dc, io_lib:format("~.2f m",[Aircraft#aircraft.altitude]), {X + FontHeight, Y + 5 * FontHeight}).

