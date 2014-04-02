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
%% Description: Console
%%

-module(console).
-author('Babacar Ndiaye <babacar@ndiaye.name>').

-include_lib("wx/include/wx.hrl").

-include("config.hrl").

-export([new/0, start/0]).

new() ->
	Pid = spawn_link(console, start, []),
	timer:send_interval(?SCREEN_REFRESH_RATE, Pid, {update}),
	Pid.

start() ->
	Frame = wxFrame:new(wx:new(), ?wxID_ANY, "World map", [{pos, {50,60}}, {size, {750, 500}}]),
	Screen = screen:new(Frame),
	
	File = wxMenu:new(),
	wxMenu:append(File, ?wxID_ABOUT, "About"),
	wxMenu:append(File, ?wxID_EXIT, "Quit"),
	MenuBar = wxMenuBar:new(),
	wxMenuBar:append(MenuBar, File, "&File"),	
	wxFrame:setMenuBar(Frame, MenuBar),
	
	wxFrame:createStatusBar(Frame),
	wxFrame:setStatusText(Frame, "Welcome to World Map"),
	wxFrame:connect(Frame, command_menu_selected),
	wxFrame:connect(Frame, close_window),
	wxFrame:connect(Frame, command_button_clicked),
	wxFrame:show(Frame),
	
	loop(Frame, Screen, [], []).

loop(Frame, Screen, Aircrafts, Segments) ->
	receive
		#wx{event=#wxClose{type=close_window}} ->
			wxFrame:destroy(Frame),
			wx:destroy(),
			ok;

		#wx{id=?wxID_ABOUT, event=_} ->
			Str = "MicroBlog is a minimal WxErlang example.",
			Dialog = wxMessageDialog:new(Frame, Str, [{style, ?wxOK bor ?wxICON_INFORMATION}, {caption, "About MicroBlog"}]),
			wxDialog:showModal(Dialog),
			wxDialog:destroy(Dialog),
			loop(Frame, Screen, Aircrafts, Segments);
		
		#wx{id=?wxID_EXIT, event=_} ->
			wxWindow:close(Frame,[]),
			loop(Frame, Screen, Aircrafts, Segments);
		
		{update} ->
			wxWindow:refresh(Screen),
			loop(Frame, Screen, Aircrafts, Segments);
		
		{register, aircrafts, NewAircrafts} -> 
			init(Screen, NewAircrafts ++ Aircrafts, Segments),
			loop(Frame, Screen, NewAircrafts ++ Aircrafts, Segments);
			
		{register, segments, NewSegments} ->
			init(Screen, Aircrafts, NewSegments ++ Segments),
			loop(Frame, Screen, Aircrafts, NewSegments ++ Segments)
	end.

init(Screen, Aircrafts, Segments) ->
	wxFrame:connect(Screen, paint, [{
		callback, 
		fun(_, _) ->
			Dc = wxPaintDC:new(Screen),
			
			screen:draw_horizontal_grids(Dc),
			screen:draw_vertical_grids(Dc),
			
			screen:draw_segments(Dc, Segments),
			AircraftDrawer = fun(Aircraft) ->
				Ref = make_ref(),
				Aircraft ! {status, self(), Ref}, 
				receive {Ref, AircraftStatus} -> screen:draw_aircraft(Dc, AircraftStatus) end
			end,
			lists:map(AircraftDrawer, Aircrafts),
			
			wxPaintDC:destroy(Dc)
		end}]).


