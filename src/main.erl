%%%-------------------------------------------------------------------
%%% @author michaellipinski
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2017 23:04
%%%-------------------------------------------------------------------
-module(main).
-author("michaellipinski").

-export([start/0]).

-include_lib("wx/include/wx.hrl").

-define(wxMAC_USE_CORE_GRAPHICS, 1).

start() ->
  Frame = init(),
  loop(Frame).

init() ->
  Wx = wx:new(),
  Frame = wxFrame:new(Wx, -1, "Raycaster #1", [{size, {320 + 22, 200 + 55}}]),

  Config = [
    {parent, Frame},
    {pos, {1.75, 3.25}},
    {angle, 90.0},
    {map, map:getMap1()}
  ],

  ok = wxFrame:connect(Frame, close_window),

  Pid = raycaster_canvas:start(Config),
  wxFrame:show(Frame),
  %% Test cast
  wx_object:cast(Pid, {test}),

  Frame.

loop(Frame) ->
  receive
    #wx{event=#wxClose{}} ->
      wxWindow:destroy(Frame),
      ok;
    Msg ->
      io:format("Unknown message ~n ~p ~n", [Msg]),
      loop(Frame)
end.
