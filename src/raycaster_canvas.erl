%%%-------------------------------------------------------------------
%%% @author michaellipinski
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Apr 2017 20:48
%%%-------------------------------------------------------------------
-module(raycaster_canvas).
-author("michaellipinski").

-define(DELAY, 40).

-behaviour(wx_object).

%% Client API
-export([start/1]).

%% wx_object callbacks
-export([init/1, terminate/2, code_change/3,
  handle_info/2, handle_call/3, handle_event/2, handle_sync_event/3, handle_cast/2]).

-include_lib("wx/include/wx.hrl").

-record(state,
{
  parent,
  config,
  canvas,
  bitmap,
  pen,
  pos,
  angle,
  map
}).


start(Config) ->
  wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Init routine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
  wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
  Parent = proplists:get_value(parent, Config),
  RawMap = proplists:get_value(map, Config),
  Map = map:convertMapToDict(RawMap),

  Panel = wxPanel:new(Parent, []),

  %% Setup sizers
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "Raycasting"}]),

  %% Create the window to paint on and make it repaint the whole window on resize
  Canvas = wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),

  wxPanel:connect(Canvas, paint, [callback]),
  wxPanel:connect(Canvas, size),
  wxPanel:connect(Canvas, key_down),

  %% Add to sizers
  wxSizer:add(Sizer, Canvas, [{flag, ?wxEXPAND},
    {proportion, 1}]),

  wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND},
    {proportion, 1}]),

  wxPanel:setSizer(Panel, MainSizer),
  wxSizer:layout(MainSizer),

  {W,H} = wxPanel:getSize(Canvas),
  Bitmap = wxBitmap:new(erlang:max(W,30),erlang:max(30,H)),

  Pos = proplists:get_value(pos, Config),
  Angle = proplists:get_value(angle, Config),

  erlang:send_after(?DELAY, self(), render),

  {Panel, #state{parent=Panel, config=Config, canvas = Canvas, bitmap = Bitmap, pos=Pos, map=Map, angle=Angle}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sync event from callback events, paint event must be handled in callbacks
%% otherwise nothing will be drawn on windows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_sync_event(#wx{event = #wxPaint{}}, _wxObj, #state{canvas=Canvas, bitmap=Bitmap}) ->
  DC = wxPaintDC:new(Canvas),
  redraw(DC, Bitmap),
  wxPaintDC:destroy(DC),
  ok.

%% Resize event
handle_event(#wx{event = #wxSize{size = {W,H}}}, State = #state{bitmap=Prev}) ->
  Bitmap = wxBitmap:new(W,H),
  draw(State#state.canvas, Bitmap, fun(DC) -> wxDC:clear(DC) end),
  wxBitmap:destroy(Prev),
  {noreply, State#state{bitmap=Bitmap}};

%% Keys handler
handle_event(#wx{event=#wxKey{rawCode = KeyCode, type = key_down}}, State = #state{pos=Pos, angle=Angle}) ->
  AngleForPos = -Angle,
  {StepAngle, NewAngle} = case KeyCode of
    0 ->
      {AngleForPos - 90, Angle};  %% A
    2 ->
      {AngleForPos + 90, Angle};  %% D
    13 ->
      {AngleForPos, Angle};       %% W
    1 ->
      {AngleForPos - 180, Angle}; %% S
    123 ->
      {0, Angle + 5};      %% Left arrow
    124 ->
      {0, Angle - 5};      %% Right arrow
    _ ->
      {0, Angle}
  end,
  StepLength = 0.3,
  Bounds  = {0, 0, 24, 24},
  NewPos = case StepAngle of
     0 -> Pos;
     _ -> calcNewPosition(Pos, StepAngle, StepLength, Bounds)
   end,
  NewState = State#state{pos=NewPos, angle=NewAngle},
  {noreply, NewState};

handle_event(Ev = #wx{}, State = #state{}) ->
  demo:format(State#state.config, "Got Event ~p\n", [Ev]),
  {noreply, State}.

handle_info(render, State = #state{canvas=Canvas, bitmap=Bitmap, angle = Angle}) ->
  real_draw(State),
  erlang:send_after(?DELAY, self(), render),
  {noreply, State};

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
  demo:format(State#state.config, "Got Info ~p\n", [Msg]),
  {noreply, State}.

handle_call(Msg, _From, State) ->
  demo:format(State#state.config, "Got Call ~p\n", [Msg]),
  {reply,{error, nyi}, State}.

handle_cast({test}, State) ->
  io:format("Test cast received~n"),
  {noreply, State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _State) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calcNewPosition({X, Y}, StepAngle, StepLength, {MinX, MinY, MaxX, MaxY}) ->
  StepAngleRads = StepAngle * 3.14 / 180,
  DX = StepLength * math:cos(StepAngleRads),
  DY = StepLength * math:sin(StepAngleRads),
  NewX = X + DX,
  NewY = Y + DY,
  {
    min(MaxX, max(MinX, NewX)),
    min(MaxY, max(MinY, NewY))
  }.

drawRaytracerStuff(DC, State) ->
  {W, H} = wxPanel:getSize(State#state.canvas),
  Map = State#state.map,

  {PosX, PosY} = State#state.pos,
  Angle = State#state.angle,

  wxDC:clear(DC),
  Columns = drawColumn(DC, Map, W, H, 0, State#state.pos, State#state.angle, []),

  %% DEBUG DRAWINGS
  drawDebugData(DC, Map, Columns, State#state.pos, State#state.angle),

  PositionText = io_lib:format("~4.1f | ~4.1f~n", [round(PosX * 10) / 10, round(PosY * 10) / 10]),
  AngleText = io_lib:format("~6.1f~n", [round(Angle * 10) / 10]),
  wxDC:drawText(DC, PositionText, {W - 100, 10}),
  wxDC:drawText(DC, AngleText, {W - 100, 20}).

real_draw(State) ->
  Fun = fun(DC) ->
    drawRaytracerStuff(DC, State)
  end,
  draw(State#state.canvas, State#state.bitmap, Fun),

  wxWindow:refresh(State#state.parent,[{eraseBackground,false}]).

drawColumn(DC, Map, WinW, WinH, X, Pos, Angle, Columns) ->
  {DistanceToPlane, {Side, Point, RealColumnHeight, Wall}} = getColumnData(Map, WinW, WinH, X, Pos, Angle),
  G = 1,
  ColumnHeight = case RealColumnHeight > 0 of
    true -> (G / RealColumnHeight) * DistanceToPlane;
    false -> 0
  end,
  DeltaX = 1,
  DeltaY = ColumnHeight,
  Y = (WinH - ColumnHeight) / 2,

  Color1 = map:getColor(Wall, ?wxWHITE),

  Color = case Side of
    1 -> Color1;
    0 -> lighter(Color1, 0.5)
  end,

  Brush = wxBrush:new(),
  wxBrush:setColour(Brush, Color),
  wxDC:setBrush(DC, Brush),

  NewColumns = lists:append([{Wall, Point}], Columns),
%%  Pen = wxPen:new(),
%%  wxPen:setColour(Pen, ?wxTRANSPARENT),
  Pen = case Color1 == ?wxWHITE of
    true  -> ?wxBLACK_PEN;
    false -> ?wxTRANSPARENT_PEN
  end,
  wxDC:setPen(DC, Pen),
  wxDC:drawRectangle(DC, {round(X), round(Y)}, {round(DeltaX), round(DeltaY)}),
  case (X + DeltaX) >= WinW of
    false ->
      drawColumn(DC, Map, WinW, WinH, X + DeltaX, Pos, Angle, NewColumns);
    _ ->
      NewColumns
  end.

lighter({R, G, B, Opacity}, Percent) ->
  {R, G, B, round(Opacity * Percent)}.

getColumnData(Map, WinW, WinH, X, {PosX, PosY}, POVAngle) ->
  Fov = 60,
  AnglePerRay = 60 / WinW,
  HalfOfFov = Fov / 2,
  DistanceToPlane = (WinW / 2) / math:tan(HalfOfFov * 3.14 / 180),
  Angle = POVAngle  - (X * AnglePerRay - Fov / 2),
  CastedRay = raycaster:castRay(Map, {PosX, PosY}, Angle),

  {DistanceToPlane, CastedRay}.

drawDebugData(DC, Map, Hits, Pos, Angle) ->
  Config = {{5,5}, 40, 5, 1},
  debug_drawer:draw_debug(DC, {Map, 24, 24}, Hits, Pos, Angle, Config).

draw(Canvas, Bitmap, Fun) ->
  MemoryDC = wxMemoryDC:new(Bitmap),
  Fun(MemoryDC),

  CDC = wxWindowDC:new(Canvas),
  wxDC:blit(CDC, {0,0}, {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)}, MemoryDC, {0,0}),
  wxWindowDC:destroy(CDC),
  wxMemoryDC:destroy(MemoryDC).

redraw(DC, Bitmap) ->
  MemoryDC = wxMemoryDC:new(Bitmap),
  wxDC:blit(DC, {0,0},
    {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
    MemoryDC, {0,0}),
  wxMemoryDC:destroy(MemoryDC).

