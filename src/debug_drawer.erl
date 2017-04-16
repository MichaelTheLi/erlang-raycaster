%%%-------------------------------------------------------------------
%%% @author michaellipinski
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2017 21:05
%%%-------------------------------------------------------------------
-module(debug_drawer).
-author("michaellipinski").

-include_lib("wx/include/wx.hrl").

%% API
-export([draw_debug/6]).

draw_debug(DC, {Map, SizeX, SizeY}, Hits, _Pos={PX, PY}, Angle, Config) ->
  {{CellWidth, CellHeight}, AngleLength, PlayerCircleSize, HitCircleSize} = Config,
  Colors = dict:fold(fun ({X, Y}, Cell, Acc) ->
    Color = map:getColor(Cell, ?wxWHITE),
    [{X, Y, Color}|Acc]
                     end, [], Map),

  FillMapColor = fun ({X, Y, Color}) ->
    Brush = wxBrush:new(),

    wxBrush:setColour(Brush, Color),
    wxDC:setBrush(DC, Brush),
    wxDC:drawRectangle(DC, {round(Y - 1) * CellWidth, round(X - 1) * CellHeight }, {CellWidth, CellHeight})
                 end,

  %%% GRID COLOR
  wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
  lists:foreach(FillMapColor, Colors),

  %%% GRID LINES
  LineEndX = CellWidth * SizeX,
  LineEndY = CellHeight * SizeY,

  Pen = wxPen:new(),
  wxPen:setColour(Pen, ?wxBLACK),
  wxDC:setPen(DC, Pen),
  wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),

  MarginX = 0,
  MarginY = 0,
  DrawXLines = fun (LineX) ->
    wxDC:drawLine(DC, {LineX, MarginY}, {LineX, LineEndY + MarginY})
   end,
  lists:foreach(DrawXLines, [round(X) * CellWidth || X <- lists:seq(1, SizeX)]),

  DrawYLines = fun (LineY) ->
    wxDC:drawLine(DC, {MarginX, LineY}, {LineEndX + MarginX, LineY})
  end,
  lists:foreach(DrawYLines, [round(Y) * CellHeight || Y <- lists:seq(1, SizeY)]),

  %%% HITS
  DrawHits = fun ({Wall, {X, Y}}) ->
    wxDC:setBrush(DC, ?wxMEDIUM_GREY_BRUSH),
    wxDC:drawCircle(DC, {round((X) * CellWidth), round((Y) * CellHeight)}, HitCircleSize)
             end,
  lists:foreach(DrawHits, Hits),

  %%% ANGLE
  CellPos = {round(PX * CellWidth), round(PY * CellHeight)},
  wxDC:drawCircle(DC, CellPos, PlayerCircleSize),

  RealAngle = Angle + 90,
  AnglePosX = AngleLength * math:sin(RealAngle * 3.1415 / 180),
  AnglePosY = AngleLength * math:cos(RealAngle * 3.1415 / 180),
  {XX, YY} = CellPos,
  wxDC:drawLine(DC, CellPos, {round(XX + AnglePosX), round(YY + AnglePosY)}).
