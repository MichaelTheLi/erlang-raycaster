%%%-------------------------------------------------------------------
%%% @author michaellipinski
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2017 23:04
%%%-------------------------------------------------------------------
-module(raycaster).
-author("michaellipinski").

-export([castRay/3]).

castRay(Map, Pos={PosX, PosY}, Angle) ->
  AngleInRads = Angle * 3.1415926535 / 180,
  {AngleWithX, NormalDirX} = case math:cos(AngleInRads) >= 0 of
    true -> {90 - Angle, 1};
    false -> {90 - Angle, -1}
  end ,
  {AngleWithY, NormalDirY} = case math:sin(AngleInRads) >= 0 of
     true -> {Angle, -1};
     false -> {Angle, 1}
  end ,
  {NewP1, Wall1} = findX(Map, Pos, {NormalDirX, NormalDirY}, AngleInRads),
  {NewP2, Wall2} = findY(Map, Pos, {NormalDirX, NormalDirY}, AngleInRads),

  getMinRay({Pos, NewP1, Wall1}, {Pos, NewP2, Wall2}).

getMinRay({P1, NewP1, Wall1}, {P2, NewP2, Wall2}) ->
  XLength = getRayLength(P1, NewP1),
  YLength = getRayLength(P2, NewP2),

  case XLength > YLength of
     true   ->
       {X, Y} = NewP2,
       {0, NewP2, YLength, Wall2};
     false  ->
       {X, Y} = NewP1,
       {1, NewP1, XLength, Wall1}
  end.

getRayLength(P1={PX, PY}, NewP1 = {PX1, PY2}) ->
  math:sqrt(math:pow(PX - PX1, 2) + math:pow(PY - PY2, 2)).

findX(Map, Pos = {PosX, PosY}, Dir, Angle) ->
  {NextX, NextY, Xa, Ya} = findFirstByX(Pos, Dir, Angle),
  forwardBy(Map, {NextX, NextY}, {Xa, Ya}).

findY(Map, Pos = {PosX, PosY}, Dir, Angle) ->
  {NextX, NextY, Xa, Ya} = findFirstByY(Pos, Dir, Angle),
  forwardBy(Map, {NextX, NextY}, {Xa, Ya}).

findFirstByX({PX, PY}, {DirX, DirY}, Angle) ->
  G = 1,
  Margin = G / 10000,
  NextX = case DirX of
    -1  -> floor(PX / G) * G - Margin;
    1   -> floor(PX / G) * G + G + Margin
  end,
  NextY = PY + (PX - NextX) * (math:tan(Angle) + Margin),
  Xa = G * DirX,
  Ya = abs(Xa * (math:tan(Angle) + Margin)),
  {NextX, NextY, Xa, DirY * Ya}.

findFirstByY({PX, PY}, {DirX, DirY}, Angle) ->
  G = 1,
  Margin = G / 10000,
  NextY = case DirY of
    -1  -> floor(PY / G) * G - Margin;
    1   -> floor(PY / G) * G + G + Margin
  end,
  NextX = PX + (PY - NextY) / (math:tan(Angle) + Margin),
  Ya = G * DirY,
  Xa = abs( Ya / (math:tan(Angle) + Margin)),
  {NextX, NextY, DirX * Xa, Ya}.


forwardBy(Map, Pos={X, Y}, Step={DX, DY}) when X > 0, X =< 25, Y > 0, Y =< 25 ->
  PosX = normalizeValue(floor(X)),
  PosY = normalizeValue(floor(Y)),
  MapPos = {PosX + 1, PosY + 1},
  Wall = try getCell(Map, MapPos) of
           _ -> getCell(Map, MapPos)
         catch
           _:_ -> ok
         end,
  case Wall > 0 of
    false -> forwardBy(Map, {X + DX, Y + DY}, Step);
    true -> {Pos, Wall}
  end;
forwardBy(Map, Pos={X, Y}, Step) ->
  PosX = normalizeValue(X),
  PosY = normalizeValue(Y),
  MapPos = {PosX, PosY},
  {MapPos, ok}.

normalizeValue(Value) ->
  min(max(Value, 0), 25).

getCell([], MapPos={X, Y}) ->
  1;
getCell(Map, MapPos={X, Y}) when is_list(Map)->
  Column = lists:nth(Y, Map),
  lists:nth(X, Column);

getCell(Map, MapPos={X, Y})->
  {ok, Value} = dict:find({Y, X}, Map),
  Value.


floor(X) when X < 0 ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T - 1
  end;
floor(X) ->
  trunc(X).

ceiling(X) when X < 0 ->
  trunc(X);
ceiling(X) ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T + 1
  end.
