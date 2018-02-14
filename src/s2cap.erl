-module(s2cap).

-include("s2geo_internals.hrl").
-include("s2cap.hrl").

-export([
  new_from_axis_height/2,
  new_from_axis_angle/2,
  new_from_axis_area/2,
  empty/0,
  full/0,

  clone/1,
  axis/1,
  height/1,
  area/1,
  angle/1,
  is_valid/1,
  is_empty/1,
  is_full/1,
  complement/1,

  contains/2,
  intersects/2,
  interior_intersects/2,
  interior_contains/2,
  add_point/2,
  add_cap/2,
  expanded/2,

  get_cap_bound/1,
  get_rect_bound/1,

  contains_s2cellid/2,
  may_intersect_s2cellid/2,
  contains_s2point/2,

  get_covering/1,
  get_covering/2,
  get_covering/3,
  get_interior_covering/3,
  get_cellunion_covering/3,
  get_cellunion_interior_covering/3
  ]).

%%====================================================================
%% API functions
%%====================================================================

new_from_axis_height({s2point, {X, Y, Z}}, Height) when is_float(X) and is_float(Y) and is_float(Z) and is_float(Height) and (Height =< 2.0) ->
  S2Cap = s2geo_nif:s2cap_constructor(?S2CAP_CONSTRUCTORS_FROM_AXIS_HEIGHT, {X, Y, Z}, Height),
  {s2cap, S2Cap}.

new_from_axis_angle({s2point, {X, Y, Z}}, {s1angle, Angle}) when is_float(X) and is_float(Y) and is_float(Z) and is_float(Angle) ->
  S2Cap = s2geo_nif:s2cap_constructor(?S2CAP_CONSTRUCTORS_FROM_AXIS_ANGLE, {X, Y, Z}, Angle),
  {s2cap, S2Cap}.

new_from_axis_area({s2point, {X, Y, Z}}, Area) when is_float(X) and is_float(Y) and is_float(Z) and is_float(Area) ->
  S2Cap = s2geo_nif:s2cap_constructor(?S2CAP_CONSTRUCTORS_FROM_AXIS_AREA, {X, Y, Z}, Area),
  {s2cap, S2Cap}.

empty() ->
  S2Cap = s2geo_nif:s2cap_constructor(?S2CAP_CONSTRUCTORS_EMPTY),
  {s2cap, S2Cap}.

full() ->
  S2Cap = s2geo_nif:s2cap_constructor(?S2CAP_CONSTRUCTORS_FULL),
  {s2cap, S2Cap}.

clone({s2cap, Ref}) when is_reference(Ref) ->
  {s2cap, s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_CLONE)}.

axis({s2cap, Ref}) when is_reference(Ref) ->
  {s2point, s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_AXIS)}.

height({s2cap, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_HEIGHT).

area({s2cap, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_AREA).

angle({s2cap, Ref}) when is_reference(Ref) ->
  {s1angle, s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_ANGLE)}.

is_valid({s2cap, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_IS_VALID).

is_empty({s2cap, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_IS_EMPTY).

is_full({s2cap, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_IS_FULL).

complement({s2cap, Ref}) when is_reference(Ref) ->
  {s2cap, s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_COMPLEMENT)}.


contains({s2cap, Ref}, {s2cap, Other}) when is_reference(Ref) and is_reference(Other) ->
  s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_CONTAINS, Other).

intersects({s2cap, Ref}, {s2cap, Other}) when is_reference(Ref) and is_reference(Other) ->
  s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_INTERSECTS, Other).

interior_intersects({s2cap, Ref}, {s2cap, Other}) when is_reference(Ref) and is_reference(Other) ->
  s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_INTERIOR_INTERSECTS, Other).

interior_contains({s2cap, Ref}, {s2point, {X, Y, Z}}) when is_reference(Ref) and is_float(X) and is_float(Y) and is_float(Z) ->
  s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_INTERIOR_CONTAINS, {X, Y, Z}).

add_point({s2cap, Ref}, {s2point, {X, Y, Z}}) when is_reference(Ref) and is_float(X) and is_float(Y) and is_float(Z) ->
  {s2cap, s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_ADD_POINT, {X, Y, Z})}.

add_cap({s2cap, Ref}, {s2cap, Other}) when is_reference(Ref) and is_reference(Other) ->
  {s2cap, s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_ADD_CAP, Other)}.

expanded({s2cap, Ref}, {s1angle, Distance}) when is_reference(Ref) ->
  {s2cap, s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_ADD_CAP, Distance)}.


get_cap_bound({s2cap, Ref}) when is_reference(Ref) ->
  {s2cap, s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_GET_CAP_BOUND)}.

get_rect_bound({s2cap, Ref}) when is_reference(Ref) ->
  {s2latlngrect, s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_GET_RECT_BOUND)}.

contains_s2cellid({s2cap, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) and is_integer(S2CellId) ->
  s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_CONTAINS_S2CELLID, S2CellId).

may_intersect_s2cellid({s2cap, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) and is_integer(S2CellId) ->
  s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_MAY_INTERSECT_S2CELLID, S2CellId).

contains_s2point({s2cap, Ref}, {s2point, {X, Y, Z}}) when is_reference(Ref) and is_float(X) and is_float(Y) and is_float(Z) ->
  s2geo_nif:s2cap_methods(Ref, ?S2CAP_METHODS_CONTAINS_S2POINT, {X, Y, Z}).



get_covering({s2cap, Ref}) when is_reference(Ref) ->
   Covering = s2geo_nif:s2cap_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, 0),
   {covering, Covering}.

get_covering({s2cap, Ref}, MaxCells) when is_reference(Ref), is_integer(MaxCells) ->
   Covering = s2geo_nif:s2cap_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, MaxCells),
   {covering, Covering};
get_covering({s2cap, Ref}, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2cap_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, 0, {MinLevel, MaxLevel}),
   {covering, Covering}.

get_covering({s2cap, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2cap_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, MaxCells, {MinLevel, MaxLevel}),
   {covering, Covering}.

get_interior_covering({s2cap, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2cap_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_INTERIOR_COVERING, MaxCells, {MinLevel, MaxLevel}),
   {covering, Covering}.

get_cellunion_covering({s2cap, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2cap_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELL_UNION_COVERING, MaxCells, {MinLevel, MaxLevel}),
   {covering, Covering}.

get_cellunion_interior_covering({s2cap, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2cap_get_covering(Ref, ?S2REGIONCOVERINGTYPE_INTERIOR_CELL_UNION_COVERING, MaxCells, {MinLevel, MaxLevel}),
   {covering, Covering}.
