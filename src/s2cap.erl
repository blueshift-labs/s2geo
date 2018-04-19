-module(s2cap).

-export([
  new/1,
  new/2,
  new_from_center_height/2,
  new_from_center_area/2,
  empty/0,
  full/0,
  clone/1,
  center/1,
  radius/1,
  height/1,
  get_radius/1,
  get_area/1,
  get_centroid/1,
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
  union/2,
  get_cap_bound/1,
  get_rect_bound/1,
  get_cell_union_bound/1,
  may_intersect/2,
  encode/1,
  decode/1,
  equal/2,
  approx_equals/3

  % get_covering/1,
  % get_covering/2,
  % get_covering/3,
  % get_interior_covering/3,
  % get_cellunion_covering/3,
  % get_cellunion_interior_covering/3
  ]).

%%====================================================================
%% API functions
%%====================================================================

new({s2point, {X, Y, Z}}) when is_float(X) and is_float(Y) and is_float(Z) ->
  S2Cap = s2geo_nif:s2cap_from_point({X, Y, Z}),
  {s2cap, S2Cap}.

new({s2point, {X, Y, Z}}, {s1angle, Radius}) when is_float(X) and is_float(Y) and is_float(Z) and is_float(Radius) ->
  S2Cap = s2geo_nif:s2cap_from_s2point_s1angle({X, Y, Z}, Radius),
  {s2cap, S2Cap};
new({s2point, {X, Y, Z}}, {s1chordangle, Radius}) when is_float(X) and is_float(Y) and is_float(Z) and is_float(Radius) ->
  S2Cap = s2geo_nif:s2cap_from_s2point_s1chordangle({X, Y, Z}, Radius),
  {s2cap, S2Cap}.

new_from_center_height({s2point, {X, Y, Z}}, Height) when is_float(X) and is_float(Y) and is_float(Z) and is_float(Height) and (Height =< 2.0) ->
  S2Cap = s2geo_nif:s2cap_from_center_height({X, Y, Z}, Height),
  {s2cap, S2Cap}.

new_from_center_area({s2point, {X, Y, Z}}, Area) when is_float(X) and is_float(Y) and is_float(Z) and is_float(Area) ->
  S2Cap = s2geo_nif:s2cap_from_center_area({X, Y, Z}, Area),
  {s2cap, S2Cap}.

empty() ->
  S2Cap = s2geo_nif:s2cap_empty(),
  {s2cap, S2Cap}.

full() ->
  S2Cap = s2geo_nif:s2cap_full(),
  {s2cap, S2Cap}.

clone({s2cap, Ref}) when is_reference(Ref) ->
  {s2cap, s2geo_nif:s2cap_clone(Ref)}.

center({s2cap, Ref}) when is_reference(Ref) ->
  {s2point, s2geo_nif:s2cap_center(Ref)}.

radius({s2cap, Ref}) when is_reference(Ref) ->
  {s1chordangle, s2geo_nif:s2cap_radius(Ref)}.

height({s2cap, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2cap_height(Ref).

get_radius({s2cap, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2cap_get_radius(Ref).

get_area({s2cap, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2cap_get_area(Ref).

get_centroid({s2cap, Ref}) when is_reference(Ref) ->
  {s2point, s2geo_nif:s2cap_get_centroid(Ref)}.

is_valid({s2cap, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2cap_is_valid(Ref).

is_empty({s2cap, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2cap_is_empty(Ref).

is_full({s2cap, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2cap_is_full(Ref).

complement({s2cap, Ref}) when is_reference(Ref) ->
  {s2cap, s2geo_nif:s2cap_complement(Ref)}.

contains({s2cap, Ref}, {s2cap, Other}) when is_reference(Ref) and is_reference(Other) ->
  s2geo_nif:s2cap_contains(Ref, Other);
contains({s2cap, Ref}, {s2cell, S2CellRef}) when is_reference(Ref) and is_reference(S2CellRef) ->
  s2geo_nif:s2cap_contains_s2cell(Ref, S2CellRef);
contains({s2cap, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) and is_integer(S2CellId) ->
  s2geo_nif:s2cap_contains_s2cellid(Ref, S2CellId);
contains({s2cap, Ref}, {s2point, {X, Y, Z}}) when is_reference(Ref) and is_float(X) and is_float(Y) and is_float(Z) ->
  s2geo_nif:s2cap_contains_s2point(Ref, {X, Y, Z}).

intersects({s2cap, Ref}, {s2cap, Other}) when is_reference(Ref) and is_reference(Other) ->
  s2geo_nif:s2cap_intersects(Ref, Other).

interior_intersects({s2cap, Ref}, {s2cap, Other}) when is_reference(Ref) and is_reference(Other) ->
  s2geo_nif:s2cap_interior_intersects(Ref, Other).

interior_contains({s2cap, Ref}, {s2point, {X, Y, Z}}) when is_reference(Ref) and is_float(X) and is_float(Y) and is_float(Z) ->
  s2geo_nif:s2cap_interior_contains(Ref, {X, Y, Z}).

add_point({s2cap, Ref}, {s2point, {X, Y, Z}}) when is_reference(Ref) and is_float(X) and is_float(Y) and is_float(Z) ->
  {s2cap, s2geo_nif:s2cap_add_point(Ref, {X, Y, Z})}.

add_cap({s2cap, Ref}, {s2cap, Other}) when is_reference(Ref) and is_reference(Other) ->
  {s2cap, s2geo_nif:s2cap_add_cap(Ref, Other)}.

expanded({s2cap, Ref}, {s1angle, Distance}) when is_reference(Ref) ->
  {s2cap, s2geo_nif:s2cap_expanded(Ref, Distance)}.

union({s2cap, Ref}, {s2cap, Other}) when is_reference(Ref) and is_reference(Other) ->
  {s2cap, s2geo_nif:s2cap_union(Ref, Other)}.

get_cap_bound({s2cap, Ref}) when is_reference(Ref) ->
  {s2cap, s2geo_nif:s2cap_get_cap_bound(Ref)}.

get_rect_bound({s2cap, Ref}) when is_reference(Ref) ->
  {s2latlngrect, s2geo_nif:s2cap_get_cap_bound(Ref)}.

get_cell_union_bound({s2cap, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2cap_get_cell_union_bound(Ref).

may_intersect({s2cap, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) and is_integer(S2CellId) ->
  s2geo_nif:s2cap_may_intersect_s2cellid(Ref, S2CellId);
may_intersect({s2cap, Ref}, {s2cell, S2Cell}) when is_reference(Ref) and is_reference(S2Cell) ->
  s2geo_nif:s2cap_may_intersect_s2cell(Ref, S2Cell).

encode({s2cap, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2cap_encode(Ref).

decode(String) when is_binary(String) ->
  s2geo_nif:s2cap_decode(String).

equal({s2cap, Ref}, {s2cap, Other}) when is_reference(Ref) and is_reference(Other) ->
  s2geo_nif:s2cap_equal(Ref, Other).

approx_equals({s2cap, Ref}, {s2cap, Other}, {s1angle, MaxError}) when is_reference(Ref) and is_reference(Other) ->
  s2geo_nif:s2cap_approx_equals(Ref, Other, MaxError).


% get_covering({s2cap, Ref}) when is_reference(Ref) ->
%    Covering = s2geo_nif:get_covering(Ref, 0),
%    {covering, Covering}.

% get_covering({s2cap, Ref}, MaxCells) when is_reference(Ref), is_integer(MaxCells) ->
%    Covering = s2geo_nif:get_covering(Ref, MaxCells),
%    {covering, Covering};
% get_covering({s2cap, Ref}, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:get_covering(Ref, 0, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_covering({s2cap, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:get_covering(Ref, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_interior_covering({s2cap, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:get_covering(Ref, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_cellunion_covering({s2cap, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:get_covering(Ref, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_cellunion_interior_covering({s2cap, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:get_covering(Ref, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.


