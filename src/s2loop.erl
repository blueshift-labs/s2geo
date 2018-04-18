-module(s2loop).

-include("s2geo_internals.hrl").

-export([
  new/1,
  new_from_s2point_list/1,
  new_from_s2latlng_list/1,

  is_valid/1,
  num_vertices/1,
  vertex/2,
  oriented_vertex/2,
  is_empty/1,
  is_full/1,
  is_empty_or_full/1,
  depth/1,
  set_depth/2,
  is_hole/1,
  sign/1,
  is_normalized/1,
  normalize/1,

  invert/1,

  get_area/1,
  get_centroid/1,
  get_turning_angle/1,
  get_turning_angle_max_error/1,
  get_distance/2,
  get_distance_to_boundary/2,

  project/2,
  project_to_boundary/2,
  contains/2,
  contains_nested/2,
  contains_non_crossing_boundary/3,
  intersects/2,

  equals/2,
  boundary_equals/2,
  boundary_approx_equals/3,
  boundary_near/3,
  compare_boundary/2,

  clone/1,

  get_cap_bound/1,
  get_rect_bound/1,

  may_intersect/2,

  encode/1,
  decode/1

  % get_covering/1,
  % get_covering/2,
  % get_covering/3,
  % get_interior_covering/3,
  % get_cellunion_covering/3,
  % get_cellunion_interior_covering/3
  ]).

new_from_s2point_list(S2PointList) when is_list(S2PointList) ->
  S2LoopRef = s2geo_nif:s2loop_new_from_s2point_list(S2PointList),
  {s2loop, S2LoopRef}.

new_from_s2latlng_list(List) when is_list(List) ->
  S2LoopRef = s2geo_nif:s2loop_new_from_s2latlng_list(List),
  {s2loop, S2LoopRef}.

new({s2cellid, S2CellId}) ->
  S2LoopRef = s2geo_nif:s2loop_new_from_s2cellid(S2CellId),
  {s2loop, S2LoopRef};
new({s2cell, S2CellRef}) when is_reference(S2CellRef)  ->
  S2LoopRef = s2geo_nif:s2loop_new_from_s2cell(S2CellRef),
  {s2loop, S2LoopRef}.

is_valid({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  s2geo_nif:s2loop_is_valid(S2LoopRef).

num_vertices({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  s2geo_nif:s2loop_num_vertices(S2LoopRef).

vertex({s2loop, S2LoopRef}, Index) when is_reference(S2LoopRef) and is_integer(Index) ->
  S2Point = s2geo_nif:s2loop_vertex(S2LoopRef, Index),
  {s2point, S2Point}.

oriented_vertex({s2loop, S2LoopRef}, Index) when is_reference(S2LoopRef) and is_integer(Index) ->
  S2Point = s2geo_nif:s2loop_oriented_vertex(S2LoopRef, Index),
  {s2point, S2Point}.

is_empty({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  s2geo_nif:s2loop_is_empty(S2LoopRef).

is_full({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  s2geo_nif:s2loop_is_full(S2LoopRef).

is_empty_or_full({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  s2geo_nif:s2loop_is_empty_or_full(S2LoopRef).

depth({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  s2geo_nif:s2loop_depth(S2LoopRef).

set_depth({s2loop, S2LoopRef}, Depth) when is_reference(S2LoopRef) and is_integer(Depth) ->
  s2geo_nif:s2loop_set_depth(S2LoopRef, Depth),
  {s2loop, S2LoopRef}.

is_hole({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  s2geo_nif:s2loop_is_hole(S2LoopRef).

sign({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  s2geo_nif:s2loop_sign(S2LoopRef).

is_normalized({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  s2geo_nif:s2loop_is_normalized(S2LoopRef).

normalize({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  s2geo_nif:s2loop_normalize(S2LoopRef),
  {s2loop, S2LoopRef}.

invert({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  NewS2LoopRef = s2geo_nif:s2loop_invert(S2LoopRef),
  {s2loop, NewS2LoopRef}.

get_area({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  s2geo_nif:s2loop_get_area(S2LoopRef).

get_centroid({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  Point = s2geo_nif:s2loop_get_centroid(S2LoopRef),
  {s2point, Point}.

get_turning_angle({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  s2geo_nif:s2loop_get_turning_angle(S2LoopRef).

get_turning_angle_max_error({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  s2geo_nif:s2loop_get_turning_angle_max_error(S2LoopRef).


get_distance({s2loop, S2LoopRef}, {s2point, S2Point}) when is_reference(S2LoopRef) ->
  S1Angle = s2geo_nif:s2loop_get_distance(S2LoopRef, S2Point),
  {s1angle, S1Angle}.

get_distance_to_boundary({s2loop, S2LoopRef}, {s2point, S2Point}) when is_reference(S2LoopRef) ->
  S1Angle = s2geo_nif:s2loop_get_distance_to_boundary(S2LoopRef, S2Point),
  {s1angle, S1Angle}.

project({s2loop, S2LoopRef}, {s2point, S2Point}) when is_reference(S2LoopRef) ->
  S2PointOut = s2geo_nif:s2loop_project(S2LoopRef, S2Point),
  {s2point, S2PointOut}.

project_to_boundary({s2loop, S2LoopRef}, {s2point, S2Point}) when is_reference(S2LoopRef) ->
  S2PointOut = s2geo_nif:s2loop_project_to_boundary(S2LoopRef, S2Point),
  {s2point, S2PointOut}.

contains({s2loop, Ref}, {s2loop, Other}) when is_reference(Ref) and is_reference(Other) ->
  s2geo_nif:s2loop_contains_s2loop(Ref, Other);
contains({s2loop, Ref}, {s2point, X, Y, Z}) when is_reference(Ref) ->
  s2geo_nif:s2loop_contains_s2point(Ref, {X, Y, Z});
contains({s2loop, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) ->
  s2geo_nif:s2loop_contains_s2cellid(Ref, S2CellId);
contains({s2loop, Ref}, {s2cell, S2Cell}) when is_reference(Ref) and is_reference(S2Cell) ->
  s2geo_nif:s2loop_contains_s2cell(Ref, S2Cell).

contains_nested({s2loop, S2LoopRef}, {s2loop, S2LoopRefOther}) when is_reference(S2LoopRef) and is_reference(S2LoopRefOther) ->
  s2geo_nif:s2loop_contains_nested(S2LoopRef, S2LoopRefOther).

contains_non_crossing_boundary({s2loop, S2LoopRefSelf}, {s2loop, S2LoopRefOther},  ReverseB) when is_reference(S2LoopRefSelf) and is_reference(S2LoopRefOther) ->
  s2geo_nif:s2loop_contains_non_crossing_boundary(S2LoopRefSelf, S2LoopRefOther, ReverseB).

intersects({s2loop, S2LoopRefSelf}, {s2loop, S2LoopRefOther}) when is_reference(S2LoopRefSelf) and is_reference(S2LoopRefOther) ->
  s2geo_nif:s2loop_intersects(S2LoopRefSelf, S2LoopRefOther).

equals({s2loop, S2LoopRefSelf}, {s2loop, S2LoopRefOther}) when is_reference(S2LoopRefSelf) and is_reference(S2LoopRefOther) ->
  s2geo_nif:s2loop_equals(S2LoopRefSelf, S2LoopRefOther).

boundary_equals({s2loop, S2LoopRefSelf}, {s2loop, S2LoopRefOther}) when is_reference(S2LoopRefSelf) and is_reference(S2LoopRefOther) ->
  s2geo_nif:s2loop_boundary_equals(S2LoopRefSelf, S2LoopRefOther).

boundary_approx_equals({s2loop, S2LoopRefSelf}, {s2loop, S2LoopRefOther}, {s1angle, MaxError}) when is_reference(S2LoopRefSelf) and is_reference(S2LoopRefOther) ->
  s2geo_nif:s2loop_boundary_approx_equals(S2LoopRefSelf, S2LoopRefOther, MaxError).

boundary_near({s2loop, S2LoopRefSelf}, {s2loop, S2LoopRefOther}, {s1angle, MaxError}) when is_reference(S2LoopRefSelf) and is_reference(S2LoopRefOther) ->
  s2geo_nif:s2loop_boundary_near(S2LoopRefSelf, S2LoopRefOther, MaxError).

compare_boundary({s2loop, S2LoopRefSelf}, {s2loop, S2LoopRefOther}) when is_reference(S2LoopRefSelf) and is_reference(S2LoopRefOther) ->
  s2geo_nif:s2loop_compare_boundary(S2LoopRefSelf, S2LoopRefOther).


clone({s2loop, S2LoopRef})  when is_reference(S2LoopRef) ->
  S2PointOut = s2geo_nif:s2loop_clone(S2LoopRef),
  {s2point, S2PointOut}.

get_cap_bound({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  S2Cap = s2geo_nif:s2loop_get_cap_bound(S2LoopRef),
  {s2cap, S2Cap}.

get_rect_bound({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
  S2LatLngRect = s2geo_nif:s2loop_get_rect_bound(S2LoopRef),
  {s2latlngrect, S2LatLngRect}.

may_intersect({s2loop, S2LoopRef}, {s2cellid, S2Cellid}) when is_reference(S2LoopRef) ->
  s2geo_nif:s2loop_may_intersect_s2cellid(S2LoopRef, S2Cellid);
may_intersect({s2loop, S2LoopRef}, {s2cell, S2Cell}) when is_reference(S2LoopRef) and is_reference(S2Cell) ->
  s2geo_nif:s2loop_may_intersect_s2cell(S2LoopRef, S2Cell).


encode({s2loop, S2LoopRef}) when is_reference(S2LoopRef) ->
   s2geo_nif:s2loop_encode(S2LoopRef).

decode(String) when is_binary(String) ->
  S2LoopRef = s2geo_nif:s2loop_decode(String),
  {s2loop, S2LoopRef}.


% get_covering({s2loop, Ref}) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2loop_(Ref, 0),
%    {covering, Covering}.

% get_covering({s2loop, Ref}, MaxCells) when is_reference(Ref), is_integer(MaxCells) ->
%    Covering = s2geo_nif:s2loop_(Ref, MaxCells),
%    {covering, Covering};
% get_covering({s2loop, Ref}, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2loop_(Ref, 0, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_covering({s2loop, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2loop_(Ref, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_interior_covering({s2loop, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2loop_(Ref, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_cellunion_covering({s2loop, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2loop_(Ref, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_cellunion_interior_covering({s2loop, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2loop_(Ref, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.

