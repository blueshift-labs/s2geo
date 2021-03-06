-module(s2cellunion).

-include("s2geo_internals.hrl").
-include("s2cellunion_internals.hrl").

%% API exports
-export([
  new_from_cellids_list/1,
  new_from_integer_list/1,
  new_from_cellids_range/2,
  encode/1,
  decode/1,
  num_cells/1,
  cellid/2,
  cellids/1,
  contains/2,
  intersects/2,
  get_union/2,
  get_intersection/2,
  get_difference/2,
  expand/2,
  expand/3,
  leaf_cells_covered/1,
  average_based_area/1,
  approx_area/1,
  exact_area/1,
  get_rect_bound/1,
  may_intersect/2,

  get_covering/1,
  get_covering/2,
  get_covering/3,
  get_interior_covering/3,
  get_cellunion_covering/3,
  get_cellunion_interior_covering/3
  ]).

get_cellids_from_list([{s2cellid, CellId}]) -> [CellId];
get_cellids_from_list([{s2cellid, CellId}|Rest]) -> [CellId | get_cellids_from_list(Rest)].

new_from_cellids_list(S2CellIdsList) when is_list(S2CellIdsList) ->
  S2CellUnionRef = s2geo_nif:s2cellunion_constructor(?S2CELLUNIONCONSTRUCTORS_INIT_FROM_CELLIDS, get_cellids_from_list(S2CellIdsList)),
  {s2cellunion, S2CellUnionRef}.

new_from_integer_list(Uint64List) when is_list(Uint64List) ->
  S2CellUnionRef = s2geo_nif:s2cellunion_constructor(?S2CELLUNIONCONSTRUCTORS_INIT_FROM_UINT64, Uint64List),
  {s2cellunion, S2CellUnionRef}.

new_from_cellids_range({s2cellid, S2CellIdMin}, {s2cellid, S2CellIdMax}) ->
  S2CellUnionRef = s2geo_nif:s2cellunion_constructor(?S2CELLUNIONCONSTRUCTORS_INIT_FROM_RANGE, S2CellIdMin, S2CellIdMax),
  {s2cellunion, S2CellUnionRef}.

encode({s2cellunion, S2CellUnionRef}) when is_reference(S2CellUnionRef) ->
   s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_ENCODE).

decode(String) when is_binary(String) ->
  S2CellUnionRef = s2geo_nif:s2cellunion_constructor(?S2CELLUNIONCONSTRUCTORS_DECODE, String),
  {s2cellunion, S2CellUnionRef}.


num_cells({s2cellunion, S2CellUnionRef}) when is_reference(S2CellUnionRef) ->
   s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_NUM_CELLS).

cellid({s2cellunion, S2CellUnionRef}, Index) when is_reference(S2CellUnionRef) and is_integer(Index) ->
   s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_CELL_ID, Index).


cellids({s2cellunion, S2CellUnionRef}) when is_reference(S2CellUnionRef) ->
   s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_CELL_IDS).

contains({s2cellunion, S2CellUnionRef}, {s2cellunion, S2CellUnionOtherRef}) when is_reference(S2CellUnionRef) and is_reference(S2CellUnionOtherRef) ->
   s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_CONTAINS_S2CELLUNION, S2CellUnionOtherRef);
contains({s2cellunion, S2CellUnionRef}, {s2cellid, S2CellId}) when is_reference(S2CellUnionRef) ->
   s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_CONTAINS_S2CELLID, S2CellId);
contains({s2cellunion, S2CellUnionRef}, {s2point, S2Point}) when is_reference(S2CellUnionRef) ->
   s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_CONTAINS_S2POINT, S2Point).

intersects({s2cellunion, S2CellUnionRef}, {s2cellunion, S2CellUnionOtherRef}) when is_reference(S2CellUnionRef) and is_reference(S2CellUnionOtherRef) ->
   s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_INTERSECTS_S2CELLUNION, S2CellUnionOtherRef);
intersects({s2cellunion, S2CellUnionRef}, {s2cellid, S2CellId}) when is_reference(S2CellUnionRef) ->
   s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_INTERSECTS_S2CELLID, S2CellId).


get_union({s2cellunion, S2CellUnionRef}, {s2cellunion, S2CellUnionOtherRef}) when is_reference(S2CellUnionRef) and is_reference(S2CellUnionOtherRef) ->
   NewS2CellUnionRef = s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_GET_UNION, S2CellUnionOtherRef),
   {s2cellunion, NewS2CellUnionRef}.

get_intersection({s2cellunion, S2CellUnionRef}, {s2cellunion, S2CellUnionOtherRef}) when is_reference(S2CellUnionRef) and is_reference(S2CellUnionOtherRef) ->
   NewS2CellUnionRef = s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_GET_INTERSECTION, S2CellUnionOtherRef),
   {s2cellunion, NewS2CellUnionRef};
get_intersection({s2cellunion, S2CellUnionRef}, {s2cellid, S2CellId}) when is_reference(S2CellUnionRef) ->
   NewS2CellUnionRef = s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_GET_INTERSECTION_WITH_S2CELLID, S2CellId),
   {s2cellunion, NewS2CellUnionRef}.

get_difference({s2cellunion, S2CellUnionRef}, {s2cellunion, S2CellUnionOtherRef}) when is_reference(S2CellUnionRef) and is_reference(S2CellUnionOtherRef) ->
   NewS2CellUnionRef = s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_GET_DIFFERENCE, S2CellUnionOtherRef),
   {s2cellunion, NewS2CellUnionRef}.

expand({s2cellunion, S2CellUnionRef}, Level) when is_reference(S2CellUnionRef) and is_integer(Level) ->
   NewS2CellUnionRef = s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_EXPAND_LEVEL, Level),
   {s2cellunion, NewS2CellUnionRef}.

expand({s2cellunion, S2CellUnionRef}, {s1angle, MinRadius}, MaxLevelDiff) when is_reference(S2CellUnionRef) and is_integer(MaxLevelDiff) ->
   NewS2CellUnionRef = s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_EXPAND_RADIUS, MinRadius, MaxLevelDiff),
   {s2cellunion, NewS2CellUnionRef}.

leaf_cells_covered({s2cellunion, S2CellUnionRef}) when is_reference(S2CellUnionRef) ->
   s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_LEAF_CELLS_COVERED).

average_based_area({s2cellunion, S2CellUnionRef}) when is_reference(S2CellUnionRef) ->
   s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_AVERAGE_BASED_AREA).

approx_area({s2cellunion, S2CellUnionRef}) when is_reference(S2CellUnionRef) ->
   s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_APPROX_AREA).

exact_area({s2cellunion, S2CellUnionRef}) when is_reference(S2CellUnionRef) ->
   s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_EXACT_AREA).

get_rect_bound({s2cellunion, S2CellUnionRef}) when is_reference(S2CellUnionRef) ->
   S2LatLngRectRef = s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_GET_RECT_BOUND),
   {s2latlngrect, S2LatLngRectRef}.

may_intersect({s2cellunion, S2CellUnionRef}, {s2cellid, S2CellId}) when is_reference(S2CellUnionRef) ->
   s2geo_nif:s2cellunion_methods(S2CellUnionRef, ?S2CELLUNIONMETHODS_MAY_INTERSECT_S2CELL, S2CellId).



get_covering({s2cellunion, Ref}) when is_reference(Ref) ->
   Covering = s2geo_nif:s2cellunion_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, 0),
   {covering, Covering}.

get_covering({s2cellunion, Ref}, MaxCells) when is_reference(Ref), is_integer(MaxCells) ->
   Covering = s2geo_nif:s2cellunion_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, MaxCells),
   {covering, Covering};
get_covering({s2cellunion, Ref}, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2cellunion_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, 0, {MinLevel, MaxLevel}),
   {covering, Covering}.

get_covering({s2cellunion, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2cellunion_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, MaxCells, {MinLevel, MaxLevel}),
   {covering, Covering}.

get_interior_covering({s2cellunion, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2cellunion_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_INTERIOR_COVERING, MaxCells, {MinLevel, MaxLevel}),
   {covering, Covering}.

get_cellunion_covering({s2cellunion, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2cellunion_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELL_UNION_COVERING, MaxCells, {MinLevel, MaxLevel}),
   {covering, Covering}.

get_cellunion_interior_covering({s2cellunion, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2cellunion_get_covering(Ref, ?S2REGIONCOVERINGTYPE_INTERIOR_CELL_UNION_COVERING, MaxCells, {MinLevel, MaxLevel}),
   {covering, Covering}.
