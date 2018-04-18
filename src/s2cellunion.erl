-module(s2cellunion).

-include("s2geo_internals.hrl").

%% API exports
-export([
    new_from_s2cellids/1,
    new_from_normalized_s2cellids/1,
    new_from_integer_list/1,
    new_from_min_max/2,
    new_from_begin_end/2,

    num_cells/1,
    s2cellid/2,
    s2cellids/1,

    is_valid/1,
    is_normalized/1,
    normalize/1,
    denormalize/3,
    pack/1,
    pack/2,

    contains/2,
    intersects/2,
    may_intersect/2,

    union/2,
    intersection/2,
    difference/2,

    expand/2,
    expand/3,
    leaf_cells_covered/1,
    average_based_area/1,
    approx_area/1,
    exact_area/1,

    get_cap_bound/1,
    get_rect_bound/1,

    equal/2,
    not_equal/2,

    encode/1,
    decode/1

  % get_covering/1,
  % get_covering/2,
  % get_covering/3,
  % get_interior_covering/3,
  % get_cellunion_covering/3,
  % get_cellunion_interior_covering/3
  ]).

get_cellids_from_list([{s2cellid, CellId}]) -> [CellId];
get_cellids_from_list([{s2cellid, CellId}|Rest]) -> [CellId | get_cellids_from_list(Rest)].

new_from_s2cellids(S2CellIdsList) when is_list(S2CellIdsList) ->
  Ref = s2geo_nif:s2cellunion_new_from_cellids(get_cellids_from_list(S2CellIdsList)),
  {s2cellunion, Ref}.

new_from_normalized_s2cellids(S2CellIdsList) when is_list(S2CellIdsList) ->
  Ref = s2geo_nif:s2cellunion_new_from_normalized_cellids(get_cellids_from_list(S2CellIdsList)),
  {s2cellunion, Ref}.

new_from_integer_list(Uint64List) when is_list(Uint64List) ->
  Ref = s2geo_nif:s2cellunion_new_from_cellids(Uint64List),
  {s2cellunion, Ref}.

new_from_min_max({s2cellid, S2CellIdMin}, {s2cellid, S2CellIdMax}) ->
  Ref = s2geo_nif:s2cellunion_new_from_min_max(S2CellIdMin, S2CellIdMax),
  {s2cellunion, Ref}.

new_from_begin_end({s2cellid, S2CellIdBegin}, {s2cellid, S2CellIdEnd}) ->
  Ref = s2geo_nif:s2cellunion_new_from_begin_end(S2CellIdBegin, S2CellIdEnd),
  {s2cellunion, Ref}.

num_cells({s2cellunion, Ref}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_num_cells(Ref).

s2cellid({s2cellunion, Ref}, Index) when is_reference(Ref) and is_integer(Index) ->
   s2geo_nif:s2cellunion_cell_id(Ref, Index).

s2cellids({s2cellunion, Ref}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_cell_ids(Ref).

is_valid({s2cellunion, Ref}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_is_valid(Ref).

is_normalized({s2cellunion, Ref}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_is_normalized(Ref).

normalize({s2cellunion, Ref}) when is_reference(Ref) ->
   {s2cellunion, s2geo_nif:s2cellunion_normalize(Ref)}.

denormalize({s2cellunion, Ref}, MinLevel, LevelMod) when is_reference(Ref) and is_integer(MinLevel) and is_integer(LevelMod) ->
  s2geo_nif:s2cellunion_denormalize(Ref, MinLevel, LevelMod).

pack({s2cellunion, Ref}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_pack(Ref, 0).

pack({s2cellunion, Ref}, Excess) when is_reference(Ref) and is_integer(Excess) ->
   s2geo_nif:s2cellunion_pack(Ref, Excess).

contains({s2cellunion, Ref}, {s2cellunion, OtherRef}) when is_reference(Ref) and is_reference(OtherRef) ->
   s2geo_nif:s2cellunion_contains_s2cellunion(Ref, OtherRef);
contains({s2cellunion, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_contains_s2cellid(Ref, S2CellId);
contains({s2cellunion, Ref}, {s2cell, S2CellRef}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_contains_s2cell(Ref, S2CellRef);
contains({s2cellunion, Ref}, {s2point, S2Point}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_contains_s2point(Ref, S2Point).

intersects({s2cellunion, Ref}, {s2cellunion, OtherRef}) when is_reference(Ref) and is_reference(OtherRef) ->
   s2geo_nif:s2cellunion_intersects_s2cellunion(Ref, OtherRef);
intersects({s2cellunion, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_intersects_s2cellid(Ref, S2CellId).

may_intersect({s2cellunion, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_may_intersect_s2cellid(Ref, S2CellId);
may_intersect({s2cellunion, Ref}, {s2cell, S2Cell}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_may_intersect_s2cell(Ref, S2Cell).

union({s2cellunion, Ref}, {s2cellunion, OtherRef}) when is_reference(Ref) and is_reference(OtherRef) ->
   NewRef = s2geo_nif:s2cellunion_union(Ref, OtherRef),
   {s2cellunion, NewRef}.

intersection({s2cellunion, Ref}, {s2cellunion, OtherRef}) when is_reference(Ref) and is_reference(OtherRef) ->
   NewRef = s2geo_nif:s2cellunion_intersection(Ref, OtherRef),
   {s2cellunion, NewRef};
intersection({s2cellunion, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) ->
   NewRef = s2geo_nif:s2cellunion_intersection_with_s2cellid(Ref, S2CellId),
   {s2cellunion, NewRef}.

difference({s2cellunion, Ref}, {s2cellunion, OtherRef}) when is_reference(Ref) and is_reference(OtherRef) ->
   NewRef = s2geo_nif:s2cellunion_difference(Ref, OtherRef),
   {s2cellunion, NewRef}.

expand({s2cellunion, Ref}, ExpandLevel) when is_reference(Ref) and is_integer(ExpandLevel) ->
   NewRef = s2geo_nif:s2cellunion_expand_level(Ref, ExpandLevel),
   {s2cellunion, NewRef}.

expand({s2cellunion, Ref}, {s1angle, MinRadius}, MaxLevelDiff) when is_reference(Ref) and is_integer(MaxLevelDiff) ->
   NewRef = s2geo_nif:s2cellunion_expand_radius(Ref, MinRadius, MaxLevelDiff),
   {s2cellunion, NewRef}.

leaf_cells_covered({s2cellunion, Ref}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_leaf_cells_covered(Ref).

average_based_area({s2cellunion, Ref}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_average_based_area(Ref).

approx_area({s2cellunion, Ref}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_approx_area(Ref).

exact_area({s2cellunion, Ref}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_exact_area(Ref).

get_cap_bound({s2cellunion, Ref}) when is_reference(Ref) ->
   S2Cap = s2geo_nif:s2cellunion_get_cap_bound(Ref),
   {s2cap, S2Cap}.

get_rect_bound({s2cellunion, Ref}) when is_reference(Ref) ->
   S2LatLngRectRef = s2geo_nif:s2cellunion_get_rect_bound(Ref),
   {s2latlngrect, S2LatLngRectRef}.

equal({s2cellunion, Ref}, {s2cellunion, OtherRef}) when is_reference(Ref) and is_reference(OtherRef) ->
   s2geo_nif:s2cellunion_equal(Ref, OtherRef).

not_equal({s2cellunion, Ref}, {s2cellunion, OtherRef}) when is_reference(Ref) and is_reference(OtherRef) ->
   s2geo_nif:s2cellunion_not_equal(Ref, OtherRef).

encode({s2cellunion, Ref}) when is_reference(Ref) ->
   s2geo_nif:s2cellunion_encode(Ref).

decode(String) when is_binary(String) ->
  Ref = s2geo_nif:s2cellunion_decode(String),
  {s2cellunion, Ref}.


% get_covering({s2cellunion, Ref}) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2cellunion_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, 0),
%    {covering, Covering}.

% get_covering({s2cellunion, Ref}, MaxCells) when is_reference(Ref), is_integer(MaxCells) ->
%    Covering = s2geo_nif:s2cellunion_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, MaxCells),
%    {covering, Covering};
% get_covering({s2cellunion, Ref}, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2cellunion_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, 0, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_covering({s2cellunion, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2cellunion_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_interior_covering({s2cellunion, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2cellunion_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_INTERIOR_COVERING, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_cellunion_covering({s2cellunion, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2cellunion_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELL_UNION_COVERING, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_cellunion_interior_covering({s2cellunion, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2cellunion_get_covering(Ref, ?S2REGIONCOVERINGTYPE_INTERIOR_CELL_UNION_COVERING, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.
