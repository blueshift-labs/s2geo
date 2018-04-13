-module(s2latlngrect).

%% API exports
-export([
    new/0,
    new/1,
    new/2,
    new_from_center_size/2,
    new_from_point_pair/2,
    empty/0,
    full/0,
    decode/1,
    encode/1,

    lat_lo/1,
    lat_hi/1,
    lng_lo/1,
    lng_hi/1,
    lat/1,
    lng/1,
    lo/1,
    hi/1,

    is_valid/1,
    is_empty/1,
    is_full/1,
    is_point/1,
    is_inverted/1,

    get_vertex/1,
    get_center/1,
    get_size/1,

    area/1,
    contains/2,
    interior_contains/2,
    intersects/2,
    interior_intersects/2,
    may_intersect/2,
    boundary_intersects/3,

    expanded/2,
    union/2,
    intersection/2,
    add_point/2,

    get_distance/2,
    get_directed_hausdorff_distance/2,
    get_hausdorff_distance/2,

    equal/2,
    not_equal/2,
    approx_equals/3,

    get_cap_bound/1,
    get_rect_bound/1,

    intersects_lat_edge/4,
    intersects_lng_edge/4

    % get_covering/1,
    % get_covering/2,
    % get_covering/3,
    % get_interior_covering/3,
    % get_cellunion_covering/3,
    % get_cellunion_interior_covering/3
    ]).

%-on_load(init/0).

%%====================================================================
%% API functions
%%====================================================================

new() ->
  LatLngRect = s2geo_nif:s2latlngrect_empty(),
  {s2latlngrect, LatLngRect}.

new({s2latlng, Point}) ->
  LatLngRect = s2geo_nif:s2latlngrect_from_point(Point),
  {s2latlngrect, LatLngRect}.

new({s2latlng, Lo}, {s2latlng, Hi}) ->
  LatLngRect = s2geo_nif:s2latlngrect_from_lat_lng(Lo, Hi),
  {s2latlngrect, LatLngRect};
new({r1interval, Lat}, {s1interval, Lng}) ->
  LatLngRect = s2geo_nif:s2latlngrect_from_r1inteval_s1interval(Lat, Lng),
  {s2latlngrect, LatLngRect}.

new_from_center_size({s2latlng, Center}, {s2latlng, Size}) ->
  LatLngRect = s2geo_nif:s2latlngrect_from_center_size(Center, Size),
  {s2latlngrect, LatLngRect}.

new_from_point_pair({s2latlng, Point1}, {s2latlng, Point2}) ->
  LatLngRect = s2geo_nif:s2latlngrect_from_point_pair(Point1, Point2),
  {s2latlngrect, LatLngRect}.

empty() -> new().

full() ->
  LatLngRect = s2geo_nif:s2latlngrect_full(),
  {s2latlngrect, LatLngRect}.

decode(String) when is_binary(String) ->
  LatLngRect = s2geo_nif:s2latlngrect_decode(String),
  {s2latlngrect, LatLngRect}.

encode({s2latlngrect, Ref}) when is_reference(Ref) ->
   s2geo_nif:s2latlngrect_ (Ref).

lat_lo({s2latlngrect, Ref}) when is_reference(Ref) ->
  S1Angle = s2geo_nif:s2latlngrect_lat_lo(Ref),
  {s1angle, S1Angle}.

lat_hi({s2latlngrect, Ref}) when is_reference(Ref) ->
  S1Angle = s2geo_nif:s2latlngrect_lat_hi(Ref),
  {s1angle, S1Angle}.

lng_lo({s2latlngrect, Ref}) when is_reference(Ref) ->
  S1Angle = s2geo_nif:s2latlngrect_lng_lo (Ref),
  {s1angle, S1Angle}.

lng_hi({s2latlngrect, Ref}) when is_reference(Ref) ->
  S1Angle = s2geo_nif:s2latlngrect_lng_hi(Ref),
  {s1angle, S1Angle}.


lat({s2latlngrect, Ref}) when is_reference(Ref) ->
  R1Interval = s2geo_nif:s2latlngrect_lat(Ref),
  {r1interval, R1Interval}.

lng({s2latlngrect, Ref}) when is_reference(Ref) ->
  S1Interval = s2geo_nif:s2latlngrect_lng(Ref),
  {s1interval, S1Interval}.

lo({s2latlngrect, Ref}) when is_reference(Ref) ->
  S2LatLng = s2geo_nif:s2latlngrect_lo(Ref),
  {s2latlng, S2LatLng}.

hi({s2latlngrect, Ref}) when is_reference(Ref) ->
  S2LatLng = s2geo_nif:s2latlngrect_hi(Ref),
  {s2latlng, S2LatLng}.

is_valid({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_is_valid(Ref).

is_empty({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_is_empty(Ref).

is_full({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_is_full(Ref).

is_point({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_is_point(Ref).

is_inverted({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_is_inverted(Ref).

get_vertex({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_get_vertex(Ref).

get_center({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_get_center(Ref).

get_size({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_get_size(Ref).

area({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_area(Ref).

contains({s2latlngrect, Ref}, {s2point, X, Y, Z}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_contains_s2point(Ref, {X, Y, Z});
contains({s2latlngrect, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_contains_s2cell_with_id(Ref, S2CellId);
contains({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_contains_s2latlngrect(Ref, Other);
contains({s2latlngrect, Ref}, {s2latlng, LatLng}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_contains_s2latlng(Ref, LatLng).

interior_contains({s2latlngrect, Ref}, {s2point, X, Y, Z}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_interior_contains_s2point(Ref, {X, Y, Z});
interior_contains({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_interior_contains_s2latlngrect(Ref, Other);
interior_contains({s2latlngrect, Ref}, {s2latlng, LatLng}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_interior_contains_s2latlng(Ref, LatLng).

intersects({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_intersects_s2latlngrect(Ref, Other);
intersects({s2latlngrect, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_intersects_s2cell_with_id(Ref, S2CellId).

interior_intersects({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_interior_intersects_s2latlngrect(Ref, Other).

boundary_intersects({s2latlngrect, Ref}, {s2point, V0}, {s2point, V1}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_boundary_intersects(Ref, V0, V1).

may_intersect({s2latlngrect, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_may_intersect_s2cell_with_id(Ref, S2CellId).

expanded({s2latlngrect, Ref}, {s2latlng, LatLng}) when is_reference(Ref) ->
  LatLngRect = s2geo_nif:s2latlngrect_expanded(Ref, LatLng),
   {s2latlngrect, LatLngRect}.

union({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  LatLngRect = s2geo_nif:s2latlngrect_union(Ref, Other),
   {s2latlngrect, LatLngRect}.

intersection({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  LatLngRect = s2geo_nif:s2latlngrect_intersection(Ref, Other),
  {s2latlngrect, LatLngRect}.

add_point({s2latlngrect, Ref}, {s2point, Point}) when is_reference(Ref) ->
   LatLngRect = s2geo_nif:s2latlngrect_add_point(Ref, Point),
   {s2latlngrect, LatLngRect};
add_point({s2latlngrect, Ref}, {s2latlng, LatLng}) when is_reference(Ref) ->
  LatLngRect = s2geo_nif:s2latlngrect_add_point_s2latlng(Ref, LatLng),
  {s2latlngrect, LatLngRect}.


get_distance({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  S1Angle = s2geo_nif:s2latlngrect_get_distance(Ref, Other),
  {s1angle, S1Angle};
get_distance({s2latlngrect, Ref}, {s2latlng, LatLng}) when is_reference(Ref) ->
  S1Angle = s2geo_nif:s2latlngrect_get_distance_s2latlng(Ref, LatLng),
  {s1angle, S1Angle}.

get_directed_hausdorff_distance({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  S1Angle = s2geo_nif:s2latlngrect_get_directed_hausdorff_distance(Ref, Other),
  {s1angle, S1Angle}.

get_hausdorff_distance({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  S1Angle = s2geo_nif:s2latlngrect_get_hausdorff_distance(Ref, Other),
  {s1angle, S1Angle}.

equal({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_equal(Ref, Other).

not_equal({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_not_equal (Ref, Other).

approx_equals({s2latlngrect, Ref}, {s2latlngrect, Other}, {s1angle, MaxError}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_approx_equals_with_s1angle_error(Ref, Other, MaxError);
approx_equals({s2latlngrect, Ref}, {s2latlngrect, Other}, {s2latlng, MaxError}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_approx_equals_with_s2latlng_error(Ref, Other, MaxError).

get_cap_bound({s2latlngrect, Ref}) when is_reference(Ref) ->
  {s2cap, s2geo_nif:s2latlngrect_get_cap_bound(Ref)}.

get_rect_bound({s2latlngrect, Ref}) when is_reference(Ref) ->
  LatLngRect = s2geo_nif:s2latlngrect_get_rect_bound(Ref),
  {s2latlngrect, LatLngRect}.

intersects_lat_edge({s2point, A}, {s2point, B}, Lat, {s1interval, Lng}) when is_float(Lat) ->
  s2geo_nif:s2latlngrect_intersects_lat_edge(A, B, Lat, Lng).

intersects_lng_edge({s2point, A}, {s2point, B}, {r1interval, Lat}, Lng) when is_float(Lng) ->
  s2geo_nif:s2latlngrect_intersects_lng_edge(A, B, Lat, Lng).



% get_covering({s2latlngrect, Ref}) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2latlngrect_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, 0),
%    {covering, Covering}.

% get_covering({s2latlngrect, Ref}, MaxCells) when is_reference(Ref), is_integer(MaxCells) ->
%    Covering = s2geo_nif:s2latlngrect_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, MaxCells),
%    {covering, Covering};
% get_covering({s2latlngrect, Ref}, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2latlngrect_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, 0, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_covering({s2latlngrect, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2latlngrect_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_interior_covering({s2latlngrect, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2latlngrect_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_INTERIOR_COVERING, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_cellunion_covering({s2latlngrect, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2latlngrect_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELL_UNION_COVERING, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.

% get_cellunion_interior_covering({s2latlngrect, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
%    Covering = s2geo_nif:s2latlngrect_get_covering(Ref, ?S2REGIONCOVERINGTYPE_INTERIOR_CELL_UNION_COVERING, MaxCells, {MinLevel, MaxLevel}),
%    {covering, Covering}.

