-module(s2latlngrect).

-include("s2geo_internals.hrl").
-include("s2latlngrect_internals.hrl").

%% API exports
-export([
    new/0,
    new/2,
    new_from_center_size/2,
    empty/0,
    full/0,
    from_point/1,
    from_point_pair/2,
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

    expanded/2,
    union/2,
    intersection/2,
    convolve_with_cap/2,
    add_point/2,

    get_distance/2,
    get_directed_hausdorff_distance/2,
    get_hausdorff_distance/2,

    equal/2,
    not_equal/2,
    approx_equals/2,

    get_cap_bound/1,
    get_rect_bound/1,

    get_covering/1,
    get_covering/2,
    get_covering/3,
    get_interior_covering/3,
    get_cellunion_covering/3,
    get_cellunion_interior_covering/3
    ]).

%-on_load(init/0).

%%====================================================================
%% API functions
%%====================================================================

new({LoLatitude, LoLongitude},
                        {HiLatitude, HiLongitude}) ->
  LatLngRect = s2geo_nif:s2latlngrect_constructor(?S2LATLNGRECT_FROM_LAT_LNG_DEGREE,
                                     {LoLatitude, LoLongitude},
                                     {HiLatitude, HiLongitude}),
  {s2latlngrect, LatLngRect}.

new() ->
  LatLngRect = s2geo_nif:s2latlngrect_constructor(?S2LATLNGRECT_EMPTY),
  {s2latlngrect, LatLngRect}.

new_from_center_size({CenterLatitude, CenterLongitude},
                     {SizeLatitude, SizeLongitude}) ->
  LatLngRect = s2geo_nif:s2latlngrect_constructor(?S2LATLNGRECT_FROM_CENTER_SIZE,
                                                  {CenterLatitude, CenterLongitude},
                                                  {SizeLatitude, SizeLongitude}),
  {s2latlngrect, LatLngRect}.

empty() -> new().

full() ->
  LatLngRect = s2geo_nif:s2latlngrect_constructor(?S2LATLNGRECT_FULL),
  {s2latlngrect, LatLngRect}.

from_point({Latitude, Longitude}) ->
  LatLngRect = s2geo_nif:s2latlngrect_constructor(?S2LATLNGRECT_FROM_POINT,
                                                  {Latitude, Longitude}),
  {s2latlngrect, LatLngRect}.

from_point_pair({P1Latitude, P1Longitude}, {P2Latitude, P2Longitude}) ->
  LatLngRect = s2geo_nif:s2latlngrect_constructor(?S2LATLNGRECT_FROM_POINT_PAIR,
                                                  {P1Latitude, P1Longitude},
                                                  {P2Latitude, P2Longitude}),
  {s2latlngrect, LatLngRect}.

decode(String) when is_binary(String) ->
  LatLngRect = s2geo_nif:s2latlngrect_constructor(?S2LATLNGRECT_DECODE, String),
  {s2latlngrect, LatLngRect}.

encode({s2latlngrect, Ref}) when is_reference(Ref) ->
   s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_ENCODE).

lat_lo({s2latlngrect, Ref}) when is_reference(Ref) ->
  S1Angle = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_LAT_LO),
  {s1angle, S1Angle}.

lat_hi({s2latlngrect, Ref}) when is_reference(Ref) ->
  S1Angle = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_LAT_HI),
  {s1angle, S1Angle}.

lng_lo({s2latlngrect, Ref}) when is_reference(Ref) ->
  S1Angle = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_LNG_LO),
  {s1angle, S1Angle}.

lng_hi({s2latlngrect, Ref}) when is_reference(Ref) ->
  S1Angle = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_LNG_HI),
  {s1angle, S1Angle}.


lat({s2latlngrect, Ref}) when is_reference(Ref) ->
  R1Interval = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_LAT),
  {r1interval, R1Interval}.

lng({s2latlngrect, Ref}) when is_reference(Ref) ->
  S1Interval = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_LNG),
  {s1interval, S1Interval}.

lo({s2latlngrect, Ref}) when is_reference(Ref) ->
  S2LatLng = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_LO),
  {s2latlng, S2LatLng}.

hi({s2latlngrect, Ref}) when is_reference(Ref) ->
  S2LatLng = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_HI),
  {s2latlng, S2LatLng}.

is_valid({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_IS_VALID).

is_empty({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_IS_EMPTY).

is_full({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_IS_FULL).

is_point({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_IS_POINT).

is_inverted({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_IS_INVERTED).

get_vertex({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_GET_VERTEX).

get_center({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_GET_CENTER).

get_size({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_GET_SIZE).

area({s2latlngrect, Ref}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_AREA).

contains({s2latlngrect, Ref}, {s2point, X, Y, Z}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_CONTAINS_S2POINT,
                                {X, Y, Z});
contains({s2latlngrect, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_CONTAINS_S2CELL_WITH_ID,
                                S2CellId);
contains({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_CONTAINS_S2CELL_WITH_ID,
                                Other);
contains({s2latlngrect, Ref}, {Latitude, Longitude}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_CONTAINS_S2LATLNG,
                                 {Latitude, Longitude}).

interior_contains({s2latlngrect, Ref}, {s2point, X, Y, Z}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_INTERIOR_CONTAINS_S2POINT,
                                {X, Y, Z});
interior_contains({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_INTERIOR_CONTAINS_S2LATLNGRECT,
                                Other);
interior_contains({s2latlngrect, Ref}, {Latitude, Longitude}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_INTERIOR_CONTAINS_S2LATLNG,
                                 {Latitude, Longitude}).

intersects({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_INTERSECTS_S2LATLNGRECT,
                                Other);
intersects({s2latlngrect, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_INTERSECTS_S2CELL_WITH_ID,
                                S2CellId).

interior_intersects({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_INTERIOR_INTERSECTS_S2LATLNGRECT,
                                Other).

may_intersect({s2latlngrect, Ref}, {s2cellid, S2CellId}) when is_reference(Ref) ->
  s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_MAY_INTERSECT_S2CELL_WITH_ID,
                                S2CellId).

expanded({s2latlngrect, Ref}, {Latitude, Longitude}) when is_reference(Ref) ->
  LatLngRect = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_EXPANDED,
                                 {Latitude, Longitude}),
   {s2latlngrect, LatLngRect}.

union({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
  LatLngRect = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_UNION,
                                Other),
   {s2latlngrect, LatLngRect}.

intersection({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
   LatLngRect = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_INTERSECTION,
                                Other),
   {s2latlngrect, LatLngRect}.

convolve_with_cap({s2latlngrect, Ref}, {s1angle, S1Angle}) when is_reference(Ref) ->
   LatLngRect = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_CONVOLVE_WITH_CAP,
                                S1Angle),
   {s2latlngrect, LatLngRect}.


add_point({s2latlngrect, Ref}, {s2point, X, Y, Z}) when is_reference(Ref) ->
   LatLngRect = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_ADD_POINT,
                                {X, Y, Z}),
   {s2latlngrect, LatLngRect};
add_point({s2latlngrect, Ref}, {Latitude, Longitude}) when is_reference(Ref) ->
   LatLngRect = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_ADD_POINT_S2LATLNG,
                                {Latitude, Longitude}),
   {s2latlngrect, LatLngRect}.


get_distance({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
   S1Angle = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_GET_DISTANCE,
                                Other),
   {s1angle, S1Angle};
get_distance({s2latlngrect, Ref}, {Latitude, Longitude}) when is_reference(Ref) ->
   S1Angle = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_GET_DISTANCE_S2LATLNG,
                                 {Latitude, Longitude}),
   {s1angle, S1Angle}.

get_directed_hausdorff_distance({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
   S1Angle = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_GET_DIRECTED_HAUSDORFF_DISTANCE,
                                Other),
   {s1angle, S1Angle}.

get_hausdorff_distance({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
   S1Angle = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_GET_HAUSDORFF_DISTANCE,
                                Other),
   {s1angle, S1Angle}.

equal({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
   s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_EQUAL,
                                Other).

not_equal({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
   s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_NOT_EQUAL,
                                Other).
approx_equals({s2latlngrect, Ref}, {s2latlngrect, Other}) when is_reference(Ref) ->
   s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_APPROX_EQUALS,
                                Other).

get_cap_bound({s2latlngrect, Ref}) when is_reference(Ref) ->
    not_implemented.

get_rect_bound({s2latlngrect, Ref}) when is_reference(Ref) ->
   LatLngRect = s2geo_nif:s2latlngrect_methods(Ref, ?S2LATLNGRECT_METHOD_GET_RECT_BOUND),
   {s2latlngrect, LatLngRect}.


get_covering({s2latlngrect, Ref}) when is_reference(Ref) ->
   Covering = s2geo_nif:s2latlngrect_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, 0),
   {covering, Covering}.

get_covering({s2latlngrect, Ref}, MaxCells) when is_reference(Ref), is_integer(MaxCells) ->
   Covering = s2geo_nif:s2latlngrect_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, MaxCells),
   {covering, Covering};
get_covering({s2latlngrect, Ref}, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2latlngrect_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, 0, {MinLevel, MaxLevel}),
   {covering, Covering}.

get_covering({s2latlngrect, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2latlngrect_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_COVERING, MaxCells, {MinLevel, MaxLevel}),
   {covering, Covering}.

get_interior_covering({s2latlngrect, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2latlngrect_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELLID_INTERIOR_COVERING, MaxCells, {MinLevel, MaxLevel}),
   {covering, Covering}.

get_cellunion_covering({s2latlngrect, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2latlngrect_get_covering(Ref, ?S2REGIONCOVERINGTYPE_CELL_UNION_COVERING, MaxCells, {MinLevel, MaxLevel}),
   {covering, Covering}.

get_cellunion_interior_covering({s2latlngrect, Ref}, MaxCells, {MinLevel, MaxLevel} ) when is_reference(Ref) ->
   Covering = s2geo_nif:s2latlngrect_get_covering(Ref, ?S2REGIONCOVERINGTYPE_INTERIOR_CELL_UNION_COVERING, MaxCells, {MinLevel, MaxLevel}),
   {covering, Covering}.
