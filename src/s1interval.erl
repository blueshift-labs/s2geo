-module(s1interval).

-include("s2geo.hrl").

-include("s1interval_internals.hrl").

%% API exports
-export([
  new/0,
  new_from_hi_lo/2,
  new_from_point/1,
  new_from_point_pair/2,

  empty/0,
  full/0,

  hi/1,
  lo/1,
  bound/2,
  bounds/1,
  is_valid/1,
  is_full/1,
  is_empty/1,
  is_inverted/1,
  get_center/1,
  get_length/1,

  complement/1,
  get_complement_center/1,

  contains/2,
  interior_contains/2,
  get_directed_hausdorff_distance/1,

  add_point/2,
  expanded/2,
  union/2,
  intersection/2,

  approx_equals/3
  ]).

new() ->
  {s1interval, s2geo_nif:s1interval_constructor(?S1INTERVALCONSTRUCTORS_DEFAULT_CONSTRUCTOR)}.

new_from_hi_lo(Lo, Hi) when is_number(Lo) and is_number(Hi) ->
  {s1interval, s2geo_nif:s1interval_constructor(?S1INTERVALCONSTRUCTORS_FROM_HI_LO, Lo, Hi)}.

new_from_point(Point) when is_number(Point) ->
  {s1interval, s2geo_nif:s1interval_constructor(?S1INTERVALCONSTRUCTORS_FROM_POINT, Point)}.

new_from_point_pair(Point1, Point2) when is_number(Point1) and is_number(Point2) ->
  {s1interval, s2geo_nif:s1interval_constructor(?S1INTERVALCONSTRUCTORS_FROM_POINT, Point1, Point2)}.

empty() ->
  {s1interval, s2geo_nif:s1interval_constructor(?S1INTERVALCONSTRUCTORS_EMPTY)}.

full() ->
  {s1interval, s2geo_nif:s1interval_constructor(?S1INTERVALCONSTRUCTORS_FULL)}.


hi({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_HI).

lo({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_LO).

bound({s1interval, S1Interval}, Index) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_BOUND, Index).

bounds({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_BOUND).

is_valid({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_IS_VALID).

is_full({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_IS_FULL).

is_empty({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_IS_EMPTY).

is_inverted({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_IS_INVERTED).

get_center({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_GET_CENTER).

get_length({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_GET_LENGTH).

complement({s1interval, S1Interval}) ->
  {s1interval, s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_COMPLEMENT)}.

get_complement_center({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_GET_COMPLEMENT_CENTER).

contains({s1interval, S1Interval}, {s1interval, Other}) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_CONTAINS, Other);
contains({s1interval, S1Interval}, Point) when is_number(Point) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_CONTAINS_DOUBLE, Point).

interior_contains({s1interval, S1Interval}, {s1interval, Other}) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_INTERIOR_CONTAINS, Other);
interior_contains({s1interval, S1Interval}, Point) when is_number(Point) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_INTERIOR_CONTAINS_DOUBLE, Point).

get_directed_hausdorff_distance({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_GET_DIRECTED_HAUSDORFF_DISTANCE).

add_point({s1interval, S1Interval}, Point) ->
  {s1interval, s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_ADD_POINT, Point)}.

expanded({s1interval, S1Interval}, Radius) ->
  {s1interval, s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_EXPANDED, Radius)}.

union({s1interval, S1Interval}, {s1interval, Other}) ->
  {s1interval, s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_UNION, Other)}.

intersection({s1interval, S1Interval}, {s1interval, Other}) ->
  {s1interval, s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_INTERSECTION, Other)}.

approx_equals({s1interval, S1Interval}, {s1interval, Other}, MaxError) when is_number(MaxError) ->
  s2geo_nif:s1interval_methods(S1Interval, ?S1INTERVALMETHODS_APPROX_EQUALS, Other, MaxError).
