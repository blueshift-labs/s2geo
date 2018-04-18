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
  intersects/2,
  interior_intersects/2,
  get_directed_hausdorff_distance/2,

  add_point/2,
  expanded/2,
  union/2,
  intersection/2,

  equals/2,
  approx_equals/3
  ]).

new() ->
  {s1interval, s2geo_nif:s1interval_default_constructor()}.

new_from_hi_lo(Lo, Hi) when is_number(Lo) and is_number(Hi) ->
  {s1interval, s2geo_nif:s1interval_from_hi_lo(Lo, Hi)}.

new_from_point(Point) when is_number(Point) ->
  {s1interval, s2geo_nif:s1interval_from_point(Point)}.

new_from_point_pair(Point1, Point2) when is_number(Point1) and is_number(Point2) ->
  {s1interval, s2geo_nif:s1interval_from_point_pair(Point1, Point2)}.

empty() ->
  {s1interval, s2geo_nif:s1interval_empty()}.

full() ->
  {s1interval, s2geo_nif:s1interval_full()}.

hi({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_hi(S1Interval).

lo({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_lo(S1Interval).

bound({s1interval, S1Interval}, Index) ->
  s2geo_nif:s1interval_bound(S1Interval, Index).

bounds({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_bounds(S1Interval).

is_valid({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_is_valid(S1Interval).

is_full({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_is_full(S1Interval).

is_empty({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_is_empty(S1Interval).

is_inverted({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_is_inverted(S1Interval).

get_center({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_get_center(S1Interval).

get_length({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_get_length(S1Interval).

complement({s1interval, S1Interval}) ->
  {s1interval, s2geo_nif:s1interval_complement(S1Interval)}.

get_complement_center({s1interval, S1Interval}) ->
  s2geo_nif:s1interval_get_complement_center(S1Interval).

contains({s1interval, S1Interval}, {s1interval, Other}) ->
  s2geo_nif:s1interval_contains(S1Interval, Other);
contains({s1interval, S1Interval}, Point) when is_number(Point) ->
  s2geo_nif:s1interval_contains_double(S1Interval, Point).

interior_contains({s1interval, S1Interval}, {s1interval, Other}) ->
  s2geo_nif:s1interval_interior_contains(S1Interval, Other);
interior_contains({s1interval, S1Interval}, Point) when is_number(Point) ->
  s2geo_nif:s1interval_interior_contains_double(S1Interval, Point).

intersects({s1interval, S1Interval}, {s1interval, Other}) ->
  s2geo_nif:s1interval_intersects(S1Interval, Other).

interior_intersects({s1interval, S1Interval}, {s1interval, Other}) ->
  s2geo_nif:s1interval_interior_intersects(S1Interval, Other).

get_directed_hausdorff_distance({s1interval, S1Interval}, {s1interval, S1IntervalOther}) ->
  s2geo_nif:s1interval_get_directed_hausdorff_distance(S1Interval, S1IntervalOther).

add_point({s1interval, S1Interval}, Point) ->
  {s1interval, s2geo_nif:s1interval_add_point(S1Interval, Point)}.

expanded({s1interval, S1Interval}, Radius) ->
  {s1interval, s2geo_nif:s1interval_expanded(S1Interval, Radius)}.

union({s1interval, S1Interval}, {s1interval, Other}) ->
  {s1interval, s2geo_nif:s1interval_union(S1Interval, Other)}.

intersection({s1interval, S1Interval}, {s1interval, Other}) ->
  {s1interval, s2geo_nif:s1interval_intersection(S1Interval, Other)}.

equals({s1interval, S1Interval}, {s1interval, Other}) ->
  s2geo_nif:s1interval_equals(S1Interval, Other).

approx_equals({s1interval, S1Interval}, {s1interval, Other}, MaxError) when is_number(MaxError) ->
  s2geo_nif:s1interval_approx_equals(S1Interval, Other, MaxError).
