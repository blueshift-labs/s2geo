-module(r1interval).

%% API exports
-export([
  new/0,
  new/2,
  new/1,
  empty/0,
  new_from_point_pair/2,
  lo/1,
  hi/1,
  bounds/1,
  is_empty/1,
  get_center/1,
  get_length/1,
  contains/2,
  interior_contains/2,
  intersects/2,
  interior_intersects/2,
  get_directed_hausdorff_distance/2,
  add_point/2,
  expanded/2,
  union/2,
  intersection/2,
  equal/2,
  approx_equal/3
  ]).

new() ->
  {r1interval, {1.0, 0.0}}.

new(Lo, Hi) when is_number(Lo) and is_number(Hi) ->
  {r1interval, {float(Lo), float(Hi)}}.

new(Point) when is_number(Point) -> new(Point, Point).

empty() -> new().

new_from_point_pair(P1, P2) when is_number(P1) and is_number(P2) ->
  Dp1 = float(P1),
  Dp2 = float(P2),
  case Dp1 =< Dp2 of
    true -> new(Dp1, Dp2);
    false -> new(Dp2, Dp1)
  end.

lo({r1interval, {Lo, _Hi}}) -> Lo.
hi({r1interval, {_Lo, Hi}}) -> Hi.

bounds({r1interval, {Lo, Hi}}) -> {Lo, Hi}.

is_empty({r1interval, {Lo, Hi}}) -> Lo > Hi.

get_center({r1interval, {Lo, Hi}}) -> 0.5 * (Lo + Hi).

get_length({r1interval, {Lo, Hi}}) -> Hi - Lo.

contains({r1interval, {Lo, Hi}}, P) when is_number(P) ->
    Dp = float(P),
    (Dp >= Lo) and (Dp =< Hi);
contains({r1interval, {_X_lo, _X_hi}}, {r1interval, {Y_lo, Y_hi}}) when Y_lo > Y_hi -> true;
contains({r1interval, {X_lo, X_hi}}, {r1interval, {Y_lo, Y_hi}}) ->
  (Y_lo >= X_lo) and (Y_hi =< X_hi).


interior_contains({r1interval, {Lo, Hi}}, P) when is_number(P) ->
    Dp = float(P),
    (Dp > Lo) and (Dp < Hi);
interior_contains({r1interval, {_X_lo, _X_hi}}, {r1interval, {Y_lo, Y_hi}}) when Y_lo > Y_hi -> true;
interior_contains({r1interval, {X_lo, X_hi}}, {r1interval, {Y_lo, Y_hi}}) ->
  (Y_lo > X_lo) and (Y_hi < X_hi).

intersects({r1interval, {X_lo, X_hi}}, {r1interval, {Y_lo, Y_hi}}) when X_lo =< Y_lo ->
  (Y_lo =< X_hi) and (Y_lo =< Y_hi);
intersects({r1interval, {X_lo, X_hi}}, {r1interval, {_Y_lo, Y_hi}}) ->
  (X_lo =< Y_hi) and (X_lo =< X_hi).

interior_intersects({r1interval, {X_lo, X_hi}}, {r1interval, {Y_lo, Y_hi}}) ->
  (Y_lo < X_hi) and (X_lo < Y_hi) and (X_lo < X_hi) and (Y_lo =< Y_hi ).

get_directed_hausdorff_distance({r1interval, {X_lo, X_hi}},
                                {r1interval, {_Y_lo, _Y_hi}}) when X_lo > X_hi -> 0.0;
get_directed_hausdorff_distance({r1interval, {_X_lo, _X_hi}},
                                {r1interval, {Y_lo, Y_hi}}) when Y_lo > Y_hi -> infinity;
get_directed_hausdorff_distance({r1interval, {X_lo, X_hi}},
                                {r1interval, {Y_lo, Y_hi}}) -> lists:max([0.0, X_hi - Y_hi, Y_lo - X_lo]).

add_point({r1interval, {Lo, Hi}}, P) when is_number(P) ->
  Dp = float(P),
  if
    Lo > Hi -> new(P, P);
    Dp < Lo -> {r1interval, {Dp, Hi}};
    Dp > Lo -> {r1interval, {Lo, Dp}};
    true -> {r1interval, {Lo, Hi}}
  end.

expanded({r1interval, {Lo, Hi}}, Radius) when is_number(Radius) ->
  if
    Radius < 0 -> error;
    Lo > Hi -> {r1interval, {Lo, Hi}};
    true -> {r1interval, {Lo - float(Radius), Hi + float(Radius)}}
  end.

union({r1interval, {X_lo, X_hi}} = X, {r1interval, {Y_lo, Y_hi}} = Y) ->
  if
    X_lo > X_hi -> Y;
    Y_lo > Y_hi -> X;
    true -> {r1interval, {lists:min([X_lo, Y_lo]), lists:max([X_hi, Y_hi]) }}
  end.

intersection({r1interval, {X_lo, X_hi}}, {r1interval, {Y_lo, Y_hi}}) ->
  {r1interval, {lists:max([X_lo, Y_lo]), lists:min([X_hi, Y_hi]) }}.

equal({r1interval, {_X_lo, _X_hi}} = X, {r1interval, {_Y_lo, _Y_hi}} = Y) -> X == Y.

approx_equal({r1interval, {X_lo, X_hi}} = X, {r1interval, {Y_lo, Y_hi}} = Y, MaxError) when is_float(MaxError) ->
  if
    X_lo > X_hi -> get_length(Y) =< MaxError;
    Y_lo > Y_hi -> get_length(X) =< MaxError;
    true -> (abs(Y_lo - X_lo) + abs(Y_hi - X_hi)) =< MaxError
  end.
