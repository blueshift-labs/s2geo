-module(test_covering).
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

covering_test() ->
  Ref = s2latlngrect:from_point_pair({33, -122}, {33.1, -122.1}),
  {covering, Covering} = s2latlngrect:get_covering(Ref, 8),
  Sorted = lists:sort(Covering),
  Expected = [9291041754864156672,
              9291043953887412224,
              9291044503643226112,
              9291045878032760832,
              9291047252422295552,
              9291047802178109440,
              9291051650468806656,
              9291052200224620544],
  ?assert(Sorted =:= Expected).
