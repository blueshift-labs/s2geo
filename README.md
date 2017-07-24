s2geo
=====

Erlang port of google s2-geometry-library
https://github.com/micolous/s2-geometry-library.git

Build
-----

$ rebar3 compile

$ rebar3 shell
s2cellid:new_from_lat_lng_degrees(37.2271363, -121.9877517).


{s2latlngrect, LatLng}=s2latlngrect:new_from_lat_lng_degree({33.0, -122.0},{33.1, -122.1}).
s2latlngrect:get_covering({s2latlngrect, LatLng}, 6, {8, 12})