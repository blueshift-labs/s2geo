-module(s2earth).

%% API exports
-export([
    to_s1angle/1,
    to_distance/1,

    to_radians/1,
    to_meters/1,
    to_km/1,
    km_to_radians/1,
    radians_to_km/1,
    meters_to_radians/1,
    radians_to_meters/1,

    square_km_to_steradians/1,
    square_meters_to_steradians/1,
    steradians_to_square_km/1,
    steradians_to_square_meters/1,

    to_longitude_radians/2,

    get_initial_bearing/2,

    get_distance_s2point/2,
    get_distance_s2latlng/2,
    get_distance_km_s2point/2,
    get_distance_km_s2latlng/2,
    get_distance_meters_s2point/2,
    get_distance_meters_s2latlng/2,

    radius_km/0,
    radius_meters/0,

    lowest_altitude_km/0,
    lowest_altitude_meters/0,

    highest_altitude_km/0,
    highest_altitude_meters/0
  ]).

to_s1angle(DistanceInMeters) when is_number(DistanceInMeters) ->
    s2geo_nif:s2earth_to_s1angle(float(DistanceInMeters)).

to_distance({s1angle, Angle}) ->
    s2geo_nif:s2earth_to_distance(Angle).

to_radians(Meters) when is_number(Meters) ->
    s2geo_nif:s2earth_to_radians(float(Meters)).

to_meters({s1angle, Angle}) ->
    s2geo_nif:s2earth_to_meters(Angle).

to_km({s1angle, Angle}) ->
    s2geo_nif:s2earth_to_km(Angle).

km_to_radians(Km) when is_number(Km) ->
    s2geo_nif:s2earth_km_to_radians(float(Km)).

radians_to_km(Radians) when is_number(Radians) ->
    s2geo_nif:s2earth_radians_to_km(float(Radians)).

meters_to_radians(Meters) when is_number(Meters) ->
    s2geo_nif:s2earth_meters_to_radians(float(Meters)).

radians_to_meters(Radians) when is_number(Radians) ->
    s2geo_nif:s2earth_radians_to_meters(float(Radians)).

square_km_to_steradians(Km2) when is_number(Km2) ->
    s2geo_nif:s2earth_square_km_to_steradians(float(Km2)).

square_meters_to_steradians(M2) when is_number(M2) ->
    s2geo_nif:s2earth_square_meters_to_steradians(float(M2)).

steradians_to_square_km(Steradians) when is_number(Steradians) ->
    s2geo_nif:s2earth_steradians_to_square_km(float(Steradians)).

steradians_to_square_meters(Steradians) when is_number(Steradians) ->
    s2geo_nif:s2earth_steradians_to_square_meters(float(Steradians)).

to_longitude_radians(DistanceInMeters, LatitudeRadians) when is_number(DistanceInMeters), is_number(LatitudeRadians)  ->
    s2geo_nif:s2earth_to_longitude_radians(float(DistanceInMeters), float(LatitudeRadians)).

get_initial_bearing({s2latlng, A}, {s2latlng, B}) ->
    s2geo_nif:s2earth_get_initial_bearing(A, B).

get_distance_s2point({s2point, A}, {s2point, B}) ->
    s2geo_nif:s2earth_get_distance_s2point(A, B).

get_distance_s2latlng({s2latlng, A}, {s2latlng, B}) ->
    s2geo_nif:s2earth_get_distance_s2latlng(A, B).

get_distance_km_s2point({s2point, A}, {s2point, B}) ->
    s2geo_nif:s2earth_get_distance_km_s2point(A, B).

get_distance_km_s2latlng({s2latlng, A}, {s2latlng, B}) ->
    s2geo_nif:s2earth_get_distance_km_s2latlng(A, B).

get_distance_meters_s2point({s2point, A}, {s2point, B}) ->
    s2geo_nif:s2earth_get_distance_meters_s2point(A, B).

get_distance_meters_s2latlng({s2latlng, A}, {s2latlng, B}) ->
    s2geo_nif:s2earth_get_distance_meters_s2latlng(A, B).


radius_km() ->
    s2geo_nif:s2earth_radius_km().

radius_meters() ->
    s2geo_nif:s2earth_radius_meters().

lowest_altitude_km() ->
    s2geo_nif:s2earth_lowest_altitude_km().

lowest_altitude_meters() ->
    s2geo_nif:s2earth_lowest_altitude_meters().

highest_altitude_km() ->
    s2geo_nif:s2earth_highest_altitude_km().

highest_altitude_meters() ->
    s2geo_nif:s2earth_highest_altitude_meters().
