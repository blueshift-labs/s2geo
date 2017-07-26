-module(s2geo).

-export([
    to_float/1
    ]).

to_float({Latitude, Longitude}) when is_float(Latitude), is_float(Longitude) ->
    {Latitude, Longitude};
to_float({Latitude, Longitude}) when is_integer(Latitude), is_integer(Longitude) ->
    {float(Latitude), float(Longitude)};
to_float({Latitude, Longitude}) when is_integer(Latitude) ->
    {float(Latitude), Longitude};
to_float({Latitude, Longitude}) when is_integer(Longitude) ->
    {Latitude, float(Longitude)}.
