-module(s2geo).

-export([
    to_float/1
    ]).

to_float(X) when is_number(X) -> float(X);
to_float({Latitude, Longitude}) ->
    {float(Latitude), float(Longitude)}.
