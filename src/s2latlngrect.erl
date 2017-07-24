-module(s2latlngrect).

-include("s2latlngrect_internals.hrl").

%% API exports
-export([
    new_from_lat_lng_degree/2,
    new/0,
    new_from_center_size/2
    ]).

%-on_load(init/0).

%%====================================================================
%% API functions
%%====================================================================

new_from_lat_lng_degree({LoLatitude, LoLongitude},
                        {HiLatitude, HiLongitude}) ->
  LatLngRect = s2geo_nif:s2latlngrect_constructor(?S2LATLNGRECT_CONSTRUCTOR_FROM_LAT_LNG_DEGREE,
                                     {LoLatitude, LoLongitude},
                                     {HiLatitude, HiLongitude}),
  {s2latlngrect, LatLngRect}.

new() ->
  LatLngRect = s2geo_nif:s2latlngrect_constructor(?S2LATLNGRECT_CONSTRUCTOR_EMPTY),
  {s2latlngrect, LatLngRect}.

new_from_center_size({CenterLatitude, CenterLongitude},
                     {SizeLatitude, SizeLongitude}) ->
  LatLngRect = s2geo_nif:s2latlngrect_constructor(?S2LATLNGRECT_CONSTRUCTOR_FROM_CENTER_SIZE,
                                                  {CenterLatitude, CenterLongitude},
                                                  {SizeLatitude, SizeLongitude}),
  {s2latlngrect, LatLngRect}.

