-module(test_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [
    {group, s2cellid}
].

groups() -> [
    {s2cellid, [sequence], [
        create_s2cell,
        s2cell_is_valid
    ]}
].

suite() ->
    [{timetrap, {seconds, 40}}
     %{require, cluster_options, erlcass_cluster_options}
     ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    _Config.

create_s2cell(_Config) ->
    9263400817301046493 = s2geo_nif:s2cellid_new_from_lat_long_degrees(37.2271363, -121.9877517).

s2cell_is_valid(_Config) ->
    true = s2geo_nif:s2cellid_is_valid(1),
    false = s2geo_nif:s2cellid_is_valid(2),
    true = s2geo_nif:s2cellid_is_valid(3),
    true = s2geo_nif:s2cellid_is_valid(4),
    true = s2geo_nif:s2cellid_is_valid(5),
    false = s2geo_nif:s2cellid_is_valid(6),
    true = s2geo_nif:s2cellid_is_valid(7),
    false = s2geo_nif:s2cellid_is_valid(8).
