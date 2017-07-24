-module(s2geo_nif).

%% API exports
-export([
    s2cellid_get_size_ij/1,
    s2cellid_get_size_st/1,
    s2cellid_lsb_for_level/1,

    s2cellid_constructor/1,
    s2cellid_constructor/2,
    s2cellid_constructor/3,
    s2cellid_constructor/4,

    s2cellid_zero_args_fn/2,

    s2cellid_one_arg_fn/3,

    s2latlngrect_constructor/1,
    s2latlngrect_constructor/2,
    s2latlngrect_constructor/3,
    s2latlngrect_methods/2,
    s2latlngrect_methods/3,
    s2latlngrect_get_covering/3,
    s2latlngrect_get_covering/4,

    s2cellunion_constructor/2,
    s2cellunion_constructor/3,
    s2cellunion_methods/2,
    s2cellunion_methods/3,
    s2cellunion_methods/4,
    s2cellunion_get_covering/3,
    s2cellunion_get_covering/4

    ]).

%%====================================================================
%% API functions
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================

-on_load(init/0).

-define(APPNAME, s2geo).
-define(LIBNAME, s2geo).


%%====================================================================
%% S2CellId functions
%%====================================================================

s2cellid_get_size_ij(_) ->
    not_loaded(?LINE).

s2cellid_get_size_st(_) ->
    not_loaded(?LINE).

s2cellid_lsb_for_level(_) ->
    not_loaded(?LINE).

s2cellid_constructor(_) ->
    not_loaded(?LINE).

s2cellid_constructor(_, _) ->
    not_loaded(?LINE).

s2cellid_constructor(_, _, _) ->
    not_loaded(?LINE).

s2cellid_constructor(_, _, _, _) ->
    not_loaded(?LINE).

s2cellid_zero_args_fn(_, _) ->
    not_loaded(?LINE).

s2cellid_one_arg_fn(_, _, _) ->
    not_loaded(?LINE).

%%====================================================================
%% S2LatLngRect functions
%%====================================================================

s2latlngrect_constructor(_) ->
    not_loaded(?LINE).

s2latlngrect_constructor(_, _) ->
    not_loaded(?LINE).

s2latlngrect_constructor(_, _, _) ->
    not_loaded(?LINE).

s2latlngrect_methods(_, _) ->
    not_loaded(?LINE).

s2latlngrect_methods(_, _, _) ->
    not_loaded(?LINE).

s2latlngrect_get_covering(_, _, _, _) ->
    not_loaded(?LINE).

s2latlngrect_get_covering(_, _, _) ->
    not_loaded(?LINE).


s2cellunion_constructor(_, _) ->
    not_loaded(?LINE).

s2cellunion_constructor(_, _, _) ->
    not_loaded(?LINE).

s2cellunion_methods(_, _) ->
    not_loaded(?LINE).

s2cellunion_methods(_, _, _) ->
    not_loaded(?LINE).

s2cellunion_methods(_, _, _, _) ->
    not_loaded(?LINE).

s2cellunion_get_covering(_, _, _) ->
    not_loaded(?LINE).

s2cellunion_get_covering(_, _, _, _) ->
    not_loaded(?LINE).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:display(SoName),
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
