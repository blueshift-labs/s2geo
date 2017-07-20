-module(s2geo_nif).

%% API exports
-export([s2cellid_new_from_lat_long_degrees/2]).

%%====================================================================
%% API functions
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================

-on_load(init/0).

-define(APPNAME, s2geo_nif).
-define(LIBNAME, s2geo).

s2cellid_new_from_lat_long_degrees(_, _) ->
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
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
