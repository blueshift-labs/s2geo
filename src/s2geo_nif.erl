-module(s2geo_nif).

%% API exports
-export([
    % s1interval_constructor/1,
    % s1interval_constructor/2,
    % s1interval_constructor/3,
    % s1interval_methods/2,
    % s1interval_methods/3,
    % s1interval_methods/4,
    s1interval_default_constructor/0,
    s1interval_empty/0,
    s1interval_full/0,
    s1interval_from_hi_lo/2,
    s1interval_from_point/1,
    s1interval_from_point_pair/2,
    s1interval_hi/1,
    s1interval_lo/1,
    s1interval_bounds/1,
    s1interval_bound/2,
    s1interval_is_valid/1,
    s1interval_is_full/1,
    s1interval_is_empty/1,
    s1interval_is_inverted/1,
    s1interval_get_center/1,
    s1interval_get_length/1,
    s1interval_complement/1,
    s1interval_get_complement_center/1,
    s1interval_interior_contains_double/2,
    s1interval_contains_double/2,
    s1interval_contains/2,
    s1interval_interior_contains/2,
    s1interval_get_directed_hausdorff_distance/2,
    s1interval_add_point/2,
    s1interval_project/2,
    s1interval_expanded/2,
    s1interval_union/2,
    s1interval_intersection/2,
    s1interval_approx_equals/3,

    s2cellid_from_s2point/1,
    s2cellid_from_lat_lng/1,
    s2cellid_none/0,
    s2cellid_sentinel/0,
    s2cellid_from_face/1,
    s2cellid_from_face_pos_level/3,
    s2cellid_to_point/1,
    s2cellid_to_point_raw/1,
    s2cellid_get_center_st/1,
    s2cellid_get_size_st/1,
    s2cellid_get_size_st_level/1,
    s2cellid_get_bound_st/1,
    s2cellid_get_center_uv/1,
    s2cellid_get_bound_uv/1,
    s2cellid_expanded_by_distance_uv/2,
    s2cellid_get_center_si_ti/1,
    s2cellid_to_lat_lng/1,
    s2cellid_is_valid/1,
    s2cellid_face/1,
    s2cellid_pos/1,
    s2cellid_level/1,
    s2cellid_get_size_ij/1,
    s2cellid_get_size_ij_level/1,
    s2cellid_is_leaf/1,
    s2cellid_is_face/1,
    s2cellid_child_position/1,
    s2cellid_range_min/1,
    s2cellid_range_max/1,
    s2cellid_contains/2,
    s2cellid_intersects/2,
    s2cellid_parent/1,
    s2cellid_parent/2,
    s2cellid_child/1,
    s2cellid_child/2,
    s2cellid_child_begin/1,
    s2cellid_child_begin/2,
    s2cellid_child_end/1,
    s2cellid_child_end/2,
    s2cellid_next/1,
    s2cellid_prev/1,
    s2cellid_advance/2,
    s2cellid_distance_from_begin/1,
    s2cellid_next_wrap/1,
    s2cellid_prev_wrap/1,
    s2cellid_advance_wrap/2,
    s2cellid_maximum_tile/2,
    s2cellid_get_common_ancestor_level/2,
    s2cellid_begin/1,
    s2cellid_end/1,
    s2cellid_to_token/1,
    s2cellid_from_token/1,
    s2cellid_to_string/1,
    s2cellid_get_edge_neighbors/1,
    s2cellid_get_vertex_neighbors/2,
    s2cellid_get_all_neighbors/2,
    s2cellid_from_face_ij/3,
    s2cellid_to_face_ij_orientation/1,
    s2cellid_lsb/1,
    s2cellid_lsb_for_level/1,
    s2cellid_ij_level_to_bound_uv/1,


    % s2latlngrect_constructor/1,
    % s2latlngrect_constructor/2,
    % s2latlngrect_constructor/3,
    % s2latlngrect_methods/2,
    % s2latlngrect_methods/3,
    % s2latlngrect_get_covering/3,
    % s2latlngrect_get_covering/4,

    % s2cellunion_constructor/2,
    % s2cellunion_constructor/3,
    % s2cellunion_methods/2,
    % s2cellunion_methods/3,
    % s2cellunion_methods/4,
    % s2cellunion_get_covering/3,
    % s2cellunion_get_covering/4,

    % s2loop_constructor/2,
    % s2loop_methods/2,
    % s2loop_methods/3,
    % s2loop_methods/4,
    % s2loop_get_covering/3,
    % s2loop_get_covering/4,

    % s2cap_constructor/1,
    % s2cap_constructor/3,

    % s2cap_methods/2,
    % s2cap_methods/3,
    % s2cap_methods/4,

    % s2cap_get_covering/3,
    % s2cap_get_covering/4,

    s2latlng_constructor/1,
    s2latlng_constructor/2,
    s2latlng_constructor/3,
    s2latlng_methods/2,
    s2latlng_methods/3,
    s2latlng_methods/4
    ]).

%%====================================================================
%% API functions
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================

-on_load(init/0).

-define(APPNAME, s2geo).
-define(LIBNAME, s2geo_nif).


%%====================================================================
%% S2CellId functions
%%====================================================================

s1interval_default_constructor() ->
    not_loaded(?LINE).

s1interval_empty() ->
    not_loaded(?LINE).

s1interval_full() ->
    not_loaded(?LINE).

s1interval_from_hi_lo(_, _) ->
    not_loaded(?LINE).

s1interval_from_point(_) ->
    not_loaded(?LINE).

s1interval_from_point_pair(_, _) ->
    not_loaded(?LINE).

s1interval_hi(_) ->
    not_loaded(?LINE).

s1interval_lo(_) ->
    not_loaded(?LINE).

s1interval_bounds(_) ->
    not_loaded(?LINE).

s1interval_bound(_, _) ->
    not_loaded(?LINE).

s1interval_is_valid(_) ->
    not_loaded(?LINE).

s1interval_is_full(_) ->
    not_loaded(?LINE).

s1interval_is_empty(_) ->
    not_loaded(?LINE).

s1interval_is_inverted(_) ->
    not_loaded(?LINE).

s1interval_get_center(_) ->
    not_loaded(?LINE).

s1interval_get_length(_) ->
    not_loaded(?LINE).

s1interval_complement(_) ->
    not_loaded(?LINE).

s1interval_get_complement_center(_) ->
    not_loaded(?LINE).

s1interval_interior_contains_double(_, _) ->
    not_loaded(?LINE).

s1interval_contains_double(_, _) ->
    not_loaded(?LINE).

s1interval_contains(_, _) ->
    not_loaded(?LINE).

s1interval_interior_contains(_, _) ->
    not_loaded(?LINE).

s1interval_get_directed_hausdorff_distance(_, _) ->
    not_loaded(?LINE).

s1interval_add_point(_, _) ->
    not_loaded(?LINE).

s1interval_project(_, _) ->
    not_loaded(?LINE).

s1interval_expanded(_, _) ->
    not_loaded(?LINE).

s1interval_union(_, _) ->
    not_loaded(?LINE).

s1interval_intersection(_, _) ->
    not_loaded(?LINE).

s1interval_approx_equals(_, _, _) ->
    not_loaded(?LINE).


%%====================================================================
%% S2Cellid functions
%%====================================================================

s2cellid_from_s2point(_) ->
    not_loaded(?LINE).

s2cellid_from_lat_lng(_) ->
    not_loaded(?LINE).

s2cellid_none() ->
    not_loaded(?LINE).

s2cellid_sentinel() ->
    not_loaded(?LINE).

s2cellid_from_face(_) ->
    not_loaded(?LINE).

s2cellid_from_face_pos_level(_, _, _) ->
    not_loaded(?LINE).

s2cellid_to_point(_) ->
    not_loaded(?LINE).

s2cellid_to_point_raw(_) ->
    not_loaded(?LINE).

s2cellid_get_center_st(_) ->
    not_loaded(?LINE).

s2cellid_get_size_st(_) ->
    not_loaded(?LINE).

s2cellid_get_size_st_level(_) ->
    not_loaded(?LINE).

s2cellid_get_bound_st(_) ->
    not_loaded(?LINE).

s2cellid_get_center_uv(_) ->
    not_loaded(?LINE).

s2cellid_get_bound_uv(_) ->
    not_loaded(?LINE).

s2cellid_expanded_by_distance_uv(_, _) ->
    not_loaded(?LINE).

s2cellid_get_center_si_ti(_) ->
    not_loaded(?LINE).

s2cellid_to_lat_lng(_) ->
    not_loaded(?LINE).

s2cellid_is_valid(_) ->
    not_loaded(?LINE).

s2cellid_face(_) ->
    not_loaded(?LINE).

s2cellid_pos(_) ->
    not_loaded(?LINE).

s2cellid_level(_) ->
    not_loaded(?LINE).

s2cellid_get_size_ij(_) ->
    not_loaded(?LINE).

s2cellid_get_size_ij_level(_) ->
    not_loaded(?LINE).

s2cellid_is_leaf(_) ->
    not_loaded(?LINE).

s2cellid_is_face(_) ->
    not_loaded(?LINE).

s2cellid_child_position(_) ->
    not_loaded(?LINE).

s2cellid_range_min(_) ->
    not_loaded(?LINE).

s2cellid_range_max(_) ->
    not_loaded(?LINE).

s2cellid_contains(_, _) ->
    not_loaded(?LINE).

s2cellid_intersects(_, _) ->
    not_loaded(?LINE).

s2cellid_parent(_) ->
    not_loaded(?LINE).

s2cellid_parent(_, _) ->
    not_loaded(?LINE).

s2cellid_child(_) ->
    not_loaded(?LINE).

s2cellid_child(_, _) ->
    not_loaded(?LINE).

s2cellid_child_begin(_) ->
    not_loaded(?LINE).

s2cellid_child_begin(_, _) ->
    not_loaded(?LINE).

s2cellid_child_end(_) ->
    not_loaded(?LINE).

s2cellid_child_end(_, _) ->
    not_loaded(?LINE).

s2cellid_next(_) ->
    not_loaded(?LINE).

s2cellid_prev(_) ->
    not_loaded(?LINE).

s2cellid_advance(_, _) ->
    not_loaded(?LINE).

s2cellid_distance_from_begin(_) ->
    not_loaded(?LINE).

s2cellid_next_wrap(_) ->
    not_loaded(?LINE).

s2cellid_prev_wrap(_) ->
    not_loaded(?LINE).

s2cellid_advance_wrap(_, _) ->
    not_loaded(?LINE).

s2cellid_maximum_tile(_, _) ->
    not_loaded(?LINE).

s2cellid_get_common_ancestor_level(_, _) ->
    not_loaded(?LINE).

s2cellid_begin(_) ->
    not_loaded(?LINE).

s2cellid_end(_) ->
    not_loaded(?LINE).

s2cellid_to_token(_) ->
    not_loaded(?LINE).

s2cellid_from_token(_) ->
    not_loaded(?LINE).

s2cellid_to_string(_) ->
    not_loaded(?LINE).

s2cellid_get_edge_neighbors(_) ->
    not_loaded(?LINE).

s2cellid_get_vertex_neighbors(_, _) ->
    not_loaded(?LINE).

s2cellid_get_all_neighbors(_, _) ->
    not_loaded(?LINE).

s2cellid_from_face_ij(_, _, _) ->
    not_loaded(?LINE).

s2cellid_to_face_ij_orientation(_) ->
    not_loaded(?LINE).

s2cellid_lsb(_) ->
    not_loaded(?LINE).

s2cellid_lsb_for_level(_) ->
    not_loaded(?LINE).

s2cellid_ij_level_to_bound_uv(_) ->
    not_loaded(?LINE).


%%====================================================================
%% S2LatLngRect functions
%%====================================================================

% s2latlngrect_constructor(_) ->
%     not_loaded(?LINE).

% s2latlngrect_constructor(_, _) ->
%     not_loaded(?LINE).

% s2latlngrect_constructor(_, _, _) ->
%     not_loaded(?LINE).

% s2latlngrect_methods(_, _) ->
%     not_loaded(?LINE).

% s2latlngrect_methods(_, _, _) ->
%     not_loaded(?LINE).

% s2latlngrect_get_covering(_, _, _, _) ->
%     not_loaded(?LINE).

% s2latlngrect_get_covering(_, _, _) ->
%     not_loaded(?LINE).

%%====================================================================
%% S2 Cell Union functions
%%====================================================================

% s2cellunion_constructor(_, _) ->
%     not_loaded(?LINE).

% s2cellunion_constructor(_, _, _) ->
%     not_loaded(?LINE).

% s2cellunion_methods(_, _) ->
%     not_loaded(?LINE).

% s2cellunion_methods(_, _, _) ->
%     not_loaded(?LINE).

% s2cellunion_methods(_, _, _, _) ->
%     not_loaded(?LINE).

% s2cellunion_get_covering(_, _, _) ->
%     not_loaded(?LINE).

% s2cellunion_get_covering(_, _, _, _) ->
%     not_loaded(?LINE).



%%====================================================================
%% S2 Loop functions
%%====================================================================

% s2loop_constructor(_, _) ->
%     not_loaded(?LINE).

% s2loop_methods(_, _) ->
%     not_loaded(?LINE).

% s2loop_methods(_, _, _) ->
%     not_loaded(?LINE).

% s2loop_methods(_, _, _, _) ->
%     not_loaded(?LINE).

% s2loop_get_covering(_, _, _) ->
%     not_loaded(?LINE).

% s2loop_get_covering(_, _, _, _) ->
%     not_loaded(?LINE).


%%====================================================================
%% S2Cap functions
%%====================================================================

% s2cap_constructor(_) ->
%     not_loaded(?LINE).

% s2cap_constructor(_, _, _) ->
%     not_loaded(?LINE).

% s2cap_methods(_, _) ->
%     not_loaded(?LINE).

% s2cap_methods(_, _, _) ->
%     not_loaded(?LINE).

% s2cap_methods(_, _, _, _) ->
%     not_loaded(?LINE).

% s2cap_get_covering(_, _, _) ->
%     not_loaded(?LINE).

% s2cap_get_covering(_, _, _, _) ->
%     not_loaded(?LINE).


%%====================================================================
%% S2LatLng functions
%%====================================================================

s2latlng_constructor(_) ->
    not_loaded(?LINE).

s2latlng_constructor(_, _) ->
    not_loaded(?LINE).

s2latlng_constructor(_, _, _) ->
    not_loaded(?LINE).

s2latlng_methods(_, _) ->
    not_loaded(?LINE).

s2latlng_methods(_, _, _) ->
    not_loaded(?LINE).

s2latlng_methods(_, _, _, _) ->
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
