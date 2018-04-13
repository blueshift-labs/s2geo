-module(s2geo_nif).

%% API exports
-export([
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

    s2latlng_default_constructor/0,
    s2latlng_from_s1angle_lat_lng/2,
    s2latlng_from_point/1,
    s2latlng_invalid/0,
    s2latlng_from_radians/2,
    s2latlng_from_degrees/2,
    s2latlng_from_e5/2,
    s2latlng_from_e6/2,
    s2latlng_from_e7/2,
    s2latlng_from_unsigned_e6/2,
    s2latlng_from_unsigned_e7/2,
    s2latlng_latitude/1,
    s2latlng_longitude/1,
    s2latlng_lat/1,
    s2latlng_lng/1,
    s2latlng_coords/1,
    s2latlng_is_valid/1,
    s2latlng_normalized/1,
    s2latlng_to_point/1,
    s2latlng_get_distance/2,
    s2latlng_add/2,
    s2latlng_subtract/2,
    s2latlng_multiply/2,
    s2latlng_eq/2,
    s2latlng_neq/2,
    s2latlng_lt/2,
    s2latlng_gt/2,
    s2latlng_leq/2,
    s2latlng_geq/2,
    s2latlng_approx_equals/3,
    s2latlng_to_string_in_degrees/1,

    s2cellunion_new_from_cellids/1,
    s2cellunion_new_from_normalized_cellids/1,
    s2cellunion_new_from_min_max/2,
    s2cellunion_new_from_begin_end/2,
    s2cellunion_num_cells/1,
    s2cellunion_cell_id/2,
    s2cellunion_cell_ids/1,
    s2cellunion_is_valid/1,
    s2cellunion_is_normalized/1,
    s2cellunion_normalize/1,
    s2cellunion_denormalize/3,
    s2cellunion_pack/2,
    s2cellunion_contains_s2cellid/2,
    s2cellunion_intersects_s2cellid/2,
    s2cellunion_contains_s2cellunion/2,
    s2cellunion_intersects_s2cellunion/2,
    s2cellunion_union/2,
    s2cellunion_intersection/2,
    s2cellunion_intersection_with_s2cellid/2,
    s2cellunion_difference/2,
    s2cellunion_expand_level/2,
    s2cellunion_expand_radius/3,
    s2cellunion_leaf_cells_covered/1,
    s2cellunion_average_based_area/1,
    s2cellunion_approx_area/1,
    s2cellunion_exact_area/1,
    s2cellunion_get_cap_bound/1,
    s2cellunion_get_rect_bound/1,
    s2cellunion_equal/2,
    s2cellunion_not_equal/2,
    s2cellunion_contains_s2cell/2,
    s2cellunion_may_intersect_s2cellid/2,
    s2cellunion_may_intersect_s2cell/2,
    s2cellunion_contains_s2point/2,
    s2cellunion_encode/1,
    s2cellunion_decode/1,

    % s2loop_constructor/2,
    % s2loop_methods/2,
    % s2loop_methods/3,
    % s2loop_methods/4,
    % s2loop_get_covering/3,
    % s2loop_get_covering/4,


    s2cap_from_s2point_s1angle/2,
    s2cap_from_s2point_s1chordangle/2,
    s2cap_from_point/1,
    s2cap_from_center_height/2,
    s2cap_from_center_area/2,
    s2cap_empty/0,
    s2cap_full/0,

    s2cap_center/1,
    s2cap_radius/1,
    s2cap_height/1,
    s2cap_get_radius/1,
    s2cap_get_area/1,
    s2cap_get_centroid/1,
    s2cap_is_valid/1,
    s2cap_is_empty/1,
    s2cap_is_full/1,
    s2cap_complement/1,

    s2cap_contains/2,
    s2cap_intersects/2,
    s2cap_interior_intersects/2,
    s2cap_interior_contains/2,

    s2cap_add_point/2,
    s2cap_add_cap/2,
    s2cap_expanded/2,
    s2cap_union/2,
    s2cap_clone/1,
    s2cap_get_cap_bound/1,
    s2cap_get_rect_bound/1,
    s2cap_get_cell_union_bound/1,

    s2cap_contains_s2cellid/2,
    s2cap_contains_s2cell/2,
    s2cap_may_intersect_s2cellid/2,
    s2cap_may_intersect_s2cell/2,
    s2cap_contains_s2point/2,

    s2cap_encode/1,
    s2cap_decode/1,
    s2cap_equal/2,
    s2cap_approx_equals/3,

    s2latlngrect_from_lat_lng/2,
    s2latlngrect_from_r1inteval_s1interval/2,
    s2latlngrect_from_center_size/2,
    s2latlngrect_from_point/1,
    s2latlngrect_from_point_pair/2,
    s2latlngrect_empty/0,
    s2latlngrect_full/0,
    s2latlngrect_full_lat/0,
    s2latlngrect_full_lng/0,

    s2latlngrect_lat_lo/1,
    s2latlngrect_lat_hi/1,
    s2latlngrect_lng_lo/1,
    s2latlngrect_lng_hi/1,
    s2latlngrect_lat/1,
    s2latlngrect_lng/1,
    s2latlngrect_lo/1,
    s2latlngrect_hi/1,
    s2latlngrect_is_valid/1,
    s2latlngrect_is_empty/1,
    s2latlngrect_is_full/1,
    s2latlngrect_is_point/1,
    s2latlngrect_is_inverted/1,
    s2latlngrect_get_vertex/1,
    s2latlngrect_get_center/1,
    s2latlngrect_get_size/1,
    s2latlngrect_area/1,
    s2latlngrect_contains_s2latlng/2,
    s2latlngrect_interior_contains_s2point/2,
    s2latlngrect_interior_contains_s2latlng/2,
    s2latlngrect_contains_s2latlngrect/2,
    s2latlngrect_interior_contains_s2latlngrect/2,
    s2latlngrect_intersects_s2latlngrect/2,
    s2latlngrect_intersects_s2cell_with_id/2,
    s2latlngrect_interior_intersects_s2latlngrect/2,
    s2latlngrect_boundary_intersects/3,
    s2latlngrect_contains_s2cell_with_id/2,
    s2latlngrect_contains_s2point/2,
    s2latlngrect_may_intersect_s2cell_with_id/2,
    s2latlngrect_expanded/2,
    s2latlngrect_union/2,
    s2latlngrect_intersection/2,

    s2latlngrect_add_point_s2latlng/2,
    s2latlngrect_add_point/2,
    s2latlngrect_get_distance/2,
    s2latlngrect_get_distance_s2latlng/2,
    s2latlngrect_get_directed_hausdorff_distance/2,
    s2latlngrect_get_hausdorff_distance/2,
    s2latlngrect_equal/2,
    s2latlngrect_not_equal/2,

    s2latlngrect_approx_equals_with_s1angle_error/2,
    s2latlngrect_approx_equals_with_s1angle_error/3,
    s2latlngrect_approx_equals_with_s2latlng_error/3,

    s2latlngrect_get_cap_bound/1,
    s2latlngrect_get_rect_bound/1,

    s2latlngrect_decode/1,
    s2latlngrect_encode/1,

    s2latlngrect_intersects_lng_edge/4,
    s2latlngrect_intersects_lat_edge/4,

    s2earth_to_s1angle/1,
    s2earth_to_distance/1,
    s2earth_to_radians/1,
    s2earth_to_meters/1,
    s2earth_to_km/1,
    s2earth_km_to_radians/1,
    s2earth_radians_to_km/1,
    s2earth_meters_to_radians/1,
    s2earth_radians_to_meters/1,
    s2earth_square_km_to_steradians/1,
    s2earth_square_meters_to_steradians/1,
    s2earth_steradians_to_square_km/1,
    s2earth_steradians_to_square_meters/1,
    s2earth_to_longitude_radians/2,
    s2earth_get_initial_bearing/2,
    s2earth_get_distance_s2point/2,
    s2earth_get_distance_s2latlng/2,
    s2earth_get_distance_km_s2point/2,
    s2earth_get_distance_km_s2latlng/2,
    s2earth_get_distance_meters_s2point/2,
    s2earth_get_distance_meters_s2latlng/2,
    s2earth_radius/0,
    s2earth_radius_km/0,
    s2earth_radius_meters/0,
    s2earth_lowest_altitude/0,
    s2earth_lowest_altitude_km/0,
    s2earth_lowest_altitude_meters/0,
    s2earth_highest_altitude/0,
    s2earth_highest_altitude_km/0,
    s2earth_highest_altitude_meters/0
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

s2latlng_default_constructor() ->
    not_loaded(?LINE).

s2latlng_from_s1angle_lat_lng(_, _)  ->
    not_loaded(?LINE).

s2latlng_from_point(_)  ->
    not_loaded(?LINE).

s2latlng_invalid() ->
    not_loaded(?LINE).

s2latlng_from_radians(_, _)  ->
    not_loaded(?LINE).

s2latlng_from_degrees(_, _)  ->
    not_loaded(?LINE).

s2latlng_from_e5(_, _) ->
    not_loaded(?LINE).

s2latlng_from_e6(_, _) ->
    not_loaded(?LINE).

s2latlng_from_e7(_, _) ->
    not_loaded(?LINE).

s2latlng_from_unsigned_e6(_, _) ->
    not_loaded(?LINE).

s2latlng_from_unsigned_e7(_, _) ->
    not_loaded(?LINE).

s2latlng_latitude(_) ->
    not_loaded(?LINE).

s2latlng_longitude(_) ->
    not_loaded(?LINE).

s2latlng_lat(_) ->
    not_loaded(?LINE).

s2latlng_lng(_) ->
    not_loaded(?LINE).

s2latlng_coords(_) ->
    not_loaded(?LINE).

s2latlng_is_valid(_) ->
    not_loaded(?LINE).

s2latlng_normalized(_) ->
    not_loaded(?LINE).

s2latlng_to_point(_) ->
    not_loaded(?LINE).

s2latlng_get_distance(_, _) ->
    not_loaded(?LINE).

s2latlng_add(_, _) ->
    not_loaded(?LINE).

s2latlng_subtract(_, _) ->
    not_loaded(?LINE).

s2latlng_multiply(_, _) ->
    not_loaded(?LINE).

s2latlng_eq(_, _) ->
    not_loaded(?LINE).

s2latlng_neq(_, _) ->
    not_loaded(?LINE).

s2latlng_lt(_, _) ->
    not_loaded(?LINE).

s2latlng_gt(_, _) ->
    not_loaded(?LINE).

s2latlng_leq(_, _) ->
    not_loaded(?LINE).

s2latlng_geq(_, _) ->
    not_loaded(?LINE).

s2latlng_approx_equals(_, _, _) ->
    not_loaded(?LINE).

s2latlng_to_string_in_degrees(_) ->
    not_loaded(?LINE).

%%====================================================================
%% S2 Cell Union functions
%%====================================================================

s2cellunion_new_from_cellids(_) ->
    not_loaded(?LINE).

s2cellunion_new_from_normalized_cellids(_) ->
    not_loaded(?LINE).

s2cellunion_new_from_min_max(_, _) ->
    not_loaded(?LINE).

s2cellunion_new_from_begin_end(_, _) ->
    not_loaded(?LINE).


s2cellunion_num_cells(_) ->
    not_loaded(?LINE).

s2cellunion_cell_id(_, _) ->
    not_loaded(?LINE).

s2cellunion_cell_ids(_) ->
    not_loaded(?LINE).

s2cellunion_is_valid(_) ->
    not_loaded(?LINE).

s2cellunion_is_normalized(_) ->
    not_loaded(?LINE).

s2cellunion_normalize(_) ->
    not_loaded(?LINE).

s2cellunion_denormalize(_, _, _) ->
    not_loaded(?LINE).

s2cellunion_pack(_, _) ->
    not_loaded(?LINE).


s2cellunion_contains_s2cellid(_, _) ->
    not_loaded(?LINE).

s2cellunion_intersects_s2cellid(_, _) ->
    not_loaded(?LINE).


s2cellunion_contains_s2cellunion(_, _) ->
    not_loaded(?LINE).

s2cellunion_intersects_s2cellunion(_, _) ->
    not_loaded(?LINE).


s2cellunion_union(_, _) ->
    not_loaded(?LINE).

s2cellunion_intersection(_, _) ->
    not_loaded(?LINE).

s2cellunion_intersection_with_s2cellid(_, _) ->
    not_loaded(?LINE).

s2cellunion_difference(_, _) ->
    not_loaded(?LINE).


s2cellunion_expand_level(_, _) ->
    not_loaded(?LINE).

s2cellunion_expand_radius(_, _, _) ->
    not_loaded(?LINE).

s2cellunion_leaf_cells_covered(_) ->
    not_loaded(?LINE).

s2cellunion_average_based_area(_) ->
    not_loaded(?LINE).

s2cellunion_approx_area(_) ->
    not_loaded(?LINE).

s2cellunion_exact_area(_) ->
    not_loaded(?LINE).


s2cellunion_get_cap_bound(_) ->
    not_loaded(?LINE).

s2cellunion_get_rect_bound(_) ->
    not_loaded(?LINE).

s2cellunion_equal(_, _) ->
    not_loaded(?LINE).

s2cellunion_not_equal(_, _) ->
    not_loaded(?LINE).


s2cellunion_contains_s2cell(_, _) ->
    not_loaded(?LINE).

s2cellunion_may_intersect_s2cellid(_, _) ->
    not_loaded(?LINE).

s2cellunion_may_intersect_s2cell(_, _) ->
    not_loaded(?LINE).

s2cellunion_contains_s2point(_, _) ->
    not_loaded(?LINE).


s2cellunion_encode(_) ->
    not_loaded(?LINE).

s2cellunion_decode(_) ->
    not_loaded(?LINE).


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

s2cap_from_s2point_s1angle(_, _) ->
    not_loaded(?LINE).

s2cap_from_s2point_s1chordangle(_, _) ->
    not_loaded(?LINE).

s2cap_from_point(_) ->
    not_loaded(?LINE).

s2cap_from_center_height(_, _) ->
    not_loaded(?LINE).

s2cap_from_center_area(_, _) ->
    not_loaded(?LINE).

s2cap_empty() ->
    not_loaded(?LINE).

s2cap_full() ->
    not_loaded(?LINE).

s2cap_center(_) ->
    not_loaded(?LINE).

s2cap_radius(_) ->
    not_loaded(?LINE).

s2cap_height(_) ->
    not_loaded(?LINE).

s2cap_get_radius(_) ->
    not_loaded(?LINE).

s2cap_get_area(_) ->
    not_loaded(?LINE).

s2cap_get_centroid(_) ->
    not_loaded(?LINE).

s2cap_is_valid(_) ->
    not_loaded(?LINE).

s2cap_is_empty(_) ->
    not_loaded(?LINE).

s2cap_is_full(_) ->
    not_loaded(?LINE).

s2cap_complement(_) ->
    not_loaded(?LINE).

s2cap_contains(_, _) ->
    not_loaded(?LINE).

s2cap_intersects(_, _) ->
    not_loaded(?LINE).

s2cap_interior_intersects(_, _) ->
    not_loaded(?LINE).

s2cap_interior_contains(_, _) ->
    not_loaded(?LINE).

s2cap_add_point(_, _) ->
    not_loaded(?LINE).

s2cap_add_cap(_, _) ->
    not_loaded(?LINE).

s2cap_expanded(_, _) ->
    not_loaded(?LINE).

s2cap_union(_, _) ->
    not_loaded(?LINE).

s2cap_clone(_) ->
    not_loaded(?LINE).

s2cap_get_cap_bound(_) ->
    not_loaded(?LINE).

s2cap_get_rect_bound(_) ->
    not_loaded(?LINE).

s2cap_get_cell_union_bound(_) ->
    not_loaded(?LINE).

s2cap_contains_s2cellid(_, _) ->
    not_loaded(?LINE).

s2cap_contains_s2cell(_, _) ->
    not_loaded(?LINE).

s2cap_may_intersect_s2cellid(_, _) ->
    not_loaded(?LINE).

s2cap_may_intersect_s2cell(_, _) ->
    not_loaded(?LINE).

s2cap_contains_s2point(_, _) ->
    not_loaded(?LINE).

s2cap_encode(_) ->
    not_loaded(?LINE).

s2cap_decode(_) ->
    not_loaded(?LINE).

s2cap_equal(_, _) ->
    not_loaded(?LINE).

s2cap_approx_equals(_, _, _) ->
    not_loaded(?LINE).

%%====================================================================
%% S2LatLngRect functions
%%====================================================================

s2latlngrect_from_lat_lng(_, _) ->
    not_loaded(?LINE).

s2latlngrect_from_r1inteval_s1interval(_, _) ->
    not_loaded(?LINE).

s2latlngrect_from_center_size(_, _) ->
    not_loaded(?LINE).

s2latlngrect_from_point(_) ->
    not_loaded(?LINE).

s2latlngrect_from_point_pair(_, _) ->
    not_loaded(?LINE).

s2latlngrect_empty() ->
    not_loaded(?LINE).

s2latlngrect_full() ->
    not_loaded(?LINE).

s2latlngrect_full_lat() ->
    not_loaded(?LINE).

s2latlngrect_full_lng() ->
    not_loaded(?LINE).


s2latlngrect_lat_lo(_) ->
    not_loaded(?LINE).

s2latlngrect_lat_hi(_) ->
    not_loaded(?LINE).

s2latlngrect_lng_lo(_) ->
    not_loaded(?LINE).

s2latlngrect_lng_hi(_) ->
    not_loaded(?LINE).

s2latlngrect_lat(_) ->
    not_loaded(?LINE).

s2latlngrect_lng(_) ->
    not_loaded(?LINE).

s2latlngrect_lo(_) ->
    not_loaded(?LINE).

s2latlngrect_hi(_) ->
    not_loaded(?LINE).

s2latlngrect_is_valid(_) ->
    not_loaded(?LINE).

s2latlngrect_is_empty(_) ->
    not_loaded(?LINE).

s2latlngrect_is_full(_) ->
    not_loaded(?LINE).

s2latlngrect_is_point(_) ->
    not_loaded(?LINE).

s2latlngrect_is_inverted(_) ->
    not_loaded(?LINE).

s2latlngrect_get_vertex(_) ->
    not_loaded(?LINE).

s2latlngrect_get_center(_) ->
    not_loaded(?LINE).

s2latlngrect_get_size(_) ->
    not_loaded(?LINE).

s2latlngrect_area(_) ->
    not_loaded(?LINE).

s2latlngrect_contains_s2latlng(_, _) ->
    not_loaded(?LINE).

s2latlngrect_interior_contains_s2point(_, _) ->
    not_loaded(?LINE).

s2latlngrect_interior_contains_s2latlng(_, _) ->
    not_loaded(?LINE).

s2latlngrect_contains_s2latlngrect(_, _) ->
    not_loaded(?LINE).

s2latlngrect_interior_contains_s2latlngrect(_, _) ->
    not_loaded(?LINE).

s2latlngrect_intersects_s2latlngrect(_, _) ->
    not_loaded(?LINE).

s2latlngrect_intersects_s2cell_with_id(_, _) ->
    not_loaded(?LINE).

s2latlngrect_interior_intersects_s2latlngrect(_, _) ->
    not_loaded(?LINE).

s2latlngrect_boundary_intersects(_, _, _) ->
    not_loaded(?LINE).


s2latlngrect_contains_s2cell_with_id(_, _) ->
    not_loaded(?LINE).

s2latlngrect_contains_s2point(_, _) ->
    not_loaded(?LINE).

s2latlngrect_may_intersect_s2cell_with_id(_, _) ->
    not_loaded(?LINE).

s2latlngrect_expanded(_, _) ->
    not_loaded(?LINE).

s2latlngrect_union(_, _) ->
    not_loaded(?LINE).

s2latlngrect_intersection(_, _) ->
    not_loaded(?LINE).


s2latlngrect_add_point_s2latlng(_, _) ->
    not_loaded(?LINE).

s2latlngrect_add_point(_, _) ->
    not_loaded(?LINE).

s2latlngrect_get_distance(_, _) ->
    not_loaded(?LINE).

s2latlngrect_get_distance_s2latlng(_, _) ->
    not_loaded(?LINE).

s2latlngrect_get_directed_hausdorff_distance(_, _) ->
    not_loaded(?LINE).

s2latlngrect_get_hausdorff_distance(_, _) ->
    not_loaded(?LINE).

s2latlngrect_equal(_, _) ->
    not_loaded(?LINE).

s2latlngrect_not_equal(_, _) ->
    not_loaded(?LINE).


s2latlngrect_approx_equals_with_s1angle_error(_, _) ->
    not_loaded(?LINE).

s2latlngrect_approx_equals_with_s1angle_error(_, _, _) ->
    not_loaded(?LINE).

s2latlngrect_approx_equals_with_s2latlng_error(_, _, _) ->
    not_loaded(?LINE).


s2latlngrect_get_cap_bound(_) ->
    not_loaded(?LINE).

s2latlngrect_get_rect_bound(_) ->
    not_loaded(?LINE).


s2latlngrect_decode(_) ->
    not_loaded(?LINE).

s2latlngrect_encode(_) ->
    not_loaded(?LINE).


s2latlngrect_intersects_lng_edge(_, _, _, _) ->
    not_loaded(?LINE).

s2latlngrect_intersects_lat_edge(_, _, _, _) ->
    not_loaded(?LINE).


%%====================================================================
%% S2Earth functions
%%====================================================================

s2earth_to_s1angle(_) ->
    not_loaded(?LINE).

s2earth_to_distance(_) ->
    not_loaded(?LINE).

s2earth_to_radians(_) ->
    not_loaded(?LINE).

s2earth_to_meters(_) ->
    not_loaded(?LINE).

s2earth_to_km(_) ->
    not_loaded(?LINE).

s2earth_km_to_radians(_) ->
    not_loaded(?LINE).

s2earth_radians_to_km(_) ->
    not_loaded(?LINE).

s2earth_meters_to_radians(_) ->
    not_loaded(?LINE).

s2earth_radians_to_meters(_) ->
    not_loaded(?LINE).

s2earth_square_km_to_steradians(_) ->
    not_loaded(?LINE).

s2earth_square_meters_to_steradians(_) ->
    not_loaded(?LINE).

s2earth_steradians_to_square_km(_) ->
    not_loaded(?LINE).

s2earth_steradians_to_square_meters(_) ->
    not_loaded(?LINE).

s2earth_to_longitude_radians(_, _) ->
    not_loaded(?LINE).

s2earth_get_initial_bearing(_, _) ->
    not_loaded(?LINE).

s2earth_get_distance_s2point(_, _) ->
    not_loaded(?LINE).

s2earth_get_distance_s2latlng(_, _) ->
    not_loaded(?LINE).

s2earth_get_distance_km_s2point(_, _) ->
    not_loaded(?LINE).

s2earth_get_distance_km_s2latlng(_, _) ->
    not_loaded(?LINE).

s2earth_get_distance_meters_s2point(_, _) ->
    not_loaded(?LINE).

s2earth_get_distance_meters_s2latlng(_, _) ->
    not_loaded(?LINE).

s2earth_radius() ->
    not_loaded(?LINE).

s2earth_radius_km() ->
    not_loaded(?LINE).

s2earth_radius_meters() ->
    not_loaded(?LINE).

s2earth_lowest_altitude() ->
    not_loaded(?LINE).

s2earth_lowest_altitude_km() ->
    not_loaded(?LINE).

s2earth_lowest_altitude_meters() ->
    not_loaded(?LINE).

s2earth_highest_altitude() ->
    not_loaded(?LINE).

s2earth_highest_altitude_km() ->
    not_loaded(?LINE).

s2earth_highest_altitude_meters() ->
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

