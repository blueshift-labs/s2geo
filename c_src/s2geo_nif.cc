#include "s2geo_nif.h"
#include "constants.h"
#include "nif_s1interval.h"
#include "nif_s2cellid.h"
#include "nif_s2cellunion.h"
#include "nif_s2latlngrect.h"
#include "nif_s2latlng.h"
#include "nif_s2loop.h"
#include "nif_s2cap.h"
#include "nif_s2earth.h"

#include "nifpp_utils.h"

#include "s2/s2cell.h"
#include "s2/s2cell_union.h"
#include "s2/s2latlng.h"
#include "s2/s2latlng_rect.h"
#include "s2/s2loop.h"
#include "s2/s2cap.h"

atoms ATOMS;

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;

    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1))
        return ret;

    return enif_make_atom(env, name);
}

static int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{

    ATOMS.atomOk = make_atom(env, s2geo::kAtomOk);
    ATOMS.atomError = make_atom(env, s2geo::kAtomError);
    ATOMS.atomTrue = make_atom(env, s2geo::kAtomTrue);
    ATOMS.atomFalse = make_atom(env, s2geo::kAtomFalse);
    ATOMS.atomNull = make_atom(env, s2geo::kAtomNull);
    ATOMS.atomBadArg = make_atom(env, s2geo::kAtomBadArg);
    ATOMS.atomOptions = make_atom(env, s2geo::kAtomOptions);
    ATOMS.atomNotImplemented = make_atom(env, s2geo::kAtomNotImplemented);
    ATOMS.atomInternalError = make_atom(env, s2geo::kAtomInternalError);

    nifpp::register_resource<S2Cell>(env, "s2geometry", "S2Cell");
    nifpp::register_resource<S2LatLngRect>(env, "s2geometry", "S2LatLngRect");
    nifpp::register_resource<S2CellUnion>(env, "s2geometry", "S2CellUnion");
    nifpp::register_resource<S2Cap>(env, "s2geometry", "S2Cap");
    nifpp::register_resource<S2Loop>(env, "s2geometry", "S2Loop");
    nifpp::register_resource<NifS2LoopRef>(env, "s2geometry", "NifS2LoopRef");
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"s1interval_default_constructor", 0, s1interval_default_constructor},
    {"s1interval_empty", 0, s1interval_empty},
    {"s1interval_full", 0, s1interval_full},
    {"s1interval_from_hi_lo", 2, s1interval_from_hi_lo},
    {"s1interval_from_point", 1, s1interval_from_point},
    {"s1interval_from_point_pair", 2, s1interval_from_point_pair},
    {"s1interval_hi", 1, s1interval_hi},
    {"s1interval_lo", 1, s1interval_lo},
    {"s1interval_bounds", 1, s1interval_bounds},
    {"s1interval_bound", 2, s1interval_bound},
    {"s1interval_is_valid", 1, s1interval_is_valid},
    {"s1interval_is_full", 1, s1interval_is_full},
    {"s1interval_is_empty", 1, s1interval_is_empty},
    {"s1interval_is_inverted", 1, s1interval_is_inverted},
    {"s1interval_get_center", 1, s1interval_get_center},
    {"s1interval_get_length", 1, s1interval_get_length},
    {"s1interval_complement", 1, s1interval_complement},
    {"s1interval_get_complement_center", 1, s1interval_get_complement_center},
    {"s1interval_interior_contains_double", 2, s1interval_interior_contains_double},
    {"s1interval_contains_double", 2, s1interval_contains_double},
    {"s1interval_contains", 2, s1interval_contains},
    {"s1interval_interior_contains", 2, s1interval_interior_contains},
    {"s1interval_intersects", 2, s1interval_intersects},
    {"s1interval_interior_intersects", 2, s1interval_interior_intersects},
    {"s1interval_get_directed_hausdorff_distance", 2, s1interval_get_directed_hausdorff_distance},
    {"s1interval_add_point", 2, s1interval_add_point},
    {"s1interval_project", 2, s1interval_project},
    {"s1interval_expanded", 2, s1interval_expanded},
    {"s1interval_union", 2, s1interval_union},
    {"s1interval_intersection", 2, s1interval_intersection},
    {"s1interval_equals", 2, s1interval_equals},
    {"s1interval_approx_equals", 3, s1interval_approx_equals},

    {"s2cellid_from_s2point", 1, s2cellid_from_s2point},
    {"s2cellid_from_lat_lng", 1, s2cellid_from_lat_lng},
    {"s2cellid_none", 0, s2cellid_none},
    {"s2cellid_sentinel", 0, s2cellid_sentinel},
    {"s2cellid_from_face", 1, s2cellid_from_face},
    {"s2cellid_from_face_pos_level", 3, s2cellid_from_face_pos_level},
    {"s2cellid_to_point", 1, s2cellid_to_point},
    {"s2cellid_to_point_raw", 1, s2cellid_to_point_raw},
    {"s2cellid_get_center_st", 1, s2cellid_get_center_st},
    {"s2cellid_get_size_st", 1, s2cellid_get_size_st},
    {"s2cellid_get_bound_st", 1, s2cellid_get_bound_st},
    {"s2cellid_get_center_uv", 1, s2cellid_get_center_uv},
    {"s2cellid_get_bound_uv", 1, s2cellid_get_bound_uv},
    {"s2cellid_expanded_by_distance_uv", 2, s2cellid_expanded_by_distance_uv},
    {"s2cellid_get_center_si_ti", 1, s2cellid_get_center_si_ti},
    {"s2cellid_to_lat_lng", 1, s2cellid_to_lat_lng},
    {"s2cellid_is_valid", 1, s2cellid_is_valid},
    {"s2cellid_face", 1, s2cellid_face},
    {"s2cellid_pos", 1, s2cellid_pos},
    {"s2cellid_level", 1, s2cellid_level},
    {"s2cellid_get_size_ij", 1, s2cellid_get_size_ij},
    {"s2cellid_get_size_ij_level", 1, s2cellid_get_size_ij_level},
    {"s2cellid_is_leaf", 1, s2cellid_is_leaf},
    {"s2cellid_is_face", 1, s2cellid_is_face},
    {"s2cellid_child_position", 1, s2cellid_child_position},
    {"s2cellid_range_min", 1, s2cellid_range_min},
    {"s2cellid_range_max", 1, s2cellid_range_max},
    {"s2cellid_contains", 2, s2cellid_contains},
    {"s2cellid_intersects", 2, s2cellid_intersects},
    {"s2cellid_parent", 1, s2cellid_parent},
    {"s2cellid_parent", 2, s2cellid_parent},
    {"s2cellid_child", 1, s2cellid_child},
    {"s2cellid_child", 2, s2cellid_child},
    {"s2cellid_child_begin", 1, s2cellid_child_begin},
    {"s2cellid_child_begin", 2, s2cellid_child_begin},
    {"s2cellid_child_end", 1, s2cellid_child_end},
    {"s2cellid_child_end", 2, s2cellid_child_end},
    {"s2cellid_next", 1, s2cellid_next},
    {"s2cellid_prev", 1, s2cellid_prev},
    {"s2cellid_advance", 2, s2cellid_advance},
    {"s2cellid_distance_from_begin", 1, s2cellid_distance_from_begin},
    {"s2cellid_next_wrap", 1, s2cellid_next_wrap},
    {"s2cellid_prev_wrap", 1, s2cellid_prev_wrap},
    {"s2cellid_advance_wrap", 2, s2cellid_advance_wrap},
    {"s2cellid_maximum_tile", 2, s2cellid_maximum_tile},
    {"s2cellid_get_common_ancestor_level", 2, s2cellid_get_common_ancestor_level},
    {"s2cellid_begin", 1, s2cellid_begin},
    {"s2cellid_end", 1, s2cellid_end},
    {"s2cellid_to_token", 1, s2cellid_to_token},
    {"s2cellid_from_token", 1, s2cellid_from_token},
    {"s2cellid_to_string", 1, s2cellid_to_string},
    {"s2cellid_get_edge_neighbors", 1, s2cellid_get_edge_neighbors},
    {"s2cellid_get_vertex_neighbors", 2, s2cellid_get_vertex_neighbors},
    {"s2cellid_get_all_neighbors", 2, s2cellid_get_all_neighbors},
    {"s2cellid_from_face_ij", 3, s2cellid_from_face_ij},
    {"s2cellid_to_face_ij_orientation", 1, s2cellid_to_face_ij_orientation},
    {"s2cellid_lsb", 1, s2cellid_lsb},
    {"s2cellid_lsb_for_level", 1, s2cellid_lsb_for_level},
    {"s2cellid_ij_level_to_bound_uv", 1, s2cellid_ij_level_to_bound_uv},


    {"s2cellunion_new_from_cellids", 1, s2cellunion_new_from_cellids},
    {"s2cellunion_new_from_normalized_cellids", 1, s2cellunion_new_from_normalized_cellids},
    {"s2cellunion_new_from_min_max", 2, s2cellunion_new_from_min_max},
    {"s2cellunion_new_from_begin_end", 2, s2cellunion_new_from_begin_end},
    {"s2cellunion_num_cells", 1, s2cellunion_num_cells},
    {"s2cellunion_cell_id", 2, s2cellunion_cell_id},
    {"s2cellunion_cell_ids", 1, s2cellunion_cell_ids},
    {"s2cellunion_is_valid", 1, s2cellunion_is_valid},
    {"s2cellunion_is_normalized", 1, s2cellunion_is_normalized},
    {"s2cellunion_normalize", 1, s2cellunion_normalize},
    {"s2cellunion_denormalize", 3, s2cellunion_denormalize},
    {"s2cellunion_pack", 2, s2cellunion_pack},
    {"s2cellunion_contains_s2cellid", 2, s2cellunion_contains_s2cellid},
    {"s2cellunion_intersects_s2cellid", 2, s2cellunion_intersects_s2cellid},
    {"s2cellunion_contains_s2cellunion", 2, s2cellunion_contains_s2cellunion},
    {"s2cellunion_intersects_s2cellunion", 2, s2cellunion_intersects_s2cellunion},
    {"s2cellunion_union", 2, s2cellunion_union},
    {"s2cellunion_intersection", 2, s2cellunion_intersection},
    {"s2cellunion_intersection_with_s2cellid", 2, s2cellunion_intersection_with_s2cellid},
    {"s2cellunion_difference", 2, s2cellunion_difference},
    {"s2cellunion_expand_level", 2, s2cellunion_expand_level},
    {"s2cellunion_expand_radius", 3, s2cellunion_expand_radius},
    {"s2cellunion_leaf_cells_covered", 1, s2cellunion_leaf_cells_covered},
    {"s2cellunion_average_based_area", 1, s2cellunion_average_based_area},
    {"s2cellunion_approx_area", 1, s2cellunion_approx_area},
    {"s2cellunion_exact_area", 1, s2cellunion_exact_area},
    {"s2cellunion_get_cap_bound", 1, s2cellunion_get_cap_bound},
    {"s2cellunion_get_rect_bound", 1, s2cellunion_get_rect_bound},
    {"s2cellunion_equal", 2, s2cellunion_equal},
    {"s2cellunion_not_equal", 2, s2cellunion_not_equal},
    {"s2cellunion_contains_s2cell", 2, s2cellunion_contains_s2cell},
    {"s2cellunion_may_intersect_s2cellid", 2, s2cellunion_may_intersect_s2cellid},
    {"s2cellunion_may_intersect_s2cell", 2, s2cellunion_may_intersect_s2cell},
    {"s2cellunion_contains_s2point", 2, s2cellunion_contains_s2point},
    {"s2cellunion_encode", 1, s2cellunion_encode},
    {"s2cellunion_decode", 1, s2cellunion_decode},


    {"s2latlngrect_from_lat_lng", 2, s2latlngrect_from_lat_lng},
    {"s2latlngrect_from_r1inteval_s1interval", 2, s2latlngrect_from_r1inteval_s1interval},
    {"s2latlngrect_from_center_size", 2, s2latlngrect_from_center_size},
    {"s2latlngrect_from_point", 1, s2latlngrect_from_point},
    {"s2latlngrect_from_point_pair", 2, s2latlngrect_from_point_pair},
    {"s2latlngrect_empty", 0, s2latlngrect_empty},
    {"s2latlngrect_full", 0, s2latlngrect_full},
    {"s2latlngrect_full_lat", 0, s2latlngrect_full_lat},
    {"s2latlngrect_full_lng", 0, s2latlngrect_full_lng},

    {"s2latlngrect_lat_lo", 1, s2latlngrect_lat_lo},
    {"s2latlngrect_lat_hi", 1, s2latlngrect_lat_hi},
    {"s2latlngrect_lng_lo", 1, s2latlngrect_lng_lo},
    {"s2latlngrect_lng_hi", 1, s2latlngrect_lng_hi},
    {"s2latlngrect_lat", 1, s2latlngrect_lat},
    {"s2latlngrect_lng", 1, s2latlngrect_lng},
    {"s2latlngrect_lo", 1, s2latlngrect_lo},
    {"s2latlngrect_hi", 1, s2latlngrect_hi},
    {"s2latlngrect_is_valid", 1, s2latlngrect_is_valid},
    {"s2latlngrect_is_empty", 1, s2latlngrect_is_empty},
    {"s2latlngrect_is_full", 1, s2latlngrect_is_full},
    {"s2latlngrect_is_point", 1, s2latlngrect_is_point},
    {"s2latlngrect_is_inverted", 1, s2latlngrect_is_inverted},
    {"s2latlngrect_get_vertex", 1, s2latlngrect_get_vertex},
    {"s2latlngrect_get_center", 1, s2latlngrect_get_center},
    {"s2latlngrect_get_size", 1, s2latlngrect_get_size},
    {"s2latlngrect_area", 1, s2latlngrect_area},
    {"s2latlngrect_contains_s2latlng", 2, s2latlngrect_contains_s2latlng},
    {"s2latlngrect_interior_contains_s2point", 2, s2latlngrect_interior_contains_s2point},
    {"s2latlngrect_interior_contains_s2latlng", 2, s2latlngrect_interior_contains_s2latlng},
    {"s2latlngrect_contains_s2latlngrect", 2, s2latlngrect_contains_s2latlngrect},
    {"s2latlngrect_interior_contains_s2latlngrect", 2, s2latlngrect_interior_contains_s2latlngrect},
    {"s2latlngrect_intersects_s2latlngrect", 2, s2latlngrect_intersects_s2latlngrect},
    {"s2latlngrect_intersects_s2cell_with_id", 2, s2latlngrect_intersects_s2cell_with_id},
    {"s2latlngrect_interior_intersects_s2latlngrect", 2, s2latlngrect_interior_intersects_s2latlngrect},
    {"s2latlngrect_boundary_intersects", 3, s2latlngrect_boundary_intersects},
    {"s2latlngrect_contains_s2cell_with_id", 2, s2latlngrect_contains_s2cell_with_id},
    {"s2latlngrect_contains_s2point", 2, s2latlngrect_contains_s2point},
    {"s2latlngrect_may_intersect_s2cell_with_id", 2, s2latlngrect_may_intersect_s2cell_with_id},
    {"s2latlngrect_expanded", 2, s2latlngrect_expanded},
    {"s2latlngrect_union", 2, s2latlngrect_union},
    {"s2latlngrect_intersection", 2, s2latlngrect_intersection},

    {"s2latlngrect_add_point_s2latlng", 2, s2latlngrect_add_point_s2latlng},
    {"s2latlngrect_add_point", 2, s2latlngrect_add_point},
    {"s2latlngrect_get_distance", 2, s2latlngrect_get_distance},
    {"s2latlngrect_get_distance_s2latlng", 2, s2latlngrect_get_distance_s2latlng},
    {"s2latlngrect_get_directed_hausdorff_distance", 2, s2latlngrect_get_directed_hausdorff_distance},
    {"s2latlngrect_get_hausdorff_distance", 2, s2latlngrect_get_hausdorff_distance},
    {"s2latlngrect_equal", 2, s2latlngrect_equal},
    {"s2latlngrect_not_equal", 2, s2latlngrect_not_equal},

    {"s2latlngrect_approx_equals_with_s1angle_error", 2, s2latlngrect_approx_equals_with_s1angle_error},
    {"s2latlngrect_approx_equals_with_s1angle_error", 3, s2latlngrect_approx_equals_with_s1angle_error},
    {"s2latlngrect_approx_equals_with_s2latlng_error", 3, s2latlngrect_approx_equals_with_s2latlng_error},

    {"s2latlngrect_get_cap_bound", 1, s2latlngrect_get_cap_bound},
    {"s2latlngrect_get_rect_bound", 1, s2latlngrect_get_rect_bound},

    {"s2latlngrect_decode", 1, s2latlngrect_decode},
    {"s2latlngrect_encode", 1, s2latlngrect_encode},

    {"s2latlngrect_intersects_lng_edge", 4, s2latlngrect_intersects_lng_edge},
    {"s2latlngrect_intersects_lat_edge", 4, s2latlngrect_intersects_lat_edge},

    // {"s2latlngrect_get_covering",   3, s2region_coverer_get_covering_for_s2latlngrect},
    // {"s2latlngrect_get_covering",   4, s2region_coverer_get_covering_for_s2latlngrect},


    {"s2loop_new_from_s2point_list", 1, s2loop_new_from_s2point_list},
    {"s2loop_new_from_s2latlng_list", 1, s2loop_new_from_s2latlng_list},
    {"s2loop_new_from_s2cellid", 1, s2loop_new_from_s2cellid},
    {"s2loop_new_from_s2cell", 1, s2loop_new_from_s2cell},
    {"s2loop_is_valid", 1, s2loop_is_valid},
    {"s2loop_num_vertices", 1, s2loop_num_vertices},
    {"s2loop_vertex", 2, s2loop_vertex},
    {"s2loop_oriented_vertex", 2, s2loop_oriented_vertex},
    {"s2loop_is_empty", 1, s2loop_is_empty},
    {"s2loop_is_full", 1, s2loop_is_full},
    {"s2loop_is_empty_or_full", 1, s2loop_is_empty_or_full},
    {"s2loop_depth", 1, s2loop_depth},
    {"s2loop_set_depth", 2, s2loop_set_depth},
    {"s2loop_is_hole", 1, s2loop_is_hole},
    {"s2loop_sign", 1, s2loop_sign},
    {"s2loop_is_normalized", 1, s2loop_is_normalized},
    {"s2loop_normalize", 1, s2loop_normalize},
    {"s2loop_invert", 1, s2loop_invert},
    {"s2loop_get_area", 1, s2loop_get_area},
    {"s2loop_get_centroid", 1, s2loop_get_centroid},
    {"s2loop_get_turning_angle", 1, s2loop_get_turning_angle},
    {"s2loop_get_turning_angle_max_error", 1, s2loop_get_turning_angle_max_error},
    {"s2loop_get_distance", 2, s2loop_get_distance},
    {"s2loop_get_distance_to_boundary", 2, s2loop_get_distance_to_boundary},
    {"s2loop_project", 2, s2loop_project},
    {"s2loop_project_to_boundary", 2, s2loop_project_to_boundary},
    {"s2loop_contains_s2loop", 2, s2loop_contains_s2loop},
    {"s2loop_contains_s2cell", 2, s2loop_contains_s2cell},
    {"s2loop_contains_s2cellid", 2, s2loop_contains_s2cellid},
    {"s2loop_contains_s2point", 2, s2loop_contains_s2point},
    {"s2loop_intersects", 2, s2loop_intersects},
    {"s2loop_equals", 2, s2loop_equals},
    {"s2loop_boundary_equals", 2, s2loop_boundary_equals},
    {"s2loop_boundary_approx_equals", 3, s2loop_boundary_approx_equals},
    {"s2loop_boundary_near", 3, s2loop_boundary_near},
    {"s2loop_clone", 1, s2loop_clone},
    {"s2loop_get_cap_bound", 1, s2loop_get_cap_bound},
    {"s2loop_get_rect_bound", 1, s2loop_get_rect_bound},
    {"s2loop_may_intersect_s2cell", 2, s2loop_may_intersect_s2cell},
    {"s2loop_may_intersect_s2cellid", 2, s2loop_may_intersect_s2cellid},
    {"s2loop_encode", 1, s2loop_encode},
    {"s2loop_decode", 1, s2loop_decode},
    {"s2loop_contains_nested", 2, s2loop_contains_nested},
    {"s2loop_compare_boundary", 2, s2loop_compare_boundary},
    {"s2loop_contains_non_crossing_boundary", 3, s2loop_contains_non_crossing_boundary},


    {"s2cap_from_s2point_s1angle", 2, s2cap_from_s2point_s1angle},
    {"s2cap_from_s2point_s1chordangle", 2, s2cap_from_s2point_s1chordangle},
    {"s2cap_from_point", 1, s2cap_from_point},
    {"s2cap_from_center_height", 2, s2cap_from_center_height},
    {"s2cap_from_center_area", 2, s2cap_from_center_area},
    {"s2cap_empty", 0, s2cap_empty},
    {"s2cap_full", 0, s2cap_full},

    {"s2cap_center", 1, s2cap_center},
    {"s2cap_radius", 1, s2cap_radius},
    {"s2cap_height", 1, s2cap_height},
    {"s2cap_get_radius", 1, s2cap_get_radius},
    {"s2cap_get_area", 1, s2cap_get_area},
    {"s2cap_get_centroid", 1, s2cap_get_centroid},

    {"s2cap_is_valid", 1, s2cap_is_valid},
    {"s2cap_is_empty", 1, s2cap_is_empty},
    {"s2cap_is_full", 1, s2cap_is_full},

    {"s2cap_complement", 1, s2cap_complement},
    {"s2cap_contains", 2, s2cap_contains},
    {"s2cap_intersects", 2, s2cap_intersects},
    {"s2cap_interior_intersects", 2, s2cap_interior_intersects},
    {"s2cap_interior_contains", 2, s2cap_interior_contains},

    {"s2cap_add_point", 2, s2cap_add_point},
    {"s2cap_add_cap", 2, s2cap_add_cap},
    {"s2cap_expanded", 2, s2cap_expanded},
    {"s2cap_union", 2, s2cap_union},
    {"s2cap_clone", 1, s2cap_clone},
    {"s2cap_get_cap_bound", 1, s2cap_get_cap_bound},
    {"s2cap_get_rect_bound", 1, s2cap_get_rect_bound},
    {"s2cap_get_cell_union_bound", 1, s2cap_get_cell_union_bound},
    {"s2cap_contains_s2cellid", 2, s2cap_contains_s2cellid},
    {"s2cap_contains_s2cell", 2, s2cap_contains_s2cell},
    {"s2cap_may_intersect_s2cellid", 2, s2cap_may_intersect_s2cellid},
    {"s2cap_may_intersect_s2cell", 2, s2cap_may_intersect_s2cell},
    {"s2cap_contains_s2point", 2, s2cap_contains_s2point},

    {"s2cap_encode", 1, s2cap_encode},
    {"s2cap_decode", 1, s2cap_decode},
    {"s2cap_equal", 2, s2cap_equal},
    {"s2cap_approx_equals", 3, s2cap_approx_equals},


    {"s2latlng_default_constructor", 0, s2latlng_default_constructor},
    {"s2latlng_from_s1angle_lat_lng", 2, s2latlng_from_s1angle_lat_lng},
    {"s2latlng_from_point", 1, s2latlng_from_point},
    {"s2latlng_invalid", 0, s2latlng_invalid},
    {"s2latlng_from_radians", 2, s2latlng_from_radians},
    {"s2latlng_from_degrees", 2, s2latlng_from_degrees},
    {"s2latlng_from_e5", 2, s2latlng_from_e5},
    {"s2latlng_from_e6", 2, s2latlng_from_e6},
    {"s2latlng_from_e7", 2, s2latlng_from_e7},
    {"s2latlng_from_unsigned_e6", 2, s2latlng_from_unsigned_e6},
    {"s2latlng_from_unsigned_e7", 2, s2latlng_from_unsigned_e7},

    {"s2latlng_latitude", 1, s2latlng_latitude},
    {"s2latlng_longitude", 1, s2latlng_longitude},

    {"s2latlng_lat", 1, s2latlng_lat},
    {"s2latlng_lng", 1, s2latlng_lng},
    {"s2latlng_coords", 1, s2latlng_coords},
    {"s2latlng_is_valid", 1, s2latlng_is_valid},
    {"s2latlng_normalized", 1, s2latlng_normalized},
    {"s2latlng_to_point", 1, s2latlng_to_point},
    {"s2latlng_get_distance", 2, s2latlng_get_distance},

    {"s2latlng_add", 2, s2latlng_add},
    {"s2latlng_subtract", 2, s2latlng_subtract},
    {"s2latlng_multiply", 2, s2latlng_multiply},

    {"s2latlng_eq", 2, s2latlng_eq},
    {"s2latlng_neq", 2, s2latlng_neq},
    {"s2latlng_lt", 2, s2latlng_lt},
    {"s2latlng_gt", 2, s2latlng_gt},
    {"s2latlng_leq", 2, s2latlng_leq},
    {"s2latlng_geq", 2, s2latlng_geq},

    {"s2latlng_approx_equals", 3, s2latlng_approx_equals},
    {"s2latlng_to_string_in_degrees", 1, s2latlng_to_string_in_degrees},


    {"s2earth_to_s1angle", 1, s2earth_to_s1angle},
    {"s2earth_to_distance", 1, s2earth_to_distance},
    {"s2earth_to_radians", 1, s2earth_to_radians},
    {"s2earth_to_meters", 1, s2earth_to_meters},
    {"s2earth_to_km", 1, s2earth_to_km},
    {"s2earth_km_to_radians", 1, s2earth_km_to_radians},
    {"s2earth_radians_to_km", 1, s2earth_radians_to_km},
    {"s2earth_meters_to_radians", 1, s2earth_meters_to_radians},
    {"s2earth_radians_to_meters", 1, s2earth_radians_to_meters},
    {"s2earth_square_km_to_steradians", 1, s2earth_square_km_to_steradians},
    {"s2earth_square_meters_to_steradians", 1, s2earth_square_meters_to_steradians},
    {"s2earth_steradians_to_square_km", 1, s2earth_steradians_to_square_km},
    {"s2earth_steradians_to_square_meters", 1, s2earth_steradians_to_square_meters},
    {"s2earth_to_longitude_radians", 2, s2earth_to_longitude_radians},
    {"s2earth_get_initial_bearing", 2, s2earth_get_initial_bearing},
    {"s2earth_get_distance_s2point", 2, s2earth_get_distance_s2point},
    {"s2earth_get_distance_s2latlng", 2, s2earth_get_distance_s2latlng},
    {"s2earth_get_distance_km_s2point", 2, s2earth_get_distance_km_s2point},
    {"s2earth_get_distance_km_s2latlng", 2, s2earth_get_distance_km_s2latlng},
    {"s2earth_get_distance_meters_s2point", 2, s2earth_get_distance_meters_s2point},
    {"s2earth_get_distance_meters_s2latlng", 2, s2earth_get_distance_meters_s2latlng},

    {"s2earth_radius_km", 0, s2earth_radius_km},
    {"s2earth_radius_meters", 0, s2earth_radius_meters},
    {"s2earth_lowest_altitude_km", 0, s2earth_lowest_altitude_km},
    {"s2earth_lowest_altitude_meters", 0, s2earth_lowest_altitude_meters},
    {"s2earth_highest_altitude_km", 0, s2earth_highest_altitude_km},
    {"s2earth_highest_altitude_meters", 0, s2earth_highest_altitude_meters},

};

ERL_NIF_INIT(s2geo_nif, nif_funcs, on_nif_load, NULL, NULL, NULL);
