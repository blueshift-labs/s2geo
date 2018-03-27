#include "s2geo_nif.h"
#include "constants.h"
#include "nif_s1interval.h"
#include "nif_s2cellid.h"
//#include "nif_s2cellunion.h"
//#include "nif_s2latlngrect.h"
#include "nif_s2latlng.h"
//#include "nif_s2loop.h"
#include "nif_s2cap.h"
#include "nif_s2earth.h"

#include "nifpp_utils.h"

//#include "s2cell.h"
//#include "s2cellunion.h"
#include "s2/s2latlng.h"
#include "s2/s2latlng_rect.h"
//#include "s2loop.h"
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

    nifpp::register_resource<S2Cell>(env, nullptr, "S2Cell");
    nifpp::register_resource<S2LatLng>(env, nullptr, "S2LatLng");
    nifpp::register_resource<S2LatLngRect>(env, nullptr, "S2LatLngRect");
//    nifpp::register_resource<S2CellUnion>(env, nullptr, "S2CellUnion");
//    nifpp::register_resource<S2Loop>(env, nullptr, "S2Loop");
    nifpp::register_resource<S2Cap>(env, nullptr, "S2Cap");

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
    {"s1interval_get_directed_hausdorff_distance", 2, s1interval_get_directed_hausdorff_distance},
    {"s1interval_add_point", 2, s1interval_add_point},
    {"s1interval_project", 2, s1interval_project},
    {"s1interval_expanded", 2, s1interval_expanded},
    {"s1interval_union", 2, s1interval_union},
    {"s1interval_intersection", 2, s1interval_intersection},
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

    // {"s2latlngrect_constructor",    1, s2latlngrect_constructor},
    // {"s2latlngrect_constructor",    2, s2latlngrect_constructor},
    // {"s2latlngrect_constructor",    3, s2latlngrect_constructor},
    // {"s2latlngrect_methods",        2, s2latlngrect_methods},
    // {"s2latlngrect_methods",        3, s2latlngrect_methods},

    // {"s2latlngrect_get_covering",   3, s2region_coverer_get_covering_for_s2latlngrect},
    // {"s2latlngrect_get_covering",   4, s2region_coverer_get_covering_for_s2latlngrect},

    // {"s2cellunion_constructor",     2, s2cellunion_constructor},
    // {"s2cellunion_constructor",     3, s2cellunion_constructor},
    // {"s2cellunion_methods",         2, s2cellunion_methods},
    // {"s2cellunion_methods",         3, s2cellunion_methods},
    // {"s2cellunion_methods",         4, s2cellunion_methods},
    // {"s2cellunion_get_covering",    3, s2region_coverer_get_covering_for_s2cellunion},
    // {"s2cellunion_get_covering",    4, s2region_coverer_get_covering_for_s2cellunion},

    // {"s2loop_constructor",          2, s2loop_constructor},
    // {"s2loop_methods",              2, s2loop_methods},
    // {"s2loop_methods",              3, s2loop_methods},
    // {"s2loop_methods",              4, s2loop_methods},
    // {"s2loop_get_covering",         3, s2region_coverer_get_covering_for_s2loop},
    // {"s2loop_get_covering",         4, s2region_coverer_get_covering_for_s2loop},

    // {"s2cap_constructor",           1, s2cap_constructor},
    // {"s2cap_constructor",           3, s2cap_constructor},
    // {"s2cap_methods",               2, s2cap_methods},
    // {"s2cap_methods",               3, s2cap_methods},
    // {"s2cap_methods",               4, s2cap_methods},
    // {"s2cap_get_covering",          3, s2region_coverer_get_covering_for_s2cap},
    // {"s2cap_get_covering",          4, s2region_coverer_get_covering_for_s2cap},

    {"s2latlng_constructor",        1, s2latlng_constructor},
    {"s2latlng_constructor",        2, s2latlng_constructor},
    {"s2latlng_constructor",        3, s2latlng_constructor},
    {"s2latlng_methods",            2, s2latlng_methods},
    {"s2latlng_methods",            3, s2latlng_methods},
    {"s2latlng_methods",            4, s2latlng_methods},


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
