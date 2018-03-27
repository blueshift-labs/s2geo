// #ifndef S2GEO_C_SRC_NIF_S2LOOP_H
// #define S2GEO_C_SRC_NIF_S2LOOP_H

// enum class S2LoopConstructors {
//   from_s2point_list    = 1,
//   from_s2latlng_list   = 2,
//   from_s2cellid        = 3,
//   decode               = 4,
//   invert               = 5,
// };

// enum class S2LoopMethods {
//   is_valid           = 10,
//   depth              = 11,
//   is_hole            = 12,
//   sign               = 13,
//   num_vertices       = 14,
//   vertex             = 15,
//   is_normalized      = 16,
//   normalize          = 17, // Not implemented
//   invert             = 18,  // Use Constructor
//   get_area           = 19,
//   get_centroid       = 20,
//   get_turning_angle  = 21,
//   contains           = 22,
//   intersects         = 23,
//   contains_nested    = 24,
//   contains_or_crosses     = 25,
//   boundary_equals    = 26,
//   boundary_approx_equals  = 27,
//   boundary_near      = 28,

//   get_rect_bound     = 29,

//   contains_s2cellid  = 30,
//   may_intersect_s2cellid  = 31,
//   contains_s2point   = 32,
//   encode             = 33,
// };

// ERL_NIF_TERM s2loop_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
// ERL_NIF_TERM s2loop_methods(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
// ERL_NIF_TERM s2region_coverer_get_covering_for_s2loop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// #endif
