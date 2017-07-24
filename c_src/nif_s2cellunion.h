#ifndef S2GEO_C_SRC_NIF_S2CELLUNION_H
#define S2GEO_C_SRC_NIF_S2CELLUNION_H

enum class S2CellUnionConstructors {
  init_from_cellids   = 1,
  init_from_uint64    = 2,
  decode              = 3,
  init_from_range     = 4,
};

enum class S2CellUnionMethods {
  num_cells           = 10,
  cell_id             = 11,
  cell_ids            = 12,
  normalize           = 13,

  contains_s2cellid             = 20,
  intersects_s2cellid           = 21,
  contains_s2cellunion          = 22,
  intersects_s2cellunion        = 23,
  get_union                     = 24,
  get_intersection              = 25,
  get_difference                = 26,
  get_intersection_with_s2cellid  = 27,

  expand_level                  = 28,
  expand_radius                 = 29,

  leaf_cells_covered            = 40,
  average_based_area            = 41,
  approx_area                   = 42,
  exact_area                    = 43,
  get_rect_bound                = 44,
  contains_s2cell               = 45,
  may_intersect_s2cell          = 46,
  contains_s2point              = 47,

  encode                        = 50,
};

ERL_NIF_TERM s2cellunion_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM s2cellunion_methods(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif

