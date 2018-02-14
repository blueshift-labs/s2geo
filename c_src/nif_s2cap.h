#ifndef S2GEO_C_SRC_NIF_S2CAP_H
#define S2GEO_C_SRC_NIF_S2CAP_H

enum class S2CapConstructors {
  from_axis_height    = 1,
  from_axis_angle     = 2,
  from_axis_area      = 3,
  empty               = 4,
  full                = 5,

};

enum class S2CapMethods {
  clone               = 9,

  axis                = 10,
  height              = 11,
  area                = 12,
  angle               = 13,
  is_valid            = 14,
  is_empty            = 15,
  is_full             = 16,
  complement          = 17,

  contains            = 20,
  intersects          = 21,
  interior_intersects = 22,
  interior_contains   = 23,
  add_point           = 24,
  add_cap             = 25,
  expanded            = 26,

  get_cap_bound       = 30,
  get_rect_bound      = 31,

  contains_s2cellid   = 40,
  may_intersect_s2cellid  = 41,
  contains_s2point    = 42,
};

ERL_NIF_TERM s2cap_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM s2cap_methods(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM s2region_coverer_get_covering_for_s2cap(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif
