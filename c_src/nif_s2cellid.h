#ifndef S2GEO_C_SRC_NIF_S2CELLID_H
#define S2GEO_C_SRC_NIF_S2CELLID_H

enum class S2CellIdConstructors {
  none                  = 1,
  sentinel              = 2,
  from_face_pos_level   = 3,
  from_point            = 4,
  from_lat_lng          = 5,
  begin                 = 6,
  end                   = 7,
  from_token            = 8,
  from_face_ij          = 9,
};

enum class S2CellIdFunctionZeroArgs {
  is_valid      = 10,
  to_point      = 11,
  to_lat_lng    = 12,
  face          = 13,
  pos           = 14,
  level         = 15,
  get_size_ij   = 16,
  get_size_st   = 17,
  is_leaf       = 18,
  is_face       = 19,
  range_min     = 20,
  range_max     = 21,
  parent        = 22,
  child_begin   = 23,
  child_end     = 24,
  next          = 25,
  prev          = 26,
  next_wrap     = 27,
  prev_wrap     = 28,
  to_token      = 29,
  to_string     = 30,
  lsb           = 31,
 };

enum class S2CellIdFunctionOneArg {
  get_size_ij       = 100,
  get_size_st       = 101,
  child_position    = 102,
  contains          = 103,
  intersects        = 104,
  parent            = 105,
  child             = 106,
  child_begin       = 107,
  child_end         = 108,
  advance           = 109,
  advance_wrap      = 110,
};

ERL_NIF_TERM s2cellid_get_size_ij(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM s2cellid_get_size_st(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM s2cellid_lsb_for_level(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM s2cellid_to_face_ij_orientation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM s2cellid_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM s2cellid_zero_args_fn(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM s2cellid_one_arg_fn(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif

