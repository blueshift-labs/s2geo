#ifndef S2GEO_C_SRC_NIF_S2LATLNGRECT_H
#define S2GEO_C_SRC_NIF_S2LATLNGRECT_H

enum class S2LatLngRectConstructors {
  from_lat_lng_degree   = 1,
  from_lat_lng          = 2,
  empty                 = 3,
  full                  = 4,
  from_center_size      = 5,
  from_point            = 6,
  from_point_pair       = 7,

  decode                = 10,
};

enum class S2LatLngRectMethods {
  lat_lo      = 101,
  lat_hi      = 102,
  lng_lo      = 103,
  lng_hi      = 104,
  lat         = 105,
  lng         = 106,
  lo          = 107,
  hi          = 108,

  is_valid    = 110,
  is_empty    = 111,
  is_full     = 112,
  is_point    = 113,
  is_inverted = 114,

  get_vertex  = 120,
  get_center  = 121,
  get_size    = 122,
  area        = 123,

  contains_s2latlng             = 130,
  interior_contains_s2point     = 131,
  interior_contains_s2latlng    = 132,
  contains_s2latlngrect         = 133,
  interior_contains_s2latlngrect  = 134,
  intersects_s2latlngrect       = 135,
  intersects_s2cell_with_id     = 136,
  interior_intersects_s2latlngrect  = 137,

  contains_s2cell_with_id = 138,
  contains_s2point = 139,
  may_intersect_s2cell_with_id = 140,

  expanded            = 142,
  Union               = 143,
  intersection        = 144,
  convolve_with_cap   = 145,

  add_point_s2latlng  = 146,
  add_point           = 147,

  get_distance              = 150,
  get_distance_s2latlng     = 151,
  get_directed_hausdorff_distance = 152,
  get_hausdorff_distance    = 153,

  equal = 160,
  not_equal = 161,
  approx_equals = 162,

  get_cap_bound = 170,
  get_rect_bound = 171,
  encode = 180,
};

ERL_NIF_TERM s2latlngrect_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM s2latlngrect_methods(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif

