#ifndef S2GEO_C_SRC_NIF_S2LATLNG_H
#define S2GEO_C_SRC_NIF_S2LATLNG_H

enum class S2LatLngConstructors {
    default_constructor         = 1,
    from_s1angle_lat_lng        = 2,
    from_point                  = 3,
    invalid                     = 4,
    from_radians                = 5,
    from_degrees                = 6,
    from_e5                     = 7,
    from_e6                     = 8,
    from_e7                     = 9,
    from_unsigned_e6            = 10,
    from_unsigned_e7            = 11,

    latitude                    = 12,
    longitude                   = 13,
};

enum class S2LatLngMethods {
    lat                         = 20,
    lng                         = 21,
    coords                      = 22,
    is_valid                    = 23,
    normalized                  = 24,
    to_point                    = 25,
    get_distance                = 26,

    add                         = 27,
    subtract                    = 28,
    multiply                    = 29,

    eq                          = 30,
    neq                        = 31,
    lt                          = 32,
    gt                          = 33,
    leq                         = 34,
    geq                         = 35,

    approx_equals               = 36,
    to_string_in_degrees        = 37,
};

ERL_NIF_TERM s2latlng_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM s2latlng_methods(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif
