#ifndef S2GEO_C_SRC_NIF_S1INTERVAL_H
#define S2GEO_C_SRC_NIF_S1INTERVAL_H

enum class S1IntervalConstructors {
    default_constructor         = 1,
    empty                       = 2,
    full                        = 3,
    from_hi_lo                  = 4,
    from_point                  = 5,
    from_point_pair             = 6,
};

enum class S1IntervalMethods {
    hi                          = 11,
    lo                          = 12,
    bounds                      = 13,
    bound                       = 14,
    is_valid                    = 15,
    is_full                     = 16,
    is_empty                    = 17,
    is_inverted                 = 18,
    get_center                  = 19,
    get_length                  = 20,
    complement                  = 21,
    get_complement_center       = 22,
    interior_contains_double    = 23,
    contains_double             = 24,
    contains                    = 25,
    interior_contains           = 26,
    get_directed_hausdorff_distance = 27,
    add_point                   = 28,
    expanded                    = 29,
    Union                       = 30,
    intersection                = 31,
    approx_equals               = 32,
};

ERL_NIF_TERM s1interval_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM s1interval_methods(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif
