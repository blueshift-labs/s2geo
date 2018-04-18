#include "s2/s2latlng.h"

#include "nifpp_utils.h"
#include "s2geo_nif.h"
#include "nif_s2latlng.h"

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)


ERL_NIF_TERM s2latlng_default_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
        return nifpp::make(env, S2LatLng());
}


ERL_NIF_TERM s2latlng_from_s1angle_lat_lng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S1Angle lat = nifpp::get<S1Angle>(env, nifpp::TERM(argv[0]));
        S1Angle lng = nifpp::get<S1Angle>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, S2LatLng(lat, lng).Normalized());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_from_point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Point p = nifpp::get<S2Point>(env, nifpp::TERM(argv[0]));
        return nifpp::make(env, S2LatLng(p));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_invalid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   return nifpp::make(env, S2LatLng::Invalid());
}


ERL_NIF_TERM s2latlng_from_radians(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        double p1 = get_double_from_term(env, argv[0]);
        double p2 = get_double_from_term(env, argv[1]);
        return nifpp::make(env, S2LatLng::FromRadians(p1, p2));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_from_degrees(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        double p1 = get_double_from_term(env, argv[0]);
        double p2 = get_double_from_term(env, argv[1]);
        return nifpp::make(env, S2LatLng::FromDegrees(p1, p2));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_from_e5(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        int lat_e5 = nifpp::get<int>(env, nifpp::TERM(argv[0]));
        int lng_e5 = nifpp::get<int>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, S2LatLng::FromE5(lat_e5, lng_e5));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_from_e6(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        int lat_e6 = nifpp::get<int>(env, nifpp::TERM(argv[0]));
        int lng_e6 = nifpp::get<int>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, S2LatLng::FromE6(lat_e6, lng_e6));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_from_e7(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        int lat_e7 = nifpp::get<int>(env, nifpp::TERM(argv[0]));
        int lng_e7 = nifpp::get<int>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, S2LatLng::FromE7(lat_e7, lng_e7));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_from_unsigned_e6(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        auto lat_e6 = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto lng_e6 = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, S2LatLng::FromUnsignedE6(lat_e6, lng_e6));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_from_unsigned_e7(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        auto lat_e7 = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto lng_e7 = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, S2LatLng::FromUnsignedE6(lat_e7, lng_e7));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2latlng_latitude(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Point p = nifpp::get<S2Point>(env, nifpp::TERM(argv[0]));
        return nifpp::make(env, S2LatLng::Latitude(p));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_longitude(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Point p = nifpp::get<S2Point>(env, nifpp::TERM(argv[0]));
        return nifpp::make(env, S2LatLng::Longitude(p));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2latlng_lat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.lat());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_lng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.lng());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_coords(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.coords());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_is_valid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.is_valid());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_normalized(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.Normalized());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_to_point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.ToPoint());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_get_distance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);

        S2LatLng other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self.GetDistance(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2latlng_add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLng other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self + other);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_subtract(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLng other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self - other);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_multiply(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        double m = get_double_from_term(env, argv[1]);
        return nifpp::make(env, self * m);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2latlng_eq(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLng other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self == other);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_neq(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLng other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self != other);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_lt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLng other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self < other);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_gt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);

        S2LatLng other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self > other);

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_leq(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLng other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self <= other);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_geq(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLng other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self >= other);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2latlng_approx_equals(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 3);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);

        S2LatLng other;
        nifpp::get_throws(env, argv[1], other);
        S1Angle max_error = nifpp::get<S1Angle>(env, nifpp::TERM(argv[2]));
        return nifpp::make(env, self.ApproxEquals(other, max_error));

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_to_string_in_degrees(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.ToStringInDegrees());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}
