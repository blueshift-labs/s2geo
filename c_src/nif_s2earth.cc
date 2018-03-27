#include "s2/s2earth.h"

#include "nifpp_utils.h"
#include "s2geo_nif.h"
#include "nif_s2earth.h"

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)


ERL_NIF_TERM s2earth_to_s1angle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        double distance;
        nifpp::get_throws(env, argv[0], distance);
        auto s1angle = S2Earth::ToAngle(util::units::Meters(distance));
        return nifpp::make(env, s1angle);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_to_distance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S1Angle angle = nifpp::get<S1Angle>(env, nifpp::TERM(argv[0]));
        util::units::Meters meters =  S2Earth::ToDistance(angle);

        return nifpp::make(env, meters.value());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2earth_to_radians(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        double distance;
        nifpp::get_throws(env, argv[0], distance);
        return nifpp::make(env, S2Earth::ToRadians(util::units::Meters(distance)));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_to_meters(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S1Angle angle = nifpp::get<S1Angle>(env, nifpp::TERM(argv[0]));
        return nifpp::make(env, S2Earth::ToMeters(angle));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_to_km(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S1Angle angle = nifpp::get<S1Angle>(env, nifpp::TERM(argv[0]));
        return nifpp::make(env, S2Earth::ToKm(angle));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_km_to_radians(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        double km;
        nifpp::get_throws(env, argv[0], km);
        return nifpp::make(env, S2Earth::KmToRadians(km));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_radians_to_km(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        double radians;
        nifpp::get_throws(env, argv[0], radians);
        return nifpp::make(env, S2Earth::RadiansToKm(radians));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_meters_to_radians(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        double meters;
        nifpp::get_throws(env, argv[0], meters);
        return nifpp::make(env, S2Earth::MetersToRadians(meters));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_radians_to_meters(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        double radians;
        nifpp::get_throws(env, argv[0], radians);
        return nifpp::make(env, S2Earth::RadiansToMeters(radians));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_square_km_to_steradians(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        double km2;
        nifpp::get_throws(env, argv[0], km2);
        return nifpp::make(env, S2Earth::SquareKmToSteradians(km2));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_square_meters_to_steradians(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        double m2;
        nifpp::get_throws(env, argv[0], m2);
        return nifpp::make(env, S2Earth::SquareMetersToSteradians(m2));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_steradians_to_square_km(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        double steradians;
        nifpp::get_throws(env, argv[0], steradians);
        return nifpp::make(env, S2Earth::SteradiansToSquareKm(steradians));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_steradians_to_square_meters(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        double steradians;
        nifpp::get_throws(env, argv[0], steradians);
        return nifpp::make(env, S2Earth::SteradiansToSquareMeters(steradians));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_to_longitude_radians(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        double distance, latitude_radians;
        nifpp::get_throws(env, argv[0], distance);
        nifpp::get_throws(env, argv[1], latitude_radians);
        return nifpp::make(env, S2Earth::ToLongitudeRadians(util::units::Meters(distance), latitude_radians));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_get_initial_bearing(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLng a, b;
        nifpp::get_throws(env, argv[0], a);
        nifpp::get_throws(env, argv[1], b);
        return nifpp::make(env, S2Earth::GetInitialBearing(a, b));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_get_distance_s2point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Point a = nifpp::get<S2Point>(env, nifpp::TERM(argv[0]));
        S2Point b = nifpp::get<S2Point>(env, nifpp::TERM(argv[1]));

        return nifpp::make(env, S2Earth::GetDistance(a, b).value());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_get_distance_s2latlng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLng a, b;
        nifpp::get_throws(env, argv[0], a);
        nifpp::get_throws(env, argv[1], b);
        return nifpp::make(env, S2Earth::GetDistance(a, b).value());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2earth_get_distance_km_s2point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Point a = nifpp::get<S2Point>(env, nifpp::TERM(argv[0]));
        S2Point b = nifpp::get<S2Point>(env, nifpp::TERM(argv[1]));

        return nifpp::make(env, S2Earth::GetDistanceKm(a, b));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_get_distance_km_s2latlng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLng a, b;
        nifpp::get_throws(env, argv[0], a);
        nifpp::get_throws(env, argv[1], b);
        return nifpp::make(env, S2Earth::GetDistanceKm(a, b));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_get_distance_meters_s2point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Point a = nifpp::get<S2Point>(env, nifpp::TERM(argv[0]));
        S2Point b = nifpp::get<S2Point>(env, nifpp::TERM(argv[1]));

        return nifpp::make(env, S2Earth::GetDistanceMeters(a, b));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_get_distance_meters_s2latlng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLng a, b;
        nifpp::get_throws(env, argv[0], a);
        nifpp::get_throws(env, argv[1], b);
        return nifpp::make(env, S2Earth::GetDistanceMeters(a, b));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2earth_radius_km(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return nifpp::make(env, S2Earth::RadiusKm());
}

ERL_NIF_TERM s2earth_radius_meters(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return nifpp::make(env, S2Earth::RadiusMeters());
}


ERL_NIF_TERM s2earth_lowest_altitude_km(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return nifpp::make(env, S2Earth::LowestAltitudeKm());
}

ERL_NIF_TERM s2earth_lowest_altitude_meters(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return nifpp::make(env, S2Earth::LowestAltitudeMeters());
}

ERL_NIF_TERM s2earth_highest_altitude_km(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return nifpp::make(env, S2Earth::HighestAltitudeKm());
}

ERL_NIF_TERM s2earth_highest_altitude_meters(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return nifpp::make(env, S2Earth::HighestAltitudeMeters());
}

