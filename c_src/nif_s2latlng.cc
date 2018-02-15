#include "s2latlng.h"

#include "nifpp_utils.h"
#include "s2geo_nif.h"
#include "nif_s2latlng.h"

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)


ERL_NIF_TERM s2latlng_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc < 1) // Need atleast one arg
        return enif_make_badarg(env);

    try{
        auto option = static_cast<S2LatLngConstructors>( nifpp::get<int>(env, nifpp::TERM(argv[0]) ) );
        switch( option ) {
            case S2LatLngConstructors::default_constructor:
            {
                CHECK_ARGS_LENGTH(env, argc, 1);
                return nifpp::make(env, S2LatLng());
            }

            case S2LatLngConstructors::from_s1angle_lat_lng:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S1Angle lat = nifpp::get<S1Angle>(env, nifpp::TERM(argv[1]));
                S1Angle lng = nifpp::get<S1Angle>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, S2LatLng(lat, lng));
            }

            case S2LatLngConstructors::from_point:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                S2Point p = nifpp::get<S2Point>(env, nifpp::TERM(argv[1]));
                return nifpp::make(env, S2LatLng(p));
            }

            case S2LatLngConstructors::invalid:
            {
                CHECK_ARGS_LENGTH(env, argc, 1);
                return nifpp::make(env, S2LatLng::Invalid());
            }

            case S2LatLngConstructors::from_radians:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                double p1 = get_double_from_term(env, argv[1]);
                double p2 = get_double_from_term(env, argv[2]);
                return nifpp::make(env, S2LatLng::FromRadians(p1, p2));
            }

            case S2LatLngConstructors::from_degrees:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                double p1 = get_double_from_term(env, argv[1]);
                double p2 = get_double_from_term(env, argv[2]);
                return nifpp::make(env, S2LatLng::FromDegrees(p1, p2));
            }

            case S2LatLngConstructors::from_e5:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                int lat_e5 = nifpp::get<int>(env, nifpp::TERM(argv[1]));
                int lng_e5 = nifpp::get<int>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, S2LatLng::FromE5(lat_e5, lng_e5));
            }

            case S2LatLngConstructors::from_e6:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                int lat_e6 = nifpp::get<int>(env, nifpp::TERM(argv[1]));
                int lng_e6 = nifpp::get<int>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, S2LatLng::FromE6(lat_e6, lng_e6));
            }

            case S2LatLngConstructors::from_e7:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                int lat_e7 = nifpp::get<int>(env, nifpp::TERM(argv[1]));
                int lng_e7 = nifpp::get<int>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, S2LatLng::FromE7(lat_e7, lng_e7));
            }

            case S2LatLngConstructors::from_unsigned_e6:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto lat_e6 = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[1]));
                auto lng_e6 = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, S2LatLng::FromUnsignedE6(lat_e6, lng_e6));
            }


            case S2LatLngConstructors::from_unsigned_e7:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto lat_e7 = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[1]));
                auto lng_e7 = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, S2LatLng::FromUnsignedE6(lat_e7, lng_e7));
            }

            case S2LatLngConstructors::latitude:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Point p = nifpp::get<S2Point>(env, nifpp::TERM(argv[1]));
                return nifpp::make(env, S2LatLng::Latitude(p));
            }

            case S2LatLngConstructors::longitude:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Point p = nifpp::get<S2Point>(env, nifpp::TERM(argv[1]));
                return nifpp::make(env, S2LatLng::Longitude(p));
            }

            default:
                return ATOMS.atomError;
        }

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}

    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlng_methods(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc < 2) // Need atleast one arg
        return enif_make_badarg(env);
    try{
        S2LatLng self;
        nifpp::get_throws(env, argv[0], self);

        auto option = static_cast<S2LatLngMethods>( nifpp::get<int>(env, nifpp::TERM(argv[1])) );
        switch( option ) {
            case S2LatLngMethods::lat:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.lat());
            }

            case S2LatLngMethods::lng:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.lng());
            }

            case S2LatLngMethods::coords:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.coords());
            }

            case S2LatLngMethods::is_valid:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.is_valid());
            }

            case S2LatLngMethods::normalized:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.Normalized());
            }

            case S2LatLngMethods::to_point:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.ToPoint());
            }

            case S2LatLngMethods::get_distance:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLng other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self.GetDistance(other));
            }

            case S2LatLngMethods::add:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLng other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self + other);
            }

            case S2LatLngMethods::subtract:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLng other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self - other);
            }

            case S2LatLngMethods::multiply:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                double m = get_double_from_term(env, argv[2]);
                return nifpp::make(env, self * m);
            }

            case S2LatLngMethods::eq:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLng other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self == other);
            }


            case S2LatLngMethods::neq:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLng other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self != other);
            }


            case S2LatLngMethods::lt:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLng other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self < other);
            }


            case S2LatLngMethods::gt:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLng other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self > other);
            }

            case S2LatLngMethods::leq:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLng other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self <= other);
            }

            case S2LatLngMethods::geq:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLng other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self >= other);
            }

            case S2LatLngMethods::approx_equals:
            {
                CHECK_ARGS_LENGTH(env, argc, 4);
                S2LatLng other;
                nifpp::get_throws(env, argv[2], other);
                double max_error = get_double_from_term(env, argv[3]);
                return nifpp::make(env, self.ApproxEquals(other, max_error));
            }

            case S2LatLngMethods::to_string_in_degrees:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.ToStringInDegrees());
            }

            default:
                return ATOMS.atomNotImplemented;
        }
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}

    return enif_make_badarg(env);
}
