#include "s2geo_nif.h"
#include "nifpp_utils.h"
#include "nif_s1interval.h"

using std::make_tuple;
using std::ref;

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)


ERL_NIF_TERM s1interval_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc < 1) // Need atleast one arg
        return enif_make_badarg(env);

    try{
        auto option = static_cast<S1IntervalConstructors>( nifpp::get<int>(env, nifpp::TERM(argv[0]) ) );
        switch( option ) {
            case S1IntervalConstructors::default_constructor:
            {
                CHECK_ARGS_LENGTH(env, argc, 1);
                return nifpp::make(env, S1Interval());
            }

            case S1IntervalConstructors::empty:
            {
                CHECK_ARGS_LENGTH(env, argc, 1);
                return nifpp::make(env, S1Interval::Empty());
            }

            case S1IntervalConstructors::full:
            {
                CHECK_ARGS_LENGTH(env, argc, 1);
                return nifpp::make(env, S1Interval::Full());
            }

            case S1IntervalConstructors::from_point:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                double p = get_double_from_term(env, argv[1]);
                return nifpp::make(env, S1Interval::FromPoint(p));
            }

            case S1IntervalConstructors::from_hi_lo:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                double lo = get_double_from_term(env, argv[1]);
                double hi = get_double_from_term(env, argv[2]);
                return nifpp::make(env, S1Interval(lo, hi));
            }

            case S1IntervalConstructors::from_point_pair:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                double p1 = get_double_from_term(env, argv[1]);
                double p2 = get_double_from_term(env, argv[2]);
                return nifpp::make(env, S1Interval::FromPointPair(p1, p2));
            }

            default:
                return ATOMS.atomError;
        }

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}

    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_methods(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc < 2) // Need atleast one arg
        return enif_make_badarg(env);
    try{
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        auto option = static_cast<S1IntervalMethods>( nifpp::get<int>(env, nifpp::TERM(argv[1])) );
        switch( option ) {
            case S1IntervalMethods::hi:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.hi());
            }

            case S1IntervalMethods::lo:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.lo());
            }

            case S1IntervalMethods::bounds:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.bounds());
            }

            case S1IntervalMethods::bound:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                int index = nifpp::get<int>(env, argv[2]);
                return nifpp::make(env, self.bound(index));
            }

            case S1IntervalMethods::is_valid:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.is_valid());
            }

            case S1IntervalMethods::is_full:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.is_full());
            }

            case S1IntervalMethods::is_empty:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.is_empty());
            }

            case S1IntervalMethods::is_inverted:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.is_inverted());
            }

            case S1IntervalMethods::get_center:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.GetCenter());
            }


            case S1IntervalMethods::get_length:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.GetLength());
            }

            case S1IntervalMethods::complement:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.Complement());
            }

            case S1IntervalMethods::get_complement_center:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self.GetComplementCenter());
            }

            case S1IntervalMethods::interior_contains_double:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                double p = get_double_from_term(env, argv[2]);
                return nifpp::make(env, self.Contains(p));
            }

            case S1IntervalMethods::contains_double:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                double p = get_double_from_term(env, argv[2]);
                return nifpp::make(env, self.InteriorContains(p));
            }

            case S1IntervalMethods::contains:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, self.Contains(other));
            }

            case S1IntervalMethods::interior_contains:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, self.InteriorContains(other));
            }

            case S1IntervalMethods::get_directed_hausdorff_distance:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, self.GetDirectedHausdorffDistance(other));
            }

            case S1IntervalMethods::add_point:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                double p = get_double_from_term(env, argv[2]);
                self.AddPoint(p);
                return nifpp::make(env, self);
            }

            case S1IntervalMethods::expanded:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                double p = get_double_from_term(env, argv[2]);
                self.Expanded(p);
                return nifpp::make(env, self);
            }

            case S1IntervalMethods::Union:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, self.Union(other));
            }

            case S1IntervalMethods::intersection:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, self.Intersection(other));
            }

            case S1IntervalMethods::approx_equals:
            {
                CHECK_ARGS_LENGTH(env, argc, 4);
                S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[2]));
                double max_error = get_double_from_term(env, argv[3]);
                return nifpp::make(env, self.ApproxEquals(other, max_error));
            }

            default:
                return ATOMS.atomNotImplemented;
        }
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}

    return enif_make_badarg(env);
}

