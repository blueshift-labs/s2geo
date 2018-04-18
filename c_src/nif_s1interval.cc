#include "s2geo_nif.h"
#include "nifpp_utils.h"
#include "nif_s1interval.h"

using std::make_tuple;

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)


ERL_NIF_TERM s1interval_default_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return nifpp::make(env, S1Interval());
}

ERL_NIF_TERM s1interval_empty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return nifpp::make(env, S1Interval::Empty());
}

ERL_NIF_TERM s1interval_full(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return nifpp::make(env, S1Interval::Full());
}

ERL_NIF_TERM s1interval_from_hi_lo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        double lo = get_double_from_term(env, argv[0]);
        double hi = get_double_from_term(env, argv[1]);
        return nifpp::make(env, S1Interval(lo, hi));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_from_point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        double p = get_double_from_term(env, argv[0]);
        return nifpp::make(env, S1Interval::FromPoint(p));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_from_point_pair(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 0);
        double p1 = get_double_from_term(env, argv[0]);
        double p2 = get_double_from_term(env, argv[1]);
        return nifpp::make(env, S1Interval::FromPointPair(p1, p2));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_hi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        return nifpp::make(env, self.hi());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_lo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.lo());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_bounds(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.bounds());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_bound(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        int index = nifpp::get<int>(env, argv[1]);
        return nifpp::make(env, self[index]);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_is_valid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.is_valid());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_is_full(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.is_full());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_is_empty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.is_empty());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_is_inverted(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.is_inverted());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_get_center(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.GetCenter());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_get_length(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.GetLength());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_complement(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.Complement());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_get_complement_center(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self.GetComplementCenter());

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_interior_contains_double(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        double p = get_double_from_term(env, argv[1]);
        return nifpp::make(env, self.Contains(p));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_contains_double(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        double p = get_double_from_term(env, argv[1]);
        return nifpp::make(env, self.InteriorContains(p));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_contains(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, self.Contains(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_interior_contains(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, self.InteriorContains(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s1interval_intersects(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, self.Intersects(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s1interval_interior_intersects(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, self.InteriorIntersects(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s1interval_get_directed_hausdorff_distance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, self.GetDirectedHausdorffDistance(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_add_point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);
        double p = get_double_from_term(env, argv[1]);
        self.AddPoint(p);
        return nifpp::make(env, self);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_project(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);
        double p = get_double_from_term(env, argv[1]);
        return nifpp::make(env, self.Project(p));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_expanded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        double p = get_double_from_term(env, argv[1]);
        self.Expanded(p);
        return nifpp::make(env, self);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_union(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, self.Union(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_intersection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, self.Intersection(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s1interval_equals(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 3);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, (self == other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s1interval_approx_equals(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 3);
        S1Interval self;
        nifpp::get_throws(env, argv[0], self);

        S1Interval other = nifpp::get<S1Interval>(env, nifpp::TERM(argv[1]));
        double max_error = get_double_from_term(env, argv[2]);
        return nifpp::make(env, self.ApproxEquals(other, max_error));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}
