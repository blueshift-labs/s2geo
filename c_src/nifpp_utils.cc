#include "s2geo_nif.h"
#include "nifpp_utils.h"
#include <string.h>
#include<functional>

using std::make_tuple;
using std::ref;

double get_double_from_term(ErlNifEnv *env, ERL_NIF_TERM term)
{
    try
    {
    return nifpp::get<double>(env, nifpp::TERM(term));
    }
    catch(nifpp::badarg) {
        return (double) nifpp::get<int>(env, nifpp::TERM(term));
    }
}

int nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, S2LatLng &var)
{
    double lat;
    double lng;

    int arity;
    const ERL_NIF_TERM *array;
    if(!enif_get_tuple(env, term, &arity, &array)) return 0;
    if (arity == 2){
        ERL_NIF_TERM first = *array;
        ERL_NIF_TERM second = *(array+1);
        lat = get_double_from_term(env, first);
        lng = get_double_from_term(env, second);
        var = S2LatLng::FromDegrees(lat, lng).Normalized();
        return 1;
    }
    return 0;
}

nifpp::TERM nifpp::make(ErlNifEnv *env, const Vector2_d &var)
{
    double x = var.x();
    double y = var.y();
    return nifpp::make(env, std::make_tuple(x, y));
}

nifpp::TERM nifpp::make(ErlNifEnv *env, const S2LatLng &var){
    auto pt = var.Normalized();
    double lat = pt.lat().degrees();
    double lng = pt.lng().degrees();
    return nifpp::make(env, std::make_tuple(lat, lng));
}

int nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, S1Angle &var){
    double degrees = nifpp::get<double>(env, nifpp::TERM(term));
    var = S1Angle::Degrees(degrees);
    return true;
}

nifpp::TERM nifpp::make(ErlNifEnv *env, const S1Angle &var){
    double degrees = var.degrees();
    return nifpp::make(env, degrees);
}

int nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, R1Interval &var){
    double p1 = 0.0;
    double p2 = 0.0;
    auto tuple = make_tuple(ref(p1), ref(p1));
    nifpp::get_throws(env, term, tuple);

    var = R1Interval::FromPointPair(p1, p2);
    return true;
}

nifpp::TERM nifpp::make(ErlNifEnv *env, const R1Interval &var){
    return nifpp::make(env, std::make_tuple(var.lo(), var.hi()));
}

int nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, S1Interval &var){
    double p1 = 0.0;
    double p2 = 0.0;
    auto tuple = make_tuple(ref(p1), ref(p1));
    nifpp::get_throws(env, term, tuple);

    var = S1Interval::FromPointPair(p1, p2);
    return true;
}

nifpp::TERM nifpp::make(ErlNifEnv *env, const S1Interval &var){
    return nifpp::make(env, std::make_tuple(var.lo(), var.hi()));
}

int nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, S2Point &var)
{
    double x;
    double y;
    double z;
    auto tuple = make_tuple(ref(x), ref(y), ref(z));
    nifpp::get_throws(env, term, tuple);
    var = S2Point(x, y, z);
    return true;
}

nifpp::TERM nifpp::make(ErlNifEnv *env, const S2Point &var)
{
    return nifpp::make(env, std::make_tuple(var.x(), var.y(), var.z()));
}

// int nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, R2Point &var)
// {
//     double x;
//     double y;
//     auto tuple = make_tuple(ref(x), ref(y));
//     nifpp::get_throws(env, term, tuple);
//     var = R2Point(x, y);
//     return true;
// }

// nifpp::TERM nifpp::make(ErlNifEnv *env, const R2Point &var)
// {
//     return nifpp::make(env, std::make_tuple(var.x(), var.y()));
// }

int nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, R2Rect &var)
{
    double lo_x = 0.0;
    double lo_y = 0.0;
    auto tuple = make_tuple(ref(lo_x), ref(lo_y));
    nifpp::get_throws(env, term, tuple);
    auto r1 = R1Interval::FromPointPair(lo_x, lo_y);

    double hi_x = 0.0;
    double hi_y = 0.0;
    auto tuple2 = make_tuple(ref(hi_x), ref(hi_y));
    nifpp::get_throws(env, term, tuple2);
    auto r2 = R1Interval::FromPointPair(hi_x, hi_y);

    var = R2Rect(r1, r2);
    return true;
}

nifpp::TERM nifpp::make(ErlNifEnv *env, const R2Rect &var)
{
    auto tuple_1 = nifpp::make(env, std::make_tuple(var.x().lo(), var.x().hi()));
    auto tuple_2 = nifpp::make(env, std::make_tuple(var.y().lo(), var.y().hi()));

    return nifpp::make(env, std::make_tuple(tuple_1, tuple_2));
}


int nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, S2CellId &var)
{
    auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(term));
    var = S2CellId(static_cast<uint64>(value));
    return true;
}

nifpp::TERM nifpp::make(ErlNifEnv *env, const S2CellId &var)
{
    uint64 id = var.id();
    return nifpp::make(env, static_cast<ErlNifUInt64>(id));
}

int nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, S1ChordAngle &var)
{
    double length2 = nifpp::get<double>(env, nifpp::TERM(term));
    var = S1ChordAngle::FromLength2(length2);
    return true;
}

nifpp::TERM nifpp::make(ErlNifEnv *env, const S1ChordAngle &var)
{
    double length2 = var.length2();
    return nifpp::make(env, length2);
}
