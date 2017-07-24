#include "s2geo_nif.h"
#include "nifpp_utils.h"
#include <string.h>

int nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, S2LatLng &var){
    double lat;
    double lng;
    auto lat_lng_tuple = make_tuple(ref(lat), ref(lng));

    int res = nifpp::get(env, term, lat_lng_tuple);
    if(res){
        var = S2LatLng::FromDegrees(lat, lng).Normalized();
    }
    return res;
}

nifpp::TERM nifpp::make(ErlNifEnv *env, const S2LatLng &var){
    auto coords = var.coords();
    return nifpp::make(env, std::make_tuple(coords[0], coords[1]));
}

int nifpp::get(ErlNifEnv *env, ERL_NIF_TERM term, S1Angle &var){
    double radians = nifpp::get<double>(env, nifpp::TERM(term));
    var = S1Angle::Radians(radians);
    return true;
}

nifpp::TERM nifpp::make(ErlNifEnv *env, const S1Angle &var){
    double radians = var.radians();
    return nifpp::make(env, radians);
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
