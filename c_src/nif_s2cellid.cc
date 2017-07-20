#include "s2latlng.h"
#include "s2cellid.h"

#include "s2geo_nif.h"
#include "nif_s2cellid.h"
#include "nif_utils.h"

uint64 get_unit64(ErlNifEnv* env, ERL_NIF_TERM term)
{
    ErlNifUInt64 value;
    if(enif_get_uint64(env, term, &value))
    {
        return static_cast<uint64>(value);
    }
    return 0;
}

ERL_NIF_TERM uint64_to_erlang_term(ErlNifEnv* env, uint64 value)
{
    return enif_make_int64(env, value);
}


ERL_NIF_TERM s2cellid_new_from_lat_long_degrees(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 2)
        return enif_make_badarg(env);

    double lat_degrees;
    double lng_degrees;

    if(!enif_get_double(env, argv[0], &lat_degrees))
        return make_bad_options(env, argv[0]);

    if(!enif_get_double(env, argv[1], &lng_degrees))
        return make_bad_options(env, argv[1]);

    S2LatLng latLng = S2LatLng::FromDegrees(lat_degrees, lng_degrees);
    S2CellId cellId = S2CellId::FromLatLng(latLng);
    uint64 id = cellId.id();
    return uint64_to_erlang_term(env, id);
}

