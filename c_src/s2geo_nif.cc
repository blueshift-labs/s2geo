#include "s2geo_nif.h"
#include "constants.h"
#include "nif_s2cellid.h"
#include "nif_utils.h"

atoms ATOMS;

int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ATOMS.atomOk = make_atom(env, s2geo::kAtomOk);
    ATOMS.atomError = make_atom(env, s2geo::kAtomError);
    ATOMS.atomTrue = make_atom(env, s2geo::kAtomTrue);
    ATOMS.atomFalse = make_atom(env, s2geo::kAtomFalse);
    ATOMS.atomNull = make_atom(env, s2geo::kAtomNull);
    ATOMS.atomBadArg = make_atom(env, s2geo::kAtomBadArg);
    ATOMS.atomOptions = make_atom(env, s2geo::kAtomOptions);

    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"s2cellid_new_from_lat_long_degrees", 2, s2cellid_new_from_lat_long_degrees}
};

ERL_NIF_INIT(s2geo_nif, nif_funcs, on_nif_load, NULL, NULL, NULL);
