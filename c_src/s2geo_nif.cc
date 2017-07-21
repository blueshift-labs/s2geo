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
    {"s2cellid_get_size_ij", 1, s2cellid_get_size_ij},
    {"s2cellid_get_size_st", 1, s2cellid_get_size_st},
    {"s2cellid_lsb_for_level", 1, s2cellid_lsb_for_level},

    {"s2cellid_constructor", 1, s2cellid_constructor},
    {"s2cellid_constructor", 2, s2cellid_constructor},
    {"s2cellid_constructor", 3, s2cellid_constructor},
    {"s2cellid_constructor", 4, s2cellid_constructor},

    {"s2cellid_zero_args_fn", 2, s2cellid_zero_args_fn},
    {"s2cellid_one_arg_fn", 3, s2cellid_one_arg_fn}
};

ERL_NIF_INIT(s2geo_nif, nif_funcs, on_nif_load, NULL, NULL, NULL);
