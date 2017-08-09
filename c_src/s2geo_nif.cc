#include "s2geo_nif.h"
#include "constants.h"
#include "nif_s1interval.h"
#include "nif_s2cellid.h"
#include "nif_s2cellunion.h"
#include "nif_s2latlngrect.h"
#include "nif_s2loop.h"

#include "nifpp_utils.h"

#include "s2cell.h"
#include "s2cellunion.h"
#include "s2latlngrect.h"
#include "s2loop.h"

atoms ATOMS;

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;

    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1))
        return ret;

    return enif_make_atom(env, name);
}

static int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ATOMS.atomOk = make_atom(env, s2geo::kAtomOk);
    ATOMS.atomError = make_atom(env, s2geo::kAtomError);
    ATOMS.atomTrue = make_atom(env, s2geo::kAtomTrue);
    ATOMS.atomFalse = make_atom(env, s2geo::kAtomFalse);
    ATOMS.atomNull = make_atom(env, s2geo::kAtomNull);
    ATOMS.atomBadArg = make_atom(env, s2geo::kAtomBadArg);
    ATOMS.atomOptions = make_atom(env, s2geo::kAtomOptions);
    ATOMS.atomNotImplemented = make_atom(env, s2geo::kAtomNotImplemented);
    ATOMS.atomInternalError = make_atom(env, s2geo::kAtomInternalError);

    nifpp::register_resource<S2Cell>(env, nullptr, "S2Cell");
    nifpp::register_resource<S2LatLng>(env, nullptr, "S2LatLng");
    nifpp::register_resource<S2LatLngRect>(env, nullptr, "S2LatLngRect");
    nifpp::register_resource<S2CellUnion>(env, nullptr, "S2CellUnion");
    nifpp::register_resource<S2Loop>(env, nullptr, "S2Loop");

    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"s1interval_constructor", 1, s1interval_constructor},
    {"s1interval_constructor", 2, s1interval_constructor},
    {"s1interval_constructor", 3, s1interval_constructor},
    {"s1interval_methods",     2, s1interval_methods},
    {"s1interval_methods",     3, s1interval_methods},
    {"s1interval_methods",     4, s1interval_methods},

    {"s2cellid_get_size_ij",   1, s2cellid_get_size_ij},
    {"s2cellid_get_size_st",   1, s2cellid_get_size_st},
    {"s2cellid_lsb_for_level", 1, s2cellid_lsb_for_level},
    {"s2cellid_to_face_ij_orientation", 1, s2cellid_to_face_ij_orientation},

    {"s2cellid_constructor", 1, s2cellid_constructor},
    {"s2cellid_constructor", 2, s2cellid_constructor},
    {"s2cellid_constructor", 3, s2cellid_constructor},
    {"s2cellid_constructor", 4, s2cellid_constructor},

    {"s2cellid_zero_args_fn", 2, s2cellid_zero_args_fn},
    {"s2cellid_one_arg_fn",   3, s2cellid_one_arg_fn},

    {"s2latlngrect_constructor",  1, s2latlngrect_constructor},
    {"s2latlngrect_constructor",  2, s2latlngrect_constructor},
    {"s2latlngrect_constructor",  3, s2latlngrect_constructor},
    {"s2latlngrect_methods",      2, s2latlngrect_methods},
    {"s2latlngrect_methods",      3, s2latlngrect_methods},

    {"s2latlngrect_get_covering", 3, s2region_coverer_get_covering_for_s2latlngrect},
    {"s2latlngrect_get_covering", 4, s2region_coverer_get_covering_for_s2latlngrect},

    {"s2cellunion_constructor",  2, s2cellunion_constructor},
    {"s2cellunion_constructor",  3, s2cellunion_constructor},
    {"s2cellunion_methods",      2, s2cellunion_methods},
    {"s2cellunion_methods",      3, s2cellunion_methods},
    {"s2cellunion_methods",      4, s2cellunion_methods},
    {"s2cellunion_get_covering", 3, s2region_coverer_get_covering_for_s2cellunion},
    {"s2cellunion_get_covering", 4, s2region_coverer_get_covering_for_s2cellunion},

    {"s2loop_constructor",       2, s2loop_constructor},
    {"s2loop_methods",           2, s2loop_methods},
    {"s2loop_methods",           3, s2loop_methods},
    {"s2loop_methods",           4, s2loop_methods},
};

ERL_NIF_INIT(s2geo_nif, nif_funcs, on_nif_load, NULL, NULL, NULL);
