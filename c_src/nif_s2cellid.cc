#include "s2latlng.h"
#include "s2cellid.h"

#include "nifpp_utils.h"
#include "s2geo_nif.h"
#include "nif_s2cellid.h"

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)

ERL_NIF_TERM s2cellid_get_size_ij(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGS_LENGTH(env, argc, 1);
    try{
        int level = nifpp::get<int>(env, nifpp::TERM(argv[0]));
        return nifpp::make(env, S2CellId::GetSizeIJ(level));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_get_size_st(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGS_LENGTH(env, argc, 1);
    try{
        int level = nifpp::get<int>(env, nifpp::TERM(argv[0]));
        return nifpp::make(env, S2CellId::GetSizeST(level));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_lsb_for_level(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGS_LENGTH(env, argc, 1);
    try{
        int level = nifpp::get<int>(env, nifpp::TERM(argv[0]));
        return nifpp::make(env, static_cast<ErlNifUInt64>( S2CellId::lsb_for_level(level) ));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_to_face_ij_orientation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGS_LENGTH(env, argc, 1);
    try{
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));

        int pi, pj, orientation;
        int face = s2cellid.ToFaceIJOrientation(&pi, &pj,&orientation);
        return nifpp::make(env, std::make_tuple(face, pi, pj, orientation));

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc < 1) // Need atleast one arg
        return enif_make_badarg(env);

    try{
        auto option = static_cast<S2CellIdConstructors>( nifpp::get<int>(env, nifpp::TERM(argv[0]) ) );
        switch( option ) {
            case S2CellIdConstructors::none:
            {
                CHECK_ARGS_LENGTH(env, argc, 1);
                return nifpp::make(env, S2CellId::None());
            }

            case S2CellIdConstructors::sentinel:
            {
                CHECK_ARGS_LENGTH(env, argc, 1);
                return nifpp::make(env, S2CellId::Sentinel());
            }

            case S2CellIdConstructors::from_face_pos_level:
            {
                CHECK_ARGS_LENGTH(env, argc, 4);

                int face = nifpp::get<int>(env, nifpp::TERM(argv[1]));
                ErlNifUInt64 pos = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[2]));
                int level = nifpp::get<int>(env, nifpp::TERM(argv[3]));

                S2CellId cellId  = S2CellId::FromFacePosLevel(face, static_cast<uint64>(pos), level);
                return nifpp::make(env, cellId);
            }

            case S2CellIdConstructors::from_lat_lng:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                auto latLng = nifpp::get<S2LatLng>(env, argv[1]);
                S2CellId cellId = S2CellId::FromLatLng(latLng);
                return nifpp::make(env, cellId);
            }

            case S2CellIdConstructors::begin:
                {
                CHECK_ARGS_LENGTH(env, argc, 2);
                int level = nifpp::get<int>(env, nifpp::TERM(argv[1]));
                return nifpp::make(env, S2CellId::Begin(level));
                }

            case S2CellIdConstructors::end:
                {
                CHECK_ARGS_LENGTH(env, argc, 2);
                int level = nifpp::get<int>(env, nifpp::TERM(argv[1]));
                return nifpp::make(env, S2CellId::End(level));
                }

            case S2CellIdConstructors::from_token:
                {
                CHECK_ARGS_LENGTH(env, argc, 2);
                string token = nifpp::get<string>(env, nifpp::TERM(argv[1]));
                return nifpp::make(env, S2CellId::FromToken(token));
                }

            case S2CellIdConstructors::from_face_ij:
                {
                CHECK_ARGS_LENGTH(env, argc, 4);
                int face = nifpp::get<int>(env, nifpp::TERM(argv[1]));
                int i    = nifpp::get<int>(env, nifpp::TERM(argv[2]));
                int j    = nifpp::get<int>(env, nifpp::TERM(argv[3]));

                S2CellId cellId = S2CellId::FromFaceIJ(face, i, j);
                return nifpp::make(env, cellId);
                }

            case S2CellIdConstructors::from_point:
                return ATOMS.atomError;

            default:
                return ATOMS.atomError;
        }

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}

    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_zero_args_fn(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGS_LENGTH(env, argc, 2);
    try{
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));


        auto option = static_cast<S2CellIdFunctionZeroArgs>(nifpp::get<int>(env, nifpp::TERM(argv[1])));

        switch( option ) {
            case S2CellIdFunctionZeroArgs::is_valid:
                if(s2cellid.is_valid()){
                    return ATOMS.atomTrue;
                }
                return ATOMS.atomFalse;

            case S2CellIdFunctionZeroArgs::to_lat_lng:
                return nifpp::make(env, s2cellid.ToLatLng());

            case S2CellIdFunctionZeroArgs::face:
                return nifpp::make(env, s2cellid.face());

            case S2CellIdFunctionZeroArgs::pos:
                return nifpp::make(env, static_cast<ErlNifUInt64>(s2cellid.pos()));

            case S2CellIdFunctionZeroArgs::level:
                return nifpp::make(env, s2cellid.level());

            case S2CellIdFunctionZeroArgs::get_size_ij:
                return nifpp::make(env, s2cellid.GetSizeIJ());

            case S2CellIdFunctionZeroArgs::get_size_st:
                return nifpp::make(env, s2cellid.GetSizeST());

            case S2CellIdFunctionZeroArgs::is_leaf:
                return nifpp::make(env, s2cellid.is_leaf());

            case S2CellIdFunctionZeroArgs::is_face:
                return nifpp::make(env, s2cellid.is_face());

            case S2CellIdFunctionZeroArgs::range_min:
                return nifpp::make(env, s2cellid.range_min());

            case S2CellIdFunctionZeroArgs::range_max:
                return nifpp::make(env, s2cellid.range_max());

            case S2CellIdFunctionZeroArgs::parent:
                return nifpp::make(env, s2cellid.parent());

            case S2CellIdFunctionZeroArgs::child_begin:
                return nifpp::make(env, s2cellid.child_begin());

            case S2CellIdFunctionZeroArgs::child_end:
                return nifpp::make(env, s2cellid.child_end());

            case S2CellIdFunctionZeroArgs::next:
                return nifpp::make(env, s2cellid.next());

            case S2CellIdFunctionZeroArgs::prev:
                return nifpp::make(env, s2cellid.prev());

            case S2CellIdFunctionZeroArgs::next_wrap:
                return nifpp::make(env, s2cellid.next_wrap());

            case S2CellIdFunctionZeroArgs::prev_wrap:
                return nifpp::make(env, s2cellid.prev_wrap());

            case S2CellIdFunctionZeroArgs::to_token:
                return nifpp::make(env, s2cellid.ToToken());

            case S2CellIdFunctionZeroArgs::to_string:
                return nifpp::make(env, s2cellid.ToString());

            case S2CellIdFunctionZeroArgs::lsb:
                return nifpp::make(env, static_cast<ErlNifUInt64>(s2cellid.lsb()));

            default:
                break;
        }
        return ATOMS.atomError;
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}

    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_one_arg_fn(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGS_LENGTH(env, argc, 3);
    try{
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));

        auto option = static_cast<S2CellIdFunctionOneArg>(nifpp::get<int>(env, nifpp::TERM(argv[1])));
        switch( option ) {
            case S2CellIdFunctionOneArg::get_size_ij:
                {
                int level = nifpp::get<int>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, s2cellid.GetSizeIJ(level));
                }

            case S2CellIdFunctionOneArg::get_size_st:
                {
                int level = nifpp::get<int>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, s2cellid.GetSizeST(level));
                }

            case S2CellIdFunctionOneArg::child_position:
                {
                int level = nifpp::get<int>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, s2cellid.child_position(level));
                }

            case S2CellIdFunctionOneArg::contains:
                {
                auto other_value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[2]));
                auto other = S2CellId(static_cast<uint64>(other_value));
                return nifpp::make(env, s2cellid.contains(other));
                }

            case S2CellIdFunctionOneArg::intersects:
                {
                auto other_value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[2]));
                auto other = S2CellId(static_cast<uint64>(other_value));
                return nifpp::make(env, s2cellid.contains(other));
                }

            case S2CellIdFunctionOneArg::parent:
                {
                int level = nifpp::get<int>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, s2cellid.parent(level));
                }

            case S2CellIdFunctionOneArg::child:
                {
                int level = nifpp::get<int>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, s2cellid.child(level));
                }

            case S2CellIdFunctionOneArg::child_begin:
                {
                int level = nifpp::get<int>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, s2cellid.child_begin(level));
                }

            case S2CellIdFunctionOneArg::child_end:
                {
                int level = nifpp::get<int>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, s2cellid.child_end(level));
                }

            case S2CellIdFunctionOneArg::advance:
                {
                auto int64_level = nifpp::get<ErlNifSInt64>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, s2cellid.advance(static_cast<int64>(int64_level)));
                }

            case S2CellIdFunctionOneArg::advance_wrap:
                {
                auto int64_level = nifpp::get<ErlNifSInt64>(env, nifpp::TERM(argv[2]));
                return nifpp::make(env, s2cellid.advance_wrap(static_cast<int64>(int64_level)));
                }

            default:
                break;
        }
        return ATOMS.atomError;
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}

    return enif_make_badarg(env);
}
