#include "s2/s2latlng.h"
#include "s2/s2cell_id.h"

#include "nifpp_utils.h"
#include "s2geo_nif.h"
#include "nif_s2cellid.h"

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)


ERL_NIF_TERM s2cellid_from_s2point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto point = nifpp::get<S2Point>(env, argv[0]);
        S2CellId cellId = S2CellId::S2CellId(point);
        return nifpp::make(env, cellId);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_from_lat_lng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto latLng = nifpp::get<S2LatLng>(env, argv[0]);
        S2CellId cellId = S2CellId::S2CellId(latLng);
        return nifpp::make(env, cellId);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_none(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return nifpp::make(env, S2CellId::None());
}


ERL_NIF_TERM s2cellid_sentinel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return nifpp::make(env, S2CellId::Sentinel());
}


ERL_NIF_TERM s2cellid_from_face(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        int face = nifpp::get<int>(env, nifpp::TERM(argv[0]));
        S2CellId cellId  = S2CellId::FromFace(face);
        return nifpp::make(env, cellId);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_from_face_pos_level(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 3);
        int face = nifpp::get<int>(env, nifpp::TERM(argv[0]));
        ErlNifUInt64 pos = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[1]));
        int level = nifpp::get<int>(env, nifpp::TERM(argv[2]));

        S2CellId cellId  = S2CellId::FromFacePosLevel(face, static_cast<uint64>(pos), level);
        return nifpp::make(env, cellId);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_to_point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.ToPoint());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_to_point_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.ToPointRaw());

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_get_center_st(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.GetCenterST());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_get_size_st(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try{
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.GetSizeST());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_get_size_st_level(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

ERL_NIF_TERM s2cellid_get_bound_st(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGS_LENGTH(env, argc, 1);
    try{
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.GetBoundST());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_get_center_uv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.GetCenterUV());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_get_bound_uv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.GetBoundUV());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_expanded_by_distance_uv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
         auto uv = nifpp::get<R2Rect>(env, argv[0]);
         auto distance = nifpp::get<S1Angle>(env, argv[1]);
         return nifpp::make(env, S2CellId::ExpandedByDistanceUV(uv, distance));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_get_center_si_ti(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        int psi, pti;
        int face = s2cellid.GetCenterSiTi(&psi, &pti);
        return nifpp::make(env, std::make_tuple(face, psi, pti));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_to_lat_lng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.ToLatLng());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2cellid_is_valid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        if(s2cellid.is_valid()){
            return ATOMS.atomTrue;
        }
        return ATOMS.atomFalse;
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_face(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.face());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_pos(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, static_cast<ErlNifUInt64>(s2cellid.pos()));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_level(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.level());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_get_size_ij(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try{
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.GetSizeIJ());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_get_size_ij_level(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try{
        CHECK_ARGS_LENGTH(env, argc, 1);
        int level = nifpp::get<int>(env, nifpp::TERM(argv[0]));
        return nifpp::make(env, S2CellId::GetSizeIJ(level));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_is_leaf(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.is_leaf());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_is_face(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.is_face());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_child_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.child_position());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_range_min(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.range_min());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_range_max(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.range_max());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_contains(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));

        auto other_value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[1]));
        auto other = S2CellId(static_cast<uint64>(other_value));
        return nifpp::make(env, s2cellid.contains(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_intersects(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        auto other_value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[1]));
        auto other = S2CellId(static_cast<uint64>(other_value));
        return nifpp::make(env, s2cellid.intersects(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_parent(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        if(argc != 1 && argc != 2 )
            return enif_make_badarg(env);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));

        if(argc == 1)
            return nifpp::make(env, s2cellid.parent());
        int level = nifpp::get<int>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, s2cellid.parent(level));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_child(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        int level = nifpp::get<int>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, s2cellid.child(level));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_child_begin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        if(argc != 1 && argc != 2 )
            return enif_make_badarg(env);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));

        if(argc == 1)
            return nifpp::make(env, s2cellid.child_begin());
        int level = nifpp::get<int>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, s2cellid.child_begin(level));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_child_end(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        if(argc != 1 && argc != 2 )
            return enif_make_badarg(env);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));

        if(argc == 1)
            return nifpp::make(env, s2cellid.child_end());
        int level = nifpp::get<int>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, s2cellid.child_end(level));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.next());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_prev(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.prev());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_advance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));

        auto int64_level = nifpp::get<ErlNifSInt64>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, s2cellid.advance(static_cast<int64>(int64_level)));

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2cellid_distance_from_begin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, static_cast<ErlNifSInt64>(s2cellid.distance_from_begin()));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_next_wrap(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.next_wrap());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_prev_wrap(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.prev_wrap());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_advance_wrap(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        auto int64_level = nifpp::get<ErlNifSInt64>(env, nifpp::TERM(argv[1]));
        return nifpp::make(env, s2cellid.advance_wrap(static_cast<int64>(int64_level)));

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_maximum_tile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));

        auto limit_value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[1]));
        auto limit = S2CellId(static_cast<uint64>(limit_value));
        return nifpp::make(env, s2cellid.maximum_tile(limit));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_get_common_ancestor_level(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));

        auto other_value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[1]));
        auto other = S2CellId(static_cast<uint64>(other_value));
        return nifpp::make(env, s2cellid.GetCommonAncestorLevel(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_begin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        int level = nifpp::get<int>(env, nifpp::TERM(argv[0]));
        return nifpp::make(env, S2CellId::Begin(level));

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_end(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        int level = nifpp::get<int>(env, nifpp::TERM(argv[0]));
        return nifpp::make(env, S2CellId::End(level));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_to_token(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.ToToken());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_from_token(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        string token = nifpp::get<string>(env, nifpp::TERM(argv[0]));
        return nifpp::make(env, S2CellId::FromToken(token));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_to_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, s2cellid.ToString());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellid_get_edge_neighbors(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGS_LENGTH(env, argc, 1);
    try{
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));

        S2CellId neighbors[4];
        s2cellid.GetEdgeNeighbors(neighbors);
        std::vector<S2CellId> v(neighbors, neighbors + 3);
        return nifpp::make(env, v);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_get_vertex_neighbors(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGS_LENGTH(env, argc, 2);
    try{
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        int level = nifpp::get<int>(env, nifpp::TERM(argv[1]));

        std::vector<S2CellId> output;
        s2cellid.AppendVertexNeighbors(level, &output);

        return nifpp::make(env, output);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_get_all_neighbors(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CHECK_ARGS_LENGTH(env, argc, 2);
    try{
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        int nbr_level = nifpp::get<int>(env, nifpp::TERM(argv[1]));

        std::vector<S2CellId> output;
        s2cellid.AppendAllNeighbors(nbr_level, &output);

        return nifpp::make(env, output);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellid_from_face_ij(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 3);
        int face = nifpp::get<int>(env, nifpp::TERM(argv[0]));
        int i    = nifpp::get<int>(env, nifpp::TERM(argv[1]));
        int j    = nifpp::get<int>(env, nifpp::TERM(argv[2]));

        S2CellId cellId = S2CellId::FromFaceIJ(face, i, j);
        return nifpp::make(env, cellId);
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


ERL_NIF_TERM s2cellid_lsb(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[0]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        return nifpp::make(env, static_cast<ErlNifUInt64>(s2cellid.lsb()));
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


ERL_NIF_TERM s2cellid_ij_level_to_bound_uv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
