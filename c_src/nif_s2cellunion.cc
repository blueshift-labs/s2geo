#include "s2/s2cell.h"
#include "s2/s2cap.h"
#include "s2/s2cell_union.h"
#include "s2/s2latlng_rect.h"
#include "s2/s2region_coverer.h"
#include "s2/util/coding/coder.h"

#include "s2geo_nif.h"
#include "nifpp_utils.h"
#include "nif_s2cellunion.h"

#include<functional>

using std::make_tuple;
using std::ref;

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)


ERL_NIF_TERM s2cellunion_new_from_cellids(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);

        std::vector<S2CellId> cellids;
        nifpp::get_throws(env, argv[0], cellids);

        nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>(cellids);
        S2CellUnion *cell_union = s2cellunion_ptr.get();
        cell_union->Normalize();

        return nifpp::make(env, s2cellunion_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_new_from_normalized_cellids(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);

        std::vector<S2CellId> cellids;
        nifpp::get_throws(env, argv[0], cellids);

        nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
        S2CellUnion *cell_union = s2cellunion_ptr.get();
        *cell_union  = S2CellUnion::FromNormalized(cellids);
        return nifpp::make(env, s2cellunion_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_new_from_min_max(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        auto min_id = nifpp::get<S2CellId>(env, argv[0]);
        auto max_id = nifpp::get<S2CellId>(env, argv[1]);

        nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>(S2CellUnion::FromMinMax(min_id, max_id));
        return nifpp::make(env, s2cellunion_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_new_from_begin_end(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        auto begin = nifpp::get<S2CellId>(env, argv[0]);
        auto end = nifpp::get<S2CellId>(env, argv[1]);

        nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>(S2CellUnion::FromBeginEnd(begin, end));
        return nifpp::make(env, s2cellunion_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_num_cells(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->num_cells());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_cell_id(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);
        auto i = nifpp::get<int>(env, argv[1]);
        return nifpp::make(env, self->cell_id(i));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_cell_ids(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->cell_ids());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellunion_is_valid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->IsValid());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_is_normalized(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->IsNormalized());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_normalize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>(*self);
        S2CellUnion *cell_union = s2cellunion_ptr.get();
        cell_union->Normalize();
        return nifpp::make(env, s2cellunion_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellunion_denormalize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 3);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);
        int min_level = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[1]));
        int max_level = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[2]));

        std::vector<S2CellId> output;
        self->Denormalize(min_level, max_level, &output);

        return nifpp::make(env, output);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellunion_pack(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);
        auto level = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[1]));

        auto cloned = self->Clone();
        cloned->Pack(level);

        nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>(*cloned);
        return nifpp::make(env, s2cellunion_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellunion_contains_s2cellid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        auto cellid = nifpp::get<S2CellId>(env, argv[1]);
        return nifpp::make(env, self->Contains(cellid));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_intersects_s2cellid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        auto cellid = nifpp::get<S2CellId>(env, argv[1]);
        return nifpp::make(env, self->Intersects(cellid));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_contains_s2cellunion(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        S2CellUnion *other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self->Contains(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_intersects_s2cellunion(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        S2CellUnion *other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self->Intersects(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_union(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        S2CellUnion *other;
        nifpp::get_throws(env, argv[1], other);

        nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>(self->Union(*other));
        return nifpp::make(env, s2cellunion_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_intersection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        S2CellUnion *other;
        nifpp::get_throws(env, argv[1], other);

        nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>(self->Intersection(*other));
        return nifpp::make(env, s2cellunion_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_intersection_with_s2cellid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        auto cellid = nifpp::get<S2CellId>(env, argv[1]);

        nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>(self->Intersection(cellid));
        return nifpp::make(env, s2cellunion_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_difference(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        S2CellUnion *other;
        nifpp::get_throws(env, argv[1], other);

        nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>(self->Difference(*other));
        return nifpp::make(env, s2cellunion_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_expand_level(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        auto expand_level = nifpp::get<int>(env, argv[1]);

        auto cloned = self->Clone();
        cloned->Expand(expand_level);
        nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>(*cloned);
        return nifpp::make(env, s2cellunion_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_expand_radius(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 3);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        auto min_radius = nifpp::get<S1Angle>(env, argv[1]);
        auto max_level_diff = nifpp::get<int>(env, argv[2]);

        auto cloned = self->Clone();
        cloned->Expand(min_radius, max_level_diff);
        nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>(*cloned);
        return nifpp::make(env, s2cellunion_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_leaf_cells_covered(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, static_cast<ErlNifUInt64>( self->LeafCellsCovered() ));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_average_based_area(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->AverageBasedArea());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_approx_area(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->ApproxArea());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_exact_area(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->ExactArea());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_get_cap_bound(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        auto cap = self->GetCapBound();
        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2cellunion_get_rect_bound(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLngRect constructed = self->GetRectBound();

        nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
        return nifpp::make(env, s2rect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellunion_equal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        S2CellUnion *other;
        nifpp::get_throws(env, argv[1], other);

        return nifpp::make(env, *self == *other);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_not_equal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        S2CellUnion *other;
        nifpp::get_throws(env, argv[1], other);

        return nifpp::make(env, *self != *other);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_contains_s2cell(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        S2Cell* cell;
        nifpp::get_throws(env, argv[1], cell);

        return nifpp::make(env, self->Contains(*cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_may_intersect_s2cellid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);
        S2CellId cellid = nifpp::get<S2CellId>(env, argv[1]);

        S2Cell cell(cellid);

        return nifpp::make(env, self->MayIntersect(cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_may_intersect_s2cell(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        S2Cell* cell;
        nifpp::get_throws(env, argv[1], cell);

        return nifpp::make(env, self->MayIntersect(*cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_contains_s2point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        auto s2point = nifpp::get<S2Point>(env, argv[1]);
        return nifpp::make(env, self->Contains(s2point));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        Encoder encoder;
        self->Encode(&encoder);
        std::string encoded(encoder.base(), encoder.length());
        return nifpp::make(env, encoded);

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cellunion_decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        std::string encoded;
        nifpp::get_throws(env, argv[0], encoded);

        Decoder decoder(encoded.c_str(), encoded.length());
        nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
        S2CellUnion *decoded = s2cellunion_ptr.get();
        decoded->Decode(&decoder);
        decoded->Normalize();

        return nifpp::make(env, s2cellunion_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



// ERL_NIF_TERM s2region_coverer_get_covering_for_s2cellunion(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
//     if(argc < 3) // Need atleast one arg
//         return enif_make_badarg(env);
//     try{
//         S2CellUnion *self;
//         nifpp::get_throws(env, argv[0], self);
//         auto covering_type = static_cast<S2RegionCoveringType>( nifpp::get<int>(env, nifpp::TERM(argv[1])) );
//         S2RegionCoverer coverer;

//         auto max_cells = nifpp::get<int>(env, argv[2]);
//         if(max_cells > 0)
//         {
//             coverer.set_max_cells(max_cells);
//         }


//         if (argc == 4)
//         {
//             int min_level;
//             int max_level;
//             auto levels = make_tuple(ref(min_level), ref(max_level));
//             nifpp::get(env, argv[3], levels);
//             coverer.set_min_level(min_level);
//             coverer.set_max_level(max_level);
//         }

//         switch( covering_type ) {
//             case S2RegionCoveringType::cellid_covering:
//             {
//                 vector<S2CellId> covering;
//                 coverer.GetCovering(*self, &covering);
//                 return nifpp::make(env, covering);
//             }

//             case S2RegionCoveringType::cellid_interior_covering:
//             {
//                 vector<S2CellId> covering;
//                 coverer.GetInteriorCovering(*self, &covering);
//                 return nifpp::make(env, covering);
//             }
//             case S2RegionCoveringType::cell_union_covering:
//             {
//                 nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
//                 S2CellUnion *covering = s2cellunion_ptr.get();

//                 coverer.GetCellUnion(*self, covering);
//                 return nifpp::make(env, s2cellunion_ptr);
//             }
//             case S2RegionCoveringType::interior_cell_union_covering:
//             {
//                 nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
//                 S2CellUnion *covering = s2cellunion_ptr.get();

//                 coverer.GetInteriorCellUnion(*self, covering);
//                 return nifpp::make(env, s2cellunion_ptr);
//             }

//             default:
//                 return ATOMS.atomNotImplemented;
//         }
//     }
//     catch(nifpp::badarg) {}
//     catch(...){ return ATOMS.atomInternalError;}

//     return enif_make_badarg(env);
// }

