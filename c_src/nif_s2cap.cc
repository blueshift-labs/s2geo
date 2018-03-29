#include "s2/s2cap.h"
#include "s2/s2latlng.h"
#include "s2/s2cell_id.h"
#include "s2/s2region_coverer.h"
#include "s2/s2cell_union.h"
#include "s2/s2latlng_rect.h"

#include "nifpp_utils.h"
#include "s2geo_nif.h"
#include "nif_s2cap.h"

#include<functional>

using std::make_tuple;
using std::ref;

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)

ERL_NIF_TERM s2cap_from_s2point_s1angle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Point center = nifpp::get<S2Point>(env, argv[0]);
        S1Angle radius = nifpp::get<S1Angle>(env, argv[1]);

        S2Cap cap = S2Cap(center, radius);
        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_from_s2point_s1chordangle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Point center = nifpp::get<S2Point>(env, argv[0]);
        S1ChordAngle radius = nifpp::get<S1ChordAngle>(env, argv[1]);

        S2Cap cap = S2Cap(center, radius);
        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_from_point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Point center = nifpp::get<S2Point>(env, argv[0]);

        S2Cap cap = S2Cap::FromPoint(center);
        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_from_center_height(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Point center = nifpp::get<S2Point>(env, argv[0]);
        double height = nifpp::get<double>(env, argv[1]);

        S2Cap cap = S2Cap::FromCenterHeight(center, height);
        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_from_center_area(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Point center = nifpp::get<S2Point>(env, argv[0]);
        double area = nifpp::get<double>(env, argv[1]);

        S2Cap cap = S2Cap::FromCenterArea(center, area);
        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_empty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 0);

        S2Cap cap = S2Cap::Empty();
        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_full(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 0);

        S2Cap cap = S2Cap::Full();
        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_center(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->center());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_radius(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->radius());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_height(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->height());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_get_radius(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->GetRadius());

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_get_area(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->GetArea());

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_get_centroid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->GetCentroid());


    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2cap_is_valid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->is_valid());


    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_is_empty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->is_empty());

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_is_full(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->is_full());


    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2cap_complement(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2Cap cap = self->Complement();
        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_contains(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2Cap *other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self->Contains(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_intersects(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2Cap *other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self->Intersects(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_interior_intersects(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2Cap *other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self->InteriorIntersects(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_interior_contains(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2Point point;
        nifpp::get_throws(env, argv[1], point);

        return nifpp::make(env, self->InteriorContains(point));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2cap_add_point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2Point point;
        nifpp::get_throws(env, argv[1], point);
        S2Cap *cap = self->Clone();
        cap->AddPoint(point);

        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(*cap);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_add_cap(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2Cap *other;
        nifpp::get_throws(env, argv[1], other);

        S2Cap *cap = self->Clone();
        cap->AddCap(*other);
        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(*cap);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_expanded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S1Angle distance;
        nifpp::get_throws(env, argv[1], distance);

        S2Cap cap = self->Expanded(distance);
        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_union(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2Cap *other;
        nifpp::get_throws(env, argv[1], other);

        S2Cap cap = self->Union(*other);
        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_clone(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->Clone());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_get_cap_bound(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2Cap cap = self->GetCapBound();
        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_get_rect_bound(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2LatLngRect s2latlngrect = self->GetRectBound();
        nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(s2latlngrect);
        return nifpp::make(env, s2rect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_get_cell_union_bound(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        //TODO
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_contains_s2cell(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2CellId cellid;
        nifpp::get_throws(env, argv[1], cellid);
        S2Cell cell(cellid);

        return nifpp::make(env, self->Contains(cell));

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cap_contains_s2cellid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2Cell *cell;
        nifpp::get_throws(env, argv[1], cell);

        return nifpp::make(env, self->Contains(*cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_may_intersect_s2cell(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2Cell *cell;
        nifpp::get_throws(env, argv[1], cell);

        return nifpp::make(env, self->MayIntersect(*cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_may_intersect_s2cellid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2CellId cellid;
        nifpp::get_throws(env, argv[1], cellid);
        S2Cell cell(cellid);

        return nifpp::make(env, self->MayIntersect(cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_contains_s2point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2Point point;
        nifpp::get_throws(env, argv[1], point);

        return nifpp::make(env, self->Contains(point));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2cap_encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2Cap *self;
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


ERL_NIF_TERM s2cap_decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        std::string encoded;
        nifpp::get_throws(env, argv[0], encoded);

        Decoder decoder(encoded.c_str(), encoded.length());
        S2Cap decoded;
        decoded.Decode(&decoder);

        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(decoded);
        return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_equal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2Cap *other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, (*self == *other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2cap_approx_equals(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 3);
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        S2Cap *other;
        nifpp::get_throws(env, argv[1], other);

        S1Angle max_error;
        nifpp::get_throws(env, argv[2], max_error);

        return nifpp::make(env, self->ApproxEquals(*other, max_error));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



// ERL_NIF_TERM s2region_coverer_get_covering_for_s2cap(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
//     if(argc < 2) // Need atleast one arg
//         return enif_make_badarg(env);

// // S2RegionCoverer::Options options;
// // options.set_max_cells(5);
// // S2RegionCoverer coverer(options);
// // S2Cap cap(center, radius);
// // S2CellUnion covering = coverer.GetCovering(cap);

//     try{
//         S2Cap *self;
//         nifpp::get_throws(env, argv[0], self);
//         auto covering_type = static_cast<S2RegionCoveringType>( nifpp::get<int>(env, nifpp::TERM(argv[1])) );
//         S2RegionCoverer::Options options;

//         auto max_cells = nifpp::get<int>(env, argv[2]);
//         if(max_cells > 0)
//         {
//             options.set_max_cells(max_cells);
//         }

//         if (argc == 4)
//         {
//             int min_level;
//             int max_level;
//             auto levels = make_tuple(ref(min_level), ref(max_level));
//             nifpp::get(env, argv[3], levels);
//             options.set_min_level(min_level);
//             options.set_max_level(max_level);
//         }
//         S2RegionCoverer coverer(options);

//         switch( covering_type ) {
//             case S2RegionCoveringType::cellid_covering:
//             {
//                 std::vector<S2CellId> covering;
//                 coverer.GetCovering(*self, &covering);
//                 return nifpp::make(env, covering);
//             }

//             case S2RegionCoveringType::cellid_interior_covering:
//             {
//                 std::vector<S2CellId> covering;
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
