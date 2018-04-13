#include "s2/s2cell.h"
#include "s2/s2latlng_rect.h"
#include "s2/s2region_coverer.h"
#include "s2/s2cell_union.h"
#include "s2/s2cap.h"
#include "s2/util/coding/coder.h"

#include "s2geo_nif.h"
#include "nifpp_utils.h"
#include "nif_s2latlngrect.h"

#include<functional>

using std::make_tuple;
using std::ref;

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)


ERL_NIF_TERM s2latlngrect_from_lat_lng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      auto low_lat_lng = nifpp::get<S2LatLng>(env, argv[0]);
      auto hi_lat_lng = nifpp::get<S2LatLng>(env, argv[1]);
      nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(low_lat_lng, hi_lat_lng);
      return nifpp::make(env, s2rect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_from_r1inteval_s1interval(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      auto lat = nifpp::get<R1Interval>(env, argv[0]);
      auto lng = nifpp::get<S1Interval>(env, argv[1]);
      nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(lat, lng);
      return nifpp::make(env, s2rect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_from_center_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);

      auto center = nifpp::get<S2LatLng>(env, argv[0]);
      auto size = nifpp::get<S2LatLng>(env, argv[1]);
      S2LatLngRect constructed = S2LatLngRect::FromCenterSize(center, size);

      nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
      return nifpp::make(env, s2rect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_from_point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);

      auto p1_lat_lng = nifpp::get<S2LatLng>(env, argv[0]);
      S2LatLngRect constructed = S2LatLngRect::FromPoint(p1_lat_lng);

      nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
      return nifpp::make(env, s2rect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_from_point_pair(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);

      auto p1_lat_lng = nifpp::get<S2LatLng>(env, argv[0]);
      auto p2_lat_lng = nifpp::get<S2LatLng>(env, argv[1]);

      S2LatLngRect constructed = S2LatLngRect::FromPointPair(p1_lat_lng, p2_lat_lng);

      nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
      return nifpp::make(env, s2rect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_empty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      S2LatLngRect constructed = S2LatLngRect::Empty();

      nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
      return nifpp::make(env, s2rect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_full(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      S2LatLngRect constructed = S2LatLngRect::Full();

      nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
      return nifpp::make(env, s2rect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_full_lat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
     return nifpp::make(env, S2LatLngRect::FullLat());
}

ERL_NIF_TERM s2latlngrect_full_lng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
     return nifpp::make(env, S2LatLngRect::FullLng());
}


ERL_NIF_TERM s2latlngrect_lat_lo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->lat_lo());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_lat_hi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->lat_hi());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_lng_lo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->lng_lo());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_lng_hi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->lng_hi());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_lat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->lat());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_lng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->lng());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_lo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->lo());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_hi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->hi());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_is_valid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->is_valid());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_is_empty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->is_empty());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_is_full(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->is_full());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_is_point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->is_point());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_is_inverted(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->is_inverted());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_get_vertex(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLng coords[4];
        coords[0] = self->GetVertex(0);
        coords[1] = self->GetVertex(1);
        coords[2] = self->GetVertex(2);
        coords[3] = self->GetVertex(3);
        std::vector<S2LatLng> vect(coords, coords + 3);
        return nifpp::make(env, vect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_get_center(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->GetCenter());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_get_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->GetSize());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_area(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        return nifpp::make(env, self->Area());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_contains_s2latlng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        auto s2latlng = nifpp::get<S2LatLng>(env, argv[1]);
        return nifpp::make(env, self->Contains(s2latlng));

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_interior_contains_s2point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        S2Point point = nifpp::get<S2Point>(env, argv[1]);
        return nifpp::make(env, self->InteriorContains(point));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_interior_contains_s2latlng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        auto s2latlng = nifpp::get<S2LatLng>(env, argv[1]);

        return nifpp::make(env, self->InteriorContains(s2latlng));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_contains_s2latlngrect(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLngRect *other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self->Contains(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_interior_contains_s2latlngrect(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLngRect *other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self->InteriorContains(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_intersects_s2latlngrect(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);

        S2LatLngRect *other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self->Intersects(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_intersects_s2cell_with_id(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[1]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        S2Cell cell(s2cellid);

        return nifpp::make(env, self->Intersects(cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_interior_intersects_s2latlngrect(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLngRect *other;
        nifpp::get_throws(env, argv[1], other);
        return nifpp::make(env, self->InteriorIntersects(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2latlngrect_boundary_intersects(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 3);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        S2Point v0 = nifpp::get<S2Point>(env, argv[1]);
        S2Point v1 = nifpp::get<S2Point>(env, argv[2]);

        return nifpp::make(env, self->BoundaryIntersects(v0, v1));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_contains_s2cell_with_id(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        auto s2cellid = nifpp::get<S2CellId>(env, nifpp::TERM(argv[1]));
        S2Cell cell(s2cellid);
        return nifpp::make(env, self->Contains(cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_contains_s2point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        S2Point point = nifpp::get<S2Point>(env, argv[1]);
        return nifpp::make(env, self->Contains(point));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_may_intersect_s2cell_with_id(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        auto s2cellid = nifpp::get<S2CellId>(env, nifpp::TERM(argv[1]));
        S2Cell cell(s2cellid);
        return nifpp::make(env, self->MayIntersect(cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_expanded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);

        auto s2latlng = nifpp::get<S2LatLng>(env, argv[1]);
        S2LatLngRect constructed = self->Expanded(s2latlng);
        nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
        return nifpp::make(env, s2rect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_union(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);

        S2LatLngRect *other;
        nifpp::get_throws(env, argv[1], other);

        S2LatLngRect constructed = self->Union(*other);
        nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
        return nifpp::make(env, s2rect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_intersection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);

        S2LatLngRect *other;
        nifpp::get_throws(env, argv[1], other);

        S2LatLngRect constructed = self->Intersection(*other);
        nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
        return nifpp::make(env, s2rect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_add_point_s2latlng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);

        auto s2latlng = nifpp::get<S2LatLng>(env, argv[1]);

        S2LatLngRect *cloned = self->Clone();
        cloned->AddPoint(s2latlng);

        nifpp::resource_ptr<S2LatLngRect> s2rect_resource = nifpp::construct_resource<S2LatLngRect>(*cloned);
        return nifpp::make(env, s2rect_resource);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_add_point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        S2Point point = nifpp::get<S2Point>(env, argv[1]);

        S2LatLngRect *cloned = self->Clone();
        cloned->AddPoint(point);

        nifpp::resource_ptr<S2LatLngRect> s2rect_resource = nifpp::construct_resource<S2LatLngRect>(*cloned);
        return nifpp::make(env, s2rect_resource);

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_get_distance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);

        S2LatLngRect *other;
        nifpp::get_throws(env, argv[1], other);

        S1Angle distance = self->GetDistance(*other);
        return nifpp::make(env, distance.radians());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_get_distance_s2latlng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);

        auto s2latlng = nifpp::get<S2LatLng>(env, argv[1]);

        S1Angle distance = self->GetDistance(s2latlng);
        return nifpp::make(env, distance.radians());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_get_directed_hausdorff_distance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);

        S2LatLngRect *other;
        nifpp::get_throws(env, argv[1], other);

        S1Angle distance = self->GetDirectedHausdorffDistance(*other);
        return nifpp::make(env, distance.radians());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_get_hausdorff_distance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLngRect *other;
        nifpp::get_throws(env, argv[1], other);

        S1Angle distance = self->GetHausdorffDistance(*other);
        return nifpp::make(env, distance.radians());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_equal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLngRect *other;
        nifpp::get_throws(env, argv[1], other);

        return nifpp::make(env, (*self == *other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_not_equal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLngRect *other;
        nifpp::get_throws(env, argv[1], other);

        return nifpp::make(env, (*self != *other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_approx_equals_with_s1angle_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        if (argc > 3) {
            return enif_make_badarg(env);
        }

        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLngRect *other;
        nifpp::get_throws(env, argv[1], other);

        if (argc == 3) {
            S1Angle max_error = nifpp::get<S1Angle>(env, nifpp::TERM(argv[2]));
            return nifpp::make(env, self->ApproxEquals(*other, max_error));
        }
        return nifpp::make(env, self->ApproxEquals(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_approx_equals_with_s2latlng_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 3);

        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);
        S2LatLngRect *other;
        nifpp::get_throws(env, argv[1], other);

        S2LatLng max_error = nifpp::get<S2LatLng>(env, nifpp::TERM(argv[2]));
        return nifpp::make(env, self->ApproxEquals(*other, max_error));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_get_cap_bound(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);

        auto cap = self->GetCapBound();
        nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
        return nifpp::make(env, s2cap_ptr);

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_get_rect_bound(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);

        S2LatLngRect rect_bound = self->GetRectBound();
        nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(rect_bound);
        return nifpp::make(env, s2rect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      std::string encoded;
      nifpp::get_throws(env, argv[0], encoded);

      Decoder decoder(encoded.c_str(), encoded.length());
      S2LatLngRect decoded;
      decoded.Decode(&decoder);

      nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(decoded);
      return nifpp::make(env, s2rect);

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        S2LatLngRect *self;
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


ERL_NIF_TERM s2latlngrect_intersects_lat_edge(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 4);

        S2Point a = nifpp::get<S2Point>(env, argv[0]);
        S2Point b = nifpp::get<S2Point>(env, argv[1]);
        double lat = nifpp::get<double>(env, argv[2]);
        S1Interval lng = nifpp::get<S1Interval>(env, argv[3]);

        return nifpp::make(env, S2LatLngRect::IntersectsLatEdge(a, b, lat, lng));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_intersects_lng_edge(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 4);

        S2Point a = nifpp::get<S2Point>(env, argv[0]);
        S2Point b = nifpp::get<S2Point>(env, argv[1]);
        R1Interval lat = nifpp::get<R1Interval>(env, argv[2]);
        double lng = nifpp::get<double>(env, argv[3]);

        return nifpp::make(env, S2LatLngRect::IntersectsLngEdge(a, b, lat, lng));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

