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

      S2LatLngRect constructed(low_lat_lng, hi_lat_lng);

      nifpp::resource_ptr<NifS2LatLngRectRef> s2rect_ref = nifpp::construct_resource<NifS2LatLngRectRef>(constructed);
      return nifpp::make(env, s2rect_ref);
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

      S2LatLngRect constructed(lat, lng);

      nifpp::resource_ptr<NifS2LatLngRectRef> s2rect_ref = nifpp::construct_resource<NifS2LatLngRectRef>(constructed);
      return nifpp::make(env, s2rect_ref);
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

      nifpp::resource_ptr<NifS2LatLngRectRef> s2rect_ref = nifpp::construct_resource<NifS2LatLngRectRef>(constructed);
      return nifpp::make(env, s2rect_ref);
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

      nifpp::resource_ptr<NifS2LatLngRectRef> s2rect_ref = nifpp::construct_resource<NifS2LatLngRectRef>(constructed);
      return nifpp::make(env, s2rect_ref);
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

      nifpp::resource_ptr<NifS2LatLngRectRef> s2rect_ref = nifpp::construct_resource<NifS2LatLngRectRef>(constructed);
      return nifpp::make(env, s2rect_ref);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_empty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 0);
      S2LatLngRect constructed = S2LatLngRect::Empty();

      nifpp::resource_ptr<NifS2LatLngRectRef> s2rect_ref = nifpp::construct_resource<NifS2LatLngRectRef>(constructed);
      return nifpp::make(env, s2rect_ref);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_full(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 0);
      S2LatLngRect constructed = S2LatLngRect::Full();

      nifpp::resource_ptr<NifS2LatLngRectRef> s2rect_ref = nifpp::construct_resource<NifS2LatLngRectRef>(constructed);
      return nifpp::make(env, s2rect_ref);
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
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->lat_lo());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_lat_hi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->lat_hi());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_lng_lo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->lng_lo());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_lng_hi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->lng_hi());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_lat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->lat());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_lng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->lng());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_lo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->lo());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_hi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->hi());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_is_valid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->is_valid());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_is_empty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->is_empty());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_is_full(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->is_full());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_is_point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->is_point());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_is_inverted(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->is_inverted());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_get_vertex(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        S2LatLng coords[4];
        coords[0] = s2latlng_rect->GetVertex(0);
        coords[1] = s2latlng_rect->GetVertex(1);
        coords[2] = s2latlng_rect->GetVertex(2);
        coords[3] = s2latlng_rect->GetVertex(3);
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
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->GetCenter());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_get_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->GetSize());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_area(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        return nifpp::make(env, s2latlng_rect->Area());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_contains_s2latlng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        auto s2latlng = nifpp::get<S2LatLng>(env, argv[1]);
        return nifpp::make(env, s2latlng_rect->Contains(s2latlng));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_interior_contains_s2point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        S2Point point = nifpp::get<S2Point>(env, argv[1]);
        return nifpp::make(env, s2latlng_rect->InteriorContains(point));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_interior_contains_s2latlng(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        auto s2latlng = nifpp::get<S2LatLng>(env, argv[1]);

        return nifpp::make(env, s2latlng_rect->InteriorContains(s2latlng));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_contains_s2latlngrect(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        NifS2LatLngRectRef *other_ref;
        nifpp::get_throws(env, argv[1], other_ref);

        std::lock(ref->mutex, other_ref->mutex);
        std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
        std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

        S2LatLngRect *s2latlng_rect = ref->rect;
        S2LatLngRect *other = other_ref->rect;

        return nifpp::make(env, s2latlng_rect->Contains(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_interior_contains_s2latlngrect(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        NifS2LatLngRectRef *other_ref;
        nifpp::get_throws(env, argv[1], other_ref);

        std::lock(ref->mutex, other_ref->mutex);
        std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
        std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

        S2LatLngRect *s2latlng_rect = ref->rect;
        S2LatLngRect *other = other_ref->rect;

        return nifpp::make(env, s2latlng_rect->InteriorContains(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_intersects_s2latlngrect(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        NifS2LatLngRectRef *other_ref;
        nifpp::get_throws(env, argv[1], other_ref);

        std::lock(ref->mutex, other_ref->mutex);
        std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
        std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

        S2LatLngRect *s2latlng_rect = ref->rect;
        S2LatLngRect *other = other_ref->rect;

        return nifpp::make(env, s2latlng_rect->Intersects(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_intersects_s2cell_with_id(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[1]));
        auto s2cellid = S2CellId(static_cast<uint64>(value));
        S2Cell cell(s2cellid);

        return nifpp::make(env, s2latlng_rect->Intersects(cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_interior_intersects_s2latlngrect(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        NifS2LatLngRectRef *other_ref;
        nifpp::get_throws(env, argv[1], other_ref);

        std::lock(ref->mutex, other_ref->mutex);
        std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
        std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

        S2LatLngRect *s2latlng_rect = ref->rect;
        S2LatLngRect *other = other_ref->rect;

        return nifpp::make(env, s2latlng_rect->InteriorIntersects(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2latlngrect_boundary_intersects(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 3);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        S2Point v0 = nifpp::get<S2Point>(env, argv[1]);
        S2Point v1 = nifpp::get<S2Point>(env, argv[2]);

        return nifpp::make(env, s2latlng_rect->BoundaryIntersects(v0, v1));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_contains_s2cell_with_id(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        auto s2cellid = nifpp::get<S2CellId>(env, nifpp::TERM(argv[1]));
        S2Cell cell(s2cellid);
        return nifpp::make(env, s2latlng_rect->Contains(cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_contains_s2point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        S2Point point = nifpp::get<S2Point>(env, argv[1]);
        return nifpp::make(env, s2latlng_rect->Contains(point));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_may_intersect_s2cell_with_id(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        auto s2cellid = nifpp::get<S2CellId>(env, nifpp::TERM(argv[1]));
        S2Cell cell(s2cellid);
        return nifpp::make(env, s2latlng_rect->MayIntersect(cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_expanded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        auto s2latlng = nifpp::get<S2LatLng>(env, argv[1]);
        S2LatLngRect constructed = s2latlng_rect->Expanded(s2latlng);
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
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        NifS2LatLngRectRef *other_ref;
        nifpp::get_throws(env, argv[1], other_ref);

        std::lock(ref->mutex, other_ref->mutex);
        std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
        std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

        S2LatLngRect *s2latlng_rect = ref->rect;
        S2LatLngRect *other = other_ref->rect;

        S2LatLngRect constructed = s2latlng_rect->Union(*other);
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
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        NifS2LatLngRectRef *other_ref;
        nifpp::get_throws(env, argv[1], other_ref);

        std::lock(ref->mutex, other_ref->mutex);
        std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
        std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

        S2LatLngRect *s2latlng_rect = ref->rect;
        S2LatLngRect *other = other_ref->rect;

        S2LatLngRect constructed = s2latlng_rect->Intersection(*other);
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
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        auto s2latlng = nifpp::get<S2LatLng>(env, argv[1]);

        s2latlng_rect->AddPoint(s2latlng);
        return nifpp::make(env, ref);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_add_point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        S2Point point = nifpp::get<S2Point>(env, argv[1]);
        s2latlng_rect->AddPoint(point);

        return nifpp::make(env, ref);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_get_distance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        NifS2LatLngRectRef *other_ref;
        nifpp::get_throws(env, argv[1], other_ref);

        std::lock(ref->mutex, other_ref->mutex);
        std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
        std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

        S2LatLngRect *s2latlng_rect = ref->rect;
        S2LatLngRect *other = other_ref->rect;

        S1Angle distance = s2latlng_rect->GetDistance(*other);
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
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        auto s2latlng = nifpp::get<S2LatLng>(env, argv[1]);

        S1Angle distance = s2latlng_rect->GetDistance(s2latlng);
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
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        NifS2LatLngRectRef *other_ref;
        nifpp::get_throws(env, argv[1], other_ref);

        std::lock(ref->mutex, other_ref->mutex);
        std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
        std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

        S2LatLngRect *s2latlng_rect = ref->rect;
        S2LatLngRect *other = other_ref->rect;

        S1Angle distance = s2latlng_rect->GetDirectedHausdorffDistance(*other);
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

        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        NifS2LatLngRectRef *other_ref;
        nifpp::get_throws(env, argv[1], other_ref);

        std::lock(ref->mutex, other_ref->mutex);
        std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
        std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

        S2LatLngRect *s2latlng_rect = ref->rect;
        S2LatLngRect *other = other_ref->rect;

        S1Angle distance = s2latlng_rect->GetHausdorffDistance(*other);
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

        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        NifS2LatLngRectRef *other_ref;
        nifpp::get_throws(env, argv[1], other_ref);

        std::lock(ref->mutex, other_ref->mutex);
        std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
        std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

        S2LatLngRect *s2latlng_rect = ref->rect;
        S2LatLngRect *other = other_ref->rect;

        return nifpp::make(env, (*s2latlng_rect == *other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_not_equal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 2);

        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        NifS2LatLngRectRef *other_ref;
        nifpp::get_throws(env, argv[1], other_ref);

        std::lock(ref->mutex, other_ref->mutex);
        std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
        std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

        S2LatLngRect *s2latlng_rect = ref->rect;
        S2LatLngRect *other = other_ref->rect;

        return nifpp::make(env, (*s2latlng_rect != *other));
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

        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        NifS2LatLngRectRef *other_ref;
        nifpp::get_throws(env, argv[1], other_ref);

        std::lock(ref->mutex, other_ref->mutex);
        std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
        std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

        S2LatLngRect *s2latlng_rect = ref->rect;
        S2LatLngRect *other = other_ref->rect;

        if (argc == 3) {
            S1Angle max_error = nifpp::get<S1Angle>(env, nifpp::TERM(argv[2]));
            return nifpp::make(env, s2latlng_rect->ApproxEquals(*other, max_error));
        }
        return nifpp::make(env, s2latlng_rect->ApproxEquals(*other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_approx_equals_with_s2latlng_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 3);

        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        NifS2LatLngRectRef *other_ref;
        nifpp::get_throws(env, argv[1], other_ref);

        std::lock(ref->mutex, other_ref->mutex);
        std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
        std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

        S2LatLngRect *s2latlng_rect = ref->rect;
        S2LatLngRect *other = other_ref->rect;

        S2LatLng max_error = nifpp::get<S2LatLng>(env, nifpp::TERM(argv[2]));
        return nifpp::make(env, s2latlng_rect->ApproxEquals(*other, max_error));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2latlngrect_get_cap_bound(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        auto cap = s2latlng_rect->GetCapBound();
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
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        S2LatLngRect rect_bound = s2latlng_rect->GetRectBound();
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
        NifS2LatLngRectRef *ref;
        nifpp::get_throws(env, argv[0], ref);
        std::lock_guard<std::mutex> guard(ref->mutex);
        S2LatLngRect *s2latlng_rect = ref->rect;

        Encoder encoder;
        s2latlng_rect->Encode(&encoder);
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

