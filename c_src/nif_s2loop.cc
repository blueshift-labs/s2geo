#include "s2/s2cell.h"
#include "s2/s2loop.h"
#include "s2/s2cell_union.h"
#include "s2/s2latlng_rect.h"
#include "s2/s2cap.h"
//#include "s2regioncoverer.h"
#include "s2/util/coding/coder.h"

#include "s2geo_nif.h"
#include "nifpp_utils.h"
#include "nif_s2loop.h"

#include<functional>
#include <mutex>

using std::make_tuple;
using std::ref;

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)


ERL_NIF_TERM s2loop_new_from_s2point_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        std::vector<S2Point> points;
        nifpp::get_throws(env, argv[0], points);

        S2Loop *s2loop = new S2Loop(points);
        s2loop->Normalize();

        nifpp::resource_ptr<NifS2LoopRef> s2loop_ref = nifpp::construct_resource<NifS2LoopRef>(s2loop);
        return nifpp::make(env, s2loop_ref);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_new_from_s2latlng_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        CHECK_ARGS_LENGTH(env, argc, 1);
        std::vector<S2LatLng> latlngs;
        nifpp::get_throws(env, argv[0], latlngs);

        std::vector<S2Point> points;
        for(auto it=latlngs.begin() ; it < latlngs.end(); it++ ){
            points.push_back(it->Normalized().ToPoint());
        }

        S2Loop *s2loop = new S2Loop(points);
        s2loop->Normalize();

        nifpp::resource_ptr<NifS2LoopRef> s2loop_ref = nifpp::construct_resource<NifS2LoopRef>(s2loop);
        return nifpp::make(env, s2loop_ref);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_new_from_s2cellid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      S2CellId cellid;
      nifpp::get_throws(env, argv[0], cellid);
      S2Cell cell(cellid);

      S2Loop *s2loop = new S2Loop(cell);
      s2loop->Normalize();

      nifpp::resource_ptr<NifS2LoopRef> s2loop_ref = nifpp::construct_resource<NifS2LoopRef>(s2loop);
      return nifpp::make(env, s2loop_ref);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2loop_new_from_s2cell(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      S2Cell *cell;
      nifpp::get_throws(env, argv[0], cell);

      S2Loop *s2loop = new S2Loop(*cell);
      s2loop->Normalize();

      nifpp::resource_ptr<NifS2LoopRef> s2loop_ref = nifpp::construct_resource<NifS2LoopRef>(s2loop);
      return nifpp::make(env, s2loop_ref);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_is_valid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      return nifpp::make(env, s2loop->IsValid());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_num_vertices(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      return nifpp::make(env, s2loop->num_vertices());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_vertex(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      auto i = nifpp::get<int>(env, argv[1]);
      return nifpp::make(env, s2loop->vertex(i));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_oriented_vertex(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      auto i = nifpp::get<int>(env, argv[1]);
      return nifpp::make(env, s2loop->oriented_vertex(i));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_is_empty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      return nifpp::make(env, s2loop->is_empty());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_is_full(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      return nifpp::make(env, s2loop->is_full());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_is_empty_or_full(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      return nifpp::make(env, s2loop->is_empty_or_full());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_depth(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      return nifpp::make(env, s2loop->depth());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_set_depth(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      auto i = nifpp::get<int>(env, argv[1]);
      s2loop->set_depth(i);
      return nifpp::make(env, ref);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2loop_is_hole(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      return nifpp::make(env, s2loop->is_hole());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_sign(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      return nifpp::make(env, s2loop->sign());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_is_normalized(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      return nifpp::make(env, s2loop->IsNormalized());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_normalize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      s2loop->Normalize();
      return nifpp::make(env, ref);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_invert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      s2loop->Invert();
      return nifpp::make(env, ref);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_get_area(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      return nifpp::make(env, s2loop->GetArea());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_get_centroid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      return nifpp::make(env, s2loop->GetCentroid());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_get_turning_angle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      return nifpp::make(env, s2loop->GetTurningAngle());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_get_turning_angle_max_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      return nifpp::make(env, s2loop->GetTurningAngleMaxError());
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2loop_get_distance(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      S2Point point;
      nifpp::get_throws(env, argv[1], point);

      return nifpp::make(env, s2loop->GetDistance(point));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2loop_get_distance_to_boundary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      S2Point point;
      nifpp::get_throws(env, argv[1], point);

      return nifpp::make(env, s2loop->GetDistanceToBoundary(point));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_project(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      S2Point point;
      nifpp::get_throws(env, argv[1], point);

      return nifpp::make(env, s2loop->Project(point));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_project_to_boundary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      S2Point point;
      nifpp::get_throws(env, argv[1], point);

      return nifpp::make(env, s2loop->ProjectToBoundary(point));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_contains_s2loop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);

      NifS2LoopRef *other_ref;
      nifpp::get_throws(env, argv[1], other_ref);

      std::lock(ref->mutex, other_ref->mutex);
      std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
      std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

      S2Loop *s2loop = ref->s2loop;
      S2Loop *other = other_ref->s2loop;

      return nifpp::make(env, s2loop->Contains(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_contains_s2cell(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      S2Cell *cell;
      nifpp::get_throws(env, argv[1], cell);

      return nifpp::make(env, s2loop->Contains(*cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2loop_contains_s2cellid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      S2CellId cellid;
      nifpp::get_throws(env, argv[1], cellid);
      S2Cell cell(cellid);

      return nifpp::make(env, s2loop->Contains(cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2loop_contains_s2point(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      S2Point point;
      nifpp::get_throws(env, argv[1], point);

      return nifpp::make(env, s2loop->Contains(point));

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_intersects(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);

      NifS2LoopRef *other_ref;
      nifpp::get_throws(env, argv[1], other_ref);

      std::lock(ref->mutex, other_ref->mutex);
      std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
      std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

      S2Loop *s2loop = ref->s2loop;
      S2Loop *other = other_ref->s2loop;

      return nifpp::make(env, s2loop->Intersects(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_equals(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);

      NifS2LoopRef *other_ref;
      nifpp::get_throws(env, argv[1], other_ref);

      std::lock(ref->mutex, other_ref->mutex);
      std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
      std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

      S2Loop *s2loop = ref->s2loop;
      S2Loop *other = other_ref->s2loop;

      return nifpp::make(env, s2loop->Equals(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_boundary_equals(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);

      NifS2LoopRef *other_ref;
      nifpp::get_throws(env, argv[1], other_ref);

      std::lock(ref->mutex, other_ref->mutex);
      std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
      std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

      S2Loop *s2loop = ref->s2loop;
      S2Loop *other = other_ref->s2loop;

      return nifpp::make(env, s2loop->BoundaryEquals(other));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_boundary_approx_equals(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 3);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);

      NifS2LoopRef *other_ref;
      nifpp::get_throws(env, argv[1], other_ref);

      S1Angle max_error = nifpp::get<S1Angle>(env, argv[2]);

      std::lock(ref->mutex, other_ref->mutex);
      std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
      std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

      S2Loop *s2loop = ref->s2loop;
      S2Loop *other = other_ref->s2loop;

      return nifpp::make(env, s2loop->BoundaryApproxEquals(*other, max_error));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_boundary_near(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 3);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);

      NifS2LoopRef *other_ref;
      nifpp::get_throws(env, argv[1], other_ref);

      S1Angle max_error = nifpp::get<S1Angle>(env, argv[2]);

      std::lock(ref->mutex, other_ref->mutex);
      std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
      std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

      S2Loop *s2loop = ref->s2loop;
      S2Loop *other = other_ref->s2loop;

      return nifpp::make(env, s2loop->BoundaryNear(*other, max_error));

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_clone(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
       NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      S2Loop *cloned = s2loop->Clone();
      nifpp::resource_ptr<NifS2LoopRef> s2loop_ref = nifpp::construct_resource<NifS2LoopRef>(cloned);
      return nifpp::make(env, s2loop_ref);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_get_cap_bound(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      auto cap = s2loop->GetCapBound();
      nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
      return nifpp::make(env, s2cap_ptr);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_get_rect_bound(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(s2loop->GetRectBound());
      return nifpp::make(env, s2rect);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2loop_may_intersect_s2cell(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      S2Cell *cell;
      nifpp::get_throws(env, argv[1], cell);

      return nifpp::make(env, s2loop->MayIntersect(*cell));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_may_intersect_s2cellid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      S2CellId cellid;
      nifpp::get_throws(env, argv[1], cellid);
      S2Cell cell(cellid);

      return nifpp::make(env, s2loop->MayIntersect(cell));

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}



ERL_NIF_TERM s2loop_encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);
      std::lock_guard<std::mutex> guard(ref->mutex);
      S2Loop *s2loop = ref->s2loop;

      Encoder encoder;
      s2loop->Encode(&encoder);
      std::string encoded(encoder.base(), encoder.length());
      return nifpp::make(env, encoded);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 1);
      std::string encoded;
      nifpp::get_throws(env, argv[0], encoded);
      Decoder decoder(encoded.c_str(), encoded.length());

      S2Loop *s2loop = new S2Loop();
      s2loop->Decode(&decoder);
      s2loop->Normalize();

      nifpp::resource_ptr<NifS2LoopRef> s2loop_ref = nifpp::construct_resource<NifS2LoopRef>(s2loop);
      return nifpp::make(env, s2loop_ref);
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_contains_nested(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);

      NifS2LoopRef *other_ref;
      nifpp::get_throws(env, argv[1], other_ref);

      std::lock(ref->mutex, other_ref->mutex);
      std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
      std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

      S2Loop *s2loop = ref->s2loop;
      S2Loop *other = other_ref->s2loop;

      return nifpp::make(env, s2loop->ContainsNested(other));

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}




ERL_NIF_TERM s2loop_compare_boundary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 2);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);

      NifS2LoopRef *other_ref;
      nifpp::get_throws(env, argv[1], other_ref);

      std::lock(ref->mutex, other_ref->mutex);
      std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
      std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

      S2Loop *s2loop = ref->s2loop;
      S2Loop *other = other_ref->s2loop;

      return nifpp::make(env, s2loop->CompareBoundary(other));

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_contains_non_crossing_boundary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    try {
      CHECK_ARGS_LENGTH(env, argc, 3);
      NifS2LoopRef *ref;
      nifpp::get_throws(env, argv[0], ref);

      NifS2LoopRef *other_ref;
      nifpp::get_throws(env, argv[1], other_ref);

      bool reverse_b = nifpp::get<bool>(env, argv[2]);

      std::lock(ref->mutex, other_ref->mutex);
      std::lock_guard<std::mutex> lock_guard1(ref->mutex, std::adopt_lock);
      std::lock_guard<std::mutex> lock_guard2(other_ref->mutex, std::adopt_lock);

      S2Loop *s2loop = ref->s2loop;
      S2Loop *other = other_ref->s2loop;

      return nifpp::make(env, s2loop->ContainsNonCrossingBoundary(other, reverse_b));
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}
    return enif_make_badarg(env);
}
