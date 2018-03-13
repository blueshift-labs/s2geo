#include "s2cell.h"
#include "s2loop.h"
#include "s2cellunion.h"
#include "s2latlngrect.h"
#include "s2regioncoverer.h"
#include "util/coding/coder.h"

#include "s2geo_nif.h"
#include "nifpp_utils.h"
#include "nif_s2loop.h"

#include<functional>

using std::make_tuple;
using std::ref;

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)



ERL_NIF_TERM s2loop_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc < 1) // Need atleast one arg
        return enif_make_badarg(env);

    try{
        auto option = static_cast<S2LoopConstructors>( nifpp::get<int>(env, nifpp::TERM(argv[0]) ) );
        switch( option ) {
            case S2LoopConstructors::from_s2latlng_list:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                std::vector<S2LatLng> latlngs;
                nifpp::get_throws(env, argv[1], latlngs);

                std::vector<S2Point> points;
                for(auto it=latlngs.begin() ; it < latlngs.end(); it++ ){
                    points.push_back(it->Normalized().ToPoint());
                }

                nifpp::resource_ptr<S2Loop> s2loop_ptr = nifpp::construct_resource<S2Loop>(points);
                S2Loop *s2loop = s2loop_ptr.get();
                s2loop->Normalize();
                return nifpp::make(env, s2loop_ptr);
            }

            case S2LoopConstructors::from_s2point_list:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                std::vector<S2Point> points;
                nifpp::get_throws(env, argv[1], points);

                nifpp::resource_ptr<S2Loop> s2loop_ptr = nifpp::construct_resource<S2Loop>(points);
                S2Loop *s2loop = s2loop_ptr.get();
                s2loop->Normalize();

                return nifpp::make(env, s2loop_ptr);
            }

            case S2LoopConstructors::from_s2cellid:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                S2CellId cellid;
                nifpp::get_throws(env, argv[1], cellid);
                S2Cell cell(cellid);
                nifpp::resource_ptr<S2Loop> s2loop_ptr = nifpp::construct_resource<S2Loop>(cell);

                return nifpp::make(env, s2loop_ptr);
            }

            case S2LoopConstructors::decode:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                std::string encoded;
                nifpp::get_throws(env, argv[1], encoded);
                Decoder decoder(encoded.c_str(), encoded.length());

                nifpp::resource_ptr<S2Loop> s2loop_ptr = nifpp::construct_resource<S2Loop>();
                S2Loop *s2loop = s2loop_ptr.get();
                s2loop->Decode(&decoder);
                s2loop->Normalize();

                return nifpp::make(env, s2loop_ptr);
            }

            case S2LoopConstructors::invert:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);

                S2Loop *self;
                nifpp::get_throws(env, argv[1], self);

                int num_vertices = self->num_vertices();
                std::vector<S2Point> points;
                for(int i = 0; i < num_vertices; i++ ){
                    points.push_back(self->vertex(i));
                }

                nifpp::resource_ptr<S2Loop> s2loop_ptr = nifpp::construct_resource<S2Loop>(points);
                S2Loop *s2loop = s2loop_ptr.get();
                s2loop->Invert();
                return nifpp::make(env, s2loop_ptr);
            }



            default:
                return ATOMS.atomError;
        }

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}

    return enif_make_badarg(env);
}


ERL_NIF_TERM s2loop_methods(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc < 2) // Need atleast one arg
        return enif_make_badarg(env);
    try{
        S2Loop *self;
        nifpp::get_throws(env, argv[0], self);

        auto option = static_cast<S2LoopMethods>( nifpp::get<int>(env, nifpp::TERM(argv[1])) );
        switch( option ) {
            case S2LoopMethods::is_valid:
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->IsValid());

            case S2LoopMethods::depth:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->depth());
            }
            case S2LoopMethods::is_hole:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->is_hole());

            }
            case S2LoopMethods::sign:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->sign());

            }
            case S2LoopMethods::num_vertices:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->num_vertices());

            }
            case S2LoopMethods::vertex:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                auto i = nifpp::get<int>(env, argv[2]);
                return nifpp::make(env, self->vertex(i));

            }
            case S2LoopMethods::is_normalized:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->IsNormalized());

            }
            case S2LoopMethods::normalize:
                return ATOMS.atomNotImplemented; // We normalize in constructor

            case S2LoopMethods::invert:
                return ATOMS.atomNotImplemented; // We normalize in constructor

            case S2LoopMethods::get_area:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->GetArea());

            }
            case S2LoopMethods::get_centroid:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->GetCentroid());

            }
            case S2LoopMethods::get_turning_angle:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->GetTurningAngle());

            }
            case S2LoopMethods::contains:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Loop *other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->Contains(other));
            }
            case S2LoopMethods::intersects:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Loop *other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->Intersects(other));

            }
            case S2LoopMethods::contains_nested:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Loop *other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->ContainsNested(other));

            }
            case S2LoopMethods::contains_or_crosses:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Loop *other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->ContainsOrCrosses(other));
            }
            case S2LoopMethods::boundary_equals:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Loop *other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->BoundaryEquals(other));

            }
            case S2LoopMethods::boundary_approx_equals:
            {
                CHECK_ARGS_LENGTH(env, argc, 4);
                S2Loop *other;
                nifpp::get_throws(env, argv[2], other);
                double max_error = nifpp::get<double>(env, argv[3]);
                return nifpp::make(env, self->BoundaryApproxEquals(other, max_error));
            }
            case S2LoopMethods::boundary_near:
            {
                CHECK_ARGS_LENGTH(env, argc, 4);
                S2Loop *other;
                nifpp::get_throws(env, argv[2], other);
                double max_error = nifpp::get<double>(env, argv[3]);
                return nifpp::make(env, self->BoundaryNear(other, max_error));
            }

            case S2LoopMethods::get_rect_bound:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(self->GetRectBound());
                return nifpp::make(env, s2rect);
            }

            case S2LoopMethods::contains_s2cellid:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2CellId cellid;
                nifpp::get_throws(env, argv[2], cellid);
                S2Cell cell(cellid);

                return nifpp::make(env, self->Contains(cell));
            }
            case S2LoopMethods::may_intersect_s2cellid:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2CellId cellid;
                nifpp::get_throws(env, argv[2], cellid);
                S2Cell cell(cellid);

                return nifpp::make(env, self->MayIntersect(cell));

            }
            case S2LoopMethods::contains_s2point:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Point point;
                nifpp::get_throws(env, argv[2], point);

                return nifpp::make(env, self->Contains(point));
            }
            case S2LoopMethods::encode:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                Encoder encoder;
                self->Encode(&encoder);
                std::string encoded(encoder.base(), encoder.length());
                return nifpp::make(env, encoded);
            }

            default:
                return ATOMS.atomNotImplemented;
        }
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}

    return enif_make_badarg(env);
}



ERL_NIF_TERM s2region_coverer_get_covering_for_s2loop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if(argc < 3) // Need atleast one arg
        return enif_make_badarg(env);
    try{
        S2Loop *self;
        nifpp::get_throws(env, argv[0], self);
        auto covering_type = static_cast<S2RegionCoveringType>( nifpp::get<int>(env, nifpp::TERM(argv[1])) );
        S2RegionCoverer coverer;

        auto max_cells = nifpp::get<int>(env, argv[2]);
        if(max_cells > 0)
        {
            coverer.set_max_cells(max_cells);
        }


        if (argc == 4)
        {
            int min_level;
            int max_level;
            auto levels = make_tuple(ref(min_level), ref(max_level));
            nifpp::get(env, argv[3], levels);
            coverer.set_min_level(min_level);
            coverer.set_max_level(max_level);
        }

        switch( covering_type ) {
            case S2RegionCoveringType::cellid_covering:
            {
                vector<S2CellId> covering;
                coverer.GetCovering(*self, &covering);
                return nifpp::make(env, covering);
            }

            case S2RegionCoveringType::cellid_interior_covering:
            {
                vector<S2CellId> covering;
                coverer.GetInteriorCovering(*self, &covering);
                return nifpp::make(env, covering);
            }
            case S2RegionCoveringType::cell_union_covering:
            {
                nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
                S2CellUnion *covering = s2cellunion_ptr.get();

                coverer.GetCellUnion(*self, covering);
                return nifpp::make(env, s2cellunion_ptr);
            }
            case S2RegionCoveringType::interior_cell_union_covering:
            {
                nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
                S2CellUnion *covering = s2cellunion_ptr.get();

                coverer.GetInteriorCellUnion(*self, covering);
                return nifpp::make(env, s2cellunion_ptr);
            }

            default:
                return ATOMS.atomNotImplemented;
        }
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}

    return enif_make_badarg(env);
}

