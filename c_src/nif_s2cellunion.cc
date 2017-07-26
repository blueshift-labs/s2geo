#include "s2cell.h"
#include "s2cellunion.h"
#include "s2latlngrect.h"
#include "s2regioncoverer.h"
#include "util/coding/coder.h"

#include "s2geo_nif.h"
#include "nifpp_utils.h"
#include "nif_s2cellunion.h"


using std::make_tuple;
using std::ref;

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)


ERL_NIF_TERM s2cellunion_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc < 1) // Need atleast one arg
        return enif_make_badarg(env);

    try{
        auto option = static_cast<S2CellUnionConstructors>( nifpp::get<int>(env, nifpp::TERM(argv[0]) ) );
        switch( option ) {
            case S2CellUnionConstructors::init_from_cellids:
            case S2CellUnionConstructors::init_from_uint64:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);

                std::vector<S2CellId> cellids;
                nifpp::get_throws(env, argv[1], cellids);

                nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
                S2CellUnion *cell_union = s2cellunion_ptr.get();
                cell_union->Init(cellids);
                cell_union->Normalize();

                return nifpp::make(env, s2cellunion_ptr);
            }

            case S2CellUnionConstructors::decode:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                std::string encoded;
                nifpp::get_throws(env, argv[1], encoded);

                Decoder decoder(encoded.c_str(), encoded.length());
                nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
                S2CellUnion *decoded = s2cellunion_ptr.get();
                decoded->Decode(&decoder);
                decoded->Normalize();

                return nifpp::make(env, s2cellunion_ptr);
            }

            case S2CellUnionConstructors::init_from_range:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto min_id = nifpp::get<S2CellId>(env, argv[1]);
                auto max_id = nifpp::get<S2CellId>(env, argv[2]);

                nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
                S2CellUnion *cell_union = s2cellunion_ptr.get();
                cell_union->InitFromRange(min_id, max_id);
                return nifpp::make(env, s2cellunion_ptr);
            }

            default:
                return ATOMS.atomError;
        }

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}

    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cellunion_methods(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if(argc < 2) // Need atleast one arg
        return enif_make_badarg(env);
    try{
        S2CellUnion *self;
        nifpp::get_throws(env, argv[0], self);

        auto option = static_cast<S2CellUnionMethods>( nifpp::get<int>(env, nifpp::TERM(argv[1])) );
        switch( option ) {
            case S2CellUnionMethods::num_cells:
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->num_cells());

            case S2CellUnionMethods::cell_id:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto i = nifpp::get<int>(env, argv[2]);
                return nifpp::make(env, self->cell_id(i));
            }

            case S2CellUnionMethods::cell_ids:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->cell_ids());
            }

            case S2CellUnionMethods::normalize:
                return ATOMS.atomNotImplemented;
            case S2CellUnionMethods::contains_s2cellid:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto cellid = nifpp::get<S2CellId>(env, argv[2]);
                return nifpp::make(env, self->Contains(cellid));
            }

            case S2CellUnionMethods::intersects_s2cellid:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto cellid = nifpp::get<S2CellId>(env, argv[2]);
                return nifpp::make(env, self->Intersects(cellid));
            }

            case S2CellUnionMethods::contains_s2cellunion:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2CellUnion *other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->Contains(other));
            }

            case S2CellUnionMethods::intersects_s2cellunion:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2CellUnion *other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->Intersects(other));
            }

            case S2CellUnionMethods::get_union:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2CellUnion *other;
                nifpp::get_throws(env, argv[2], other);

                nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
                S2CellUnion *cell_union = s2cellunion_ptr.get();
                cell_union->GetUnion(self, other);
                return nifpp::make(env, s2cellunion_ptr);
            }

            case S2CellUnionMethods::get_intersection:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2CellUnion *other;
                nifpp::get_throws(env, argv[2], other);

                nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
                S2CellUnion *cell_union = s2cellunion_ptr.get();
                cell_union->GetIntersection(self, other);
                return nifpp::make(env, s2cellunion_ptr);
            }

            case S2CellUnionMethods::get_difference:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2CellUnion *other;
                nifpp::get_throws(env, argv[2], other);

                nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
                S2CellUnion *cell_union = s2cellunion_ptr.get();
                cell_union->GetDifference(self, other);
                return nifpp::make(env, s2cellunion_ptr);
            }

            case S2CellUnionMethods::get_intersection_with_s2cellid:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto cellid = nifpp::get<S2CellId>(env, argv[2]);

                nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
                S2CellUnion *cell_union = s2cellunion_ptr.get();
                cell_union->GetIntersection(self, cellid);
                return nifpp::make(env, s2cellunion_ptr);
            }


            case S2CellUnionMethods::expand_level:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto expand_level = nifpp::get<int>(env, argv[2]);

                auto cloned = self->Clone();
                cloned->Expand(expand_level);
                nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
                S2CellUnion *cell_union = s2cellunion_ptr.get();
                auto cellids = cloned->cell_ids();
                cell_union->InitRawSwap(&cellids);

                return nifpp::make(env, s2cellunion_ptr);
            }


            case S2CellUnionMethods::expand_radius:
            {
                CHECK_ARGS_LENGTH(env, argc, 4);
                auto min_radius = nifpp::get<S1Angle>(env, argv[2]);
                auto max_level_diff = nifpp::get<int>(env, argv[3]);

                auto cloned = self->Clone();
                cloned->Expand(min_radius, max_level_diff);
                nifpp::resource_ptr<S2CellUnion> s2cellunion_ptr = nifpp::construct_resource<S2CellUnion>();
                S2CellUnion *cell_union = s2cellunion_ptr.get();
                auto cellids = cloned->cell_ids();
                cell_union->InitRawSwap(&cellids);

                return nifpp::make(env, s2cellunion_ptr);
            }


            case S2CellUnionMethods::leaf_cells_covered:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, static_cast<ErlNifUInt64>( self->LeafCellsCovered() ));
            }

            case S2CellUnionMethods::average_based_area:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->AverageBasedArea());
            }

            case S2CellUnionMethods::approx_area:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->ApproxArea());
            }

            case S2CellUnionMethods::exact_area:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->ExactArea());
            }

            case S2CellUnionMethods::get_rect_bound:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                S2LatLngRect constructed = self->GetRectBound();

                nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
                return nifpp::make(env, s2rect);
            }

            case S2CellUnionMethods::contains_s2cell:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto s2cellid = nifpp::get<S2CellId>(env, argv[2]);
                S2Cell cell(s2cellid);

                return nifpp::make(env, self->Contains(cell));
            }

            case S2CellUnionMethods::may_intersect_s2cell:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto s2cellid = nifpp::get<S2CellId>(env, argv[2]);
                S2Cell cell(s2cellid);

                return nifpp::make(env, self->MayIntersect(cell));
            }

            case S2CellUnionMethods::contains_s2point:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto s2point = nifpp::get<S2Point>(env, argv[2]);
                return nifpp::make(env, self->Contains(s2point));
            }

            case S2CellUnionMethods::encode:
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

ERL_NIF_TERM s2region_coverer_get_covering_for_s2cellunion(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if(argc < 3) // Need atleast one arg
        return enif_make_badarg(env);
    try{
        S2CellUnion *self;
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

