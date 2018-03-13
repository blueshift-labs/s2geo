#include "s2cap.h"
#include "s2latlng.h"
#include "s2cellid.h"
#include "s2regioncoverer.h"
#include "s2cellunion.h"

#include "nifpp_utils.h"
#include "s2geo_nif.h"
#include "nif_s2cap.h"
#include "s2latlngrect.h"

#include<functional>

using std::make_tuple;
using std::ref;

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)

ERL_NIF_TERM s2cap_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc < 1) // Need atleast one arg
        return enif_make_badarg(env);

    try{
        auto option = static_cast<S2CapConstructors>( nifpp::get<int>(env, nifpp::TERM(argv[0]) ) );
        switch( option ) {
            case S2CapConstructors::from_axis_height:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Point axis;
                double height;
                nifpp::get_throws(env, argv[1], axis);
                nifpp::get_throws(env, argv[2], height);

                S2Cap cap = S2Cap::FromAxisHeight(axis, height);
                nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
                return nifpp::make(env, s2cap_ptr);
            }

            case S2CapConstructors::from_axis_angle:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Point axis;
                S1Angle angle;
                nifpp::get_throws(env, argv[1], axis);
                nifpp::get_throws(env, argv[2], angle);

                S2Cap cap = S2Cap::FromAxisAngle(axis, angle);
                nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
                return nifpp::make(env, s2cap_ptr);
            }

            case S2CapConstructors::from_axis_area:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Point axis;
                double area;
                nifpp::get_throws(env, argv[1], axis);
                nifpp::get_throws(env, argv[2], area);

                S2Cap cap = S2Cap::FromAxisArea(axis, area);
                nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
                return nifpp::make(env, s2cap_ptr);
            }

            case S2CapConstructors::full:
            {
                CHECK_ARGS_LENGTH(env, argc, 1);

                S2Cap cap = S2Cap::Full();
                nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
                return nifpp::make(env, s2cap_ptr);
            }

            case S2CapConstructors::empty:
            {
                CHECK_ARGS_LENGTH(env, argc, 1);

                S2Cap cap = S2Cap::Empty();
                nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
                return nifpp::make(env, s2cap_ptr);
            }

            default:
                return ATOMS.atomError;
        }

    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}

    return enif_make_badarg(env);
}

ERL_NIF_TERM s2cap_methods(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if(argc < 2) // Need atleast one arg
        return enif_make_badarg(env);
    try{
        S2Cap *self;
        nifpp::get_throws(env, argv[0], self);

        auto option = static_cast<S2CapMethods>( nifpp::get<int>(env, nifpp::TERM(argv[1])) );
        switch( option ) {
            case S2CapMethods::clone:
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->Clone());

            case S2CapMethods::axis:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->axis());
            }
            case S2CapMethods::height:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->height());

            }
            case S2CapMethods::area:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->area());

            }
            case S2CapMethods::angle:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->angle());

            }
            case S2CapMethods::is_valid:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->is_valid());

            }
            case S2CapMethods::is_empty:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->is_empty());

            }
            case S2CapMethods::is_full:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->is_full());

            }
            case S2CapMethods::complement:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);

                S2Cap cap = self->Complement();
                nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
                return nifpp::make(env, s2cap_ptr);
            }
            case S2CapMethods::contains:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Cap *other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->Contains(*other));
            }
            case S2CapMethods::intersects:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Cap *other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->Intersects(*other));

            }
            case S2CapMethods::interior_intersects:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Cap *other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->InteriorIntersects(*other));

            }
            case S2CapMethods::interior_contains:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Point other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->InteriorContains(other));
            }
            case S2CapMethods::add_point:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Point other;
                nifpp::get_throws(env, argv[2], other);
                S2Cap *cap = self->Clone();
                cap -> AddPoint(other);

                nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(*cap);
                return nifpp::make(env, s2cap_ptr);
            }
            case S2CapMethods::add_cap:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Cap *other;
                nifpp::get_throws(env, argv[2], other);

                S2Cap *cap = self->Clone();
                cap->AddCap(*other);
                nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(*cap);
                return nifpp::make(env, s2cap_ptr);

            }
            case S2CapMethods::expanded:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S1Angle other;
                nifpp::get_throws(env, argv[2], other);

                S2Cap cap = self->Expanded(other);
                nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
                return nifpp::make(env, s2cap_ptr);
            }
            case S2CapMethods::get_cap_bound:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);

                S2Cap cap = self->GetCapBound();
                nifpp::resource_ptr<S2Cap> s2cap_ptr = nifpp::construct_resource<S2Cap>(cap);
                return nifpp::make(env, s2cap_ptr);
            }
            case S2CapMethods::get_rect_bound:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                S2LatLngRect s2latlngrect = self->GetRectBound();
                nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(s2latlngrect);
                return nifpp::make(env, s2rect);
            }

            case S2CapMethods::contains_s2cellid:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2CellId cellid;
                nifpp::get_throws(env, argv[2], cellid);
                S2Cell cell(cellid);

                return nifpp::make(env, self->Contains(cell));
            }
            case S2CapMethods::may_intersect_s2cellid:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2CellId cellid;
                nifpp::get_throws(env, argv[2], cellid);
                S2Cell cell(cellid);

                return nifpp::make(env, self->MayIntersect(cell));

            }
            case S2CapMethods::contains_s2point:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2Point point;
                nifpp::get_throws(env, argv[2], point);

                return nifpp::make(env, self->Contains(point));
            }

            default:
                return ATOMS.atomNotImplemented;
        }
    }
    catch(nifpp::badarg) {}
    catch(...){ return ATOMS.atomInternalError;}

    return enif_make_badarg(env);
}


ERL_NIF_TERM s2region_coverer_get_covering_for_s2cap(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if(argc < 2) // Need atleast one arg
        return enif_make_badarg(env);
    try{
        S2Cap *self;
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
