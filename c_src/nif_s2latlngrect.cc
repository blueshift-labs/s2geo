#include "s2cell.h"
#include "s2latlngrect.h"
#include "util/coding/coder.h"

#include "s2geo_nif.h"
#include "nifpp_utils.h"
#include "nif_s2latlngrect.h"


using std::make_tuple;
using std::ref;

#define CHECK_ARGS_LENGTH(env, arg, len) \
    if(arg != len) \
        return enif_make_badarg(env)


ERL_NIF_TERM s2latlngrect_constructor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc < 1) // Need atleast one arg
        return enif_make_badarg(env);

    try{
        auto option = static_cast<S2LatLngRectConstructors>( nifpp::get<int>(env, nifpp::TERM(argv[0]) ) );
        switch( option ) {
            case S2LatLngRectConstructors::from_lat_lng_degree:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);

                auto low_lat_lng = nifpp::get<S2LatLng>(env, argv[1]);
                auto hi_lat_lng = nifpp::get<S2LatLng>(env, argv[2]);
                nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(low_lat_lng, hi_lat_lng);
                return nifpp::make(env, s2rect);
            }
            case S2LatLngRectConstructors::from_lat_lng:
                return ATOMS.atomNotImplemented;

            case S2LatLngRectConstructors::empty:
            {
                CHECK_ARGS_LENGTH(env, argc, 1);
                S2LatLngRect constructed = S2LatLngRect::Empty();

                nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
                return nifpp::make(env, s2rect);
            }

            case S2LatLngRectConstructors::full:
            {
                CHECK_ARGS_LENGTH(env, argc, 1);
                S2LatLngRect constructed = S2LatLngRect::Full();

                nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
                return nifpp::make(env, s2rect);
            }

            case S2LatLngRectConstructors::from_center_size:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);

                auto center_lat_lng = nifpp::get<S2LatLng>(env, argv[1]);
                auto size_lat_lng = nifpp::get<S2LatLng>(env, argv[2]);
                S2LatLngRect constructed = S2LatLngRect::FromCenterSize(center_lat_lng, size_lat_lng);

                nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
                return nifpp::make(env, s2rect);
            }

            case S2LatLngRectConstructors::from_point:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);

                auto p1_lat_lng = nifpp::get<S2LatLng>(env, argv[1]);
                S2LatLngRect constructed = S2LatLngRect::FromPoint(p1_lat_lng);

                nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
                return nifpp::make(env, s2rect);
            }

            case S2LatLngRectConstructors::from_point_pair:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);

                auto p1_lat_lng = nifpp::get<S2LatLng>(env, argv[1]);
                auto p2_lat_lng = nifpp::get<S2LatLng>(env, argv[2]);

                S2LatLngRect constructed = S2LatLngRect::FromPointPair(p1_lat_lng, p2_lat_lng);

                nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
                return nifpp::make(env, s2rect);
            }

            case S2LatLngRectConstructors::decode:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                std::string encoded;
                nifpp::get_throws(env, argv[1], encoded);

                Decoder decoder(encoded.c_str(), encoded.length());
                S2LatLngRect decoded;
                decoded.Decode(&decoder);

                nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(decoded);
                return nifpp::make(env, s2rect);
            }

            default:
                return ATOMS.atomError;
        }

    }
    catch(nifpp::badarg) {}
    return enif_make_badarg(env);
}

ERL_NIF_TERM s2latlngrect_methods(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if(argc < 2) // Need atleast one arg
        return enif_make_badarg(env);
    try{
        S2LatLngRect *self;
        nifpp::get_throws(env, argv[0], self);

        auto option = static_cast<S2LatLngRectMethods>( nifpp::get<int>(env, nifpp::TERM(argv[1])) );
        switch( option ) {
            case S2LatLngRectMethods::lat_lo:
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->lat_lo());

            case S2LatLngRectMethods::lat_hi:
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->lat_hi());

            case S2LatLngRectMethods::lng_lo:
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->lng_lo());

            case S2LatLngRectMethods::lng_hi:
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->lng_hi());

            case S2LatLngRectMethods::lat:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->lat());
            }

            case S2LatLngRectMethods::lng:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->lng());
            }

            case S2LatLngRectMethods::lo:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->lo());
            }

            case S2LatLngRectMethods::hi:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->hi());
            }

            case S2LatLngRectMethods::is_valid:
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->is_valid());

            case S2LatLngRectMethods::is_empty:
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->is_empty());

            case S2LatLngRectMethods::is_full:
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->is_full());

            case S2LatLngRectMethods::is_point:
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->is_point());

            case S2LatLngRectMethods::is_inverted:
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->is_inverted());

            case S2LatLngRectMethods::get_vertex:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                S2LatLng coords[4];
                coords[0] = self->GetVertex(0);
                coords[1] = self->GetVertex(1);
                coords[2] = self->GetVertex(2);
                coords[3] = self->GetVertex(3);

                return nifpp::make(env, coords);
            }

            case S2LatLngRectMethods::get_center:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->GetCenter());
            }

            case S2LatLngRectMethods::get_size:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->GetSize());
            }

            case S2LatLngRectMethods::area:
            {
                CHECK_ARGS_LENGTH(env, argc, 2);
                return nifpp::make(env, self->Area());
            }


            // Containment

            case S2LatLngRectMethods::contains_s2latlng:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto s2latlng = nifpp::get<S2LatLng>(env, argv[2]);
                return nifpp::make(env, self->Contains(s2latlng));
            }

            case S2LatLngRectMethods::interior_contains_s2point:
                return ATOMS.atomNotImplemented;

            case S2LatLngRectMethods::interior_contains_s2latlng:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto s2latlng = nifpp::get<S2LatLng>(env, argv[2]);

                return nifpp::make(env, self->InteriorContains(s2latlng));
            }

            case S2LatLngRectMethods::contains_s2latlngrect:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLngRect *other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->Contains(*other));
            }


            case S2LatLngRectMethods::interior_contains_s2latlngrect:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLngRect *other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->InteriorContains(*other));
            }

            case S2LatLngRectMethods::intersects_s2latlngrect:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLngRect *other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->Intersects(*other));
            }

            case S2LatLngRectMethods::intersects_s2cell_with_id:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);

                auto value = nifpp::get<ErlNifUInt64>(env, nifpp::TERM(argv[2]));
                auto s2cellid = S2CellId(static_cast<uint64>(value));
                S2Cell cell(s2cellid);

                return nifpp::make(env, self->Intersects(cell));
            }

            case S2LatLngRectMethods::interior_intersects_s2latlngrect:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLngRect *other;
                nifpp::get_throws(env, argv[2], other);
                return nifpp::make(env, self->InteriorIntersects(*other));
            }


            case S2LatLngRectMethods::contains_s2cell_with_id:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto s2cellid = nifpp::get<S2CellId>(env, nifpp::TERM(argv[2]));
                S2Cell cell(s2cellid);
                return nifpp::make(env, self->Contains(cell));
            }

            case S2LatLngRectMethods::contains_s2point:
                return ATOMS.atomNotImplemented;

            case S2LatLngRectMethods::may_intersect_s2cell_with_id:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto s2cellid = nifpp::get<S2CellId>(env, nifpp::TERM(argv[2]));
                S2Cell cell(s2cellid);
                return nifpp::make(env, self->MayIntersect(cell));
            }

            case S2LatLngRectMethods::expanded:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto s2latlng = nifpp::get<S2LatLng>(env, argv[2]);

                S2LatLngRect constructed = self->Expanded(s2latlng);
                nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
                return nifpp::make(env, s2rect);
            }

            case S2LatLngRectMethods::Union:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLngRect *other;
                nifpp::get_throws(env, argv[2], other);

                S2LatLngRect constructed = self->Union(*other);
                nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
                return nifpp::make(env, s2rect);
            }

            case S2LatLngRectMethods::intersection:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLngRect *other;
                nifpp::get_throws(env, argv[2], other);

                S2LatLngRect constructed = self->Intersection(*other);
                nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
                return nifpp::make(env, s2rect);
            }

            case S2LatLngRectMethods::convolve_with_cap:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S1Angle arg = nifpp::get<S1Angle>(env, nifpp::TERM(argv[2]));

                S2LatLngRect constructed = self->ConvolveWithCap(arg);
                nifpp::resource_ptr<S2LatLngRect> s2rect = nifpp::construct_resource<S2LatLngRect>(constructed);
                return nifpp::make(env, s2rect);
            }

            case S2LatLngRectMethods::add_point_s2latlng:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto s2latlng = nifpp::get<S2LatLng>(env, argv[2]);

                S2LatLngRect *cloned = self->Clone();
                cloned->AddPoint(s2latlng);

                nifpp::resource_ptr<S2LatLngRect> s2rect_resource = nifpp::construct_resource<S2LatLngRect>(*cloned);
                return nifpp::make(env, s2rect_resource);
            }

            case S2LatLngRectMethods::add_point:
                return ATOMS.atomNotImplemented;

            case S2LatLngRectMethods::get_distance:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLngRect *other;
                nifpp::get_throws(env, argv[2], other);

                S1Angle distance = self->GetDistance(*other);
                return nifpp::make(env, distance.radians());
            }

            case S2LatLngRectMethods::get_distance_s2latlng:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                auto s2latlng = nifpp::get<S2LatLng>(env, argv[2]);

                S1Angle distance = self->GetDistance(s2latlng);
                return nifpp::make(env, distance.radians());
            }

            case S2LatLngRectMethods::get_directed_hausdorff_distance:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLngRect *other;
                nifpp::get_throws(env, argv[2], other);

                S1Angle distance = self->GetDirectedHausdorffDistance(*other);
                return nifpp::make(env, distance.radians());
            }

            case S2LatLngRectMethods::get_hausdorff_distance:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLngRect *other;
                nifpp::get_throws(env, argv[2], other);

                S1Angle distance = self->GetHausdorffDistance(*other);
                return nifpp::make(env, distance.radians());
            }

            case S2LatLngRectMethods::equal:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLngRect *other;
                nifpp::get_throws(env, argv[2], other);

                return nifpp::make(env, (*self == *other));
            }

            case S2LatLngRectMethods::not_equal:
            {
                CHECK_ARGS_LENGTH(env, argc, 3);
                S2LatLngRect *other;
                nifpp::get_throws(env, argv[2], other);

                return nifpp::make(env, (*self != *other));
            }

            case S2LatLngRectMethods::approx_equals:
            {
                if (argc > 4) {
                    return enif_make_badarg(env);
                }
                S2LatLngRect *other;
                nifpp::get_throws(env, argv[2], other);

                double max_error = 1e-15;
                if (argc == 4) {
                    max_error = nifpp::get<double>(env, nifpp::TERM(argv[3]));
                }

                return nifpp::make(env, self->ApproxEquals(*other, max_error));
            }

            case S2LatLngRectMethods::get_cap_bound:
            case S2LatLngRectMethods::get_rect_bound:
                return ATOMS.atomNotImplemented;

            case S2LatLngRectMethods::encode:
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
    return enif_make_badarg(env);
}


