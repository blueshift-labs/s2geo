#ifndef S2GEO_C_SRC_NIFPP_UTILS_H
#define S2GEO_C_SRC_NIFPP_UTILS_H

#include "erl_nif.h"

#include "s2/s1angle.h"
#include "s2/r1interval.h"
#include "s2/s1interval.h"
#include "s2/s2region.h"
#include "s2/s2latlng.h"
#include "s2/s2cell_id.h"
#include "s2/s2cell.h"
#include "s2/util/math/vector.h"
#include "s2/s1chord_angle.h"


namespace nifpp
{
struct TERM;

nifpp::TERM make(ErlNifEnv *env, const Vector2_d &var);

int get(ErlNifEnv *env, ERL_NIF_TERM term, S1Angle &var);
nifpp::TERM make(ErlNifEnv *env, const S1Angle &var);

int get(ErlNifEnv *env, ERL_NIF_TERM term, R1Interval &var);
nifpp::TERM make(ErlNifEnv *env, const R1Interval &var);

int get(ErlNifEnv *env, ERL_NIF_TERM term, S1Interval &var);
nifpp::TERM make(ErlNifEnv *env, const S1Interval &var);

int get(ErlNifEnv *env, ERL_NIF_TERM term, S2Point &var);
nifpp::TERM make(ErlNifEnv *env, const S2Point &var);

//int get(ErlNifEnv *env, ERL_NIF_TERM term, R2Point &var);
//nifpp::TERM make(ErlNifEnv *env, const R2Point &var);

int get(ErlNifEnv *env, ERL_NIF_TERM term, R2Rect &var);
nifpp::TERM make(ErlNifEnv *env, const R2Rect &var);

int get(ErlNifEnv *env, ERL_NIF_TERM term, S2LatLng &var);
nifpp::TERM make(ErlNifEnv *env, const S2LatLng &var);

int get(ErlNifEnv *env, ERL_NIF_TERM term, S2CellId &var);
nifpp::TERM make(ErlNifEnv *env, const S2CellId &var);


int get(ErlNifEnv *env, ERL_NIF_TERM term, S1ChordAngle &var);
nifpp::TERM make(ErlNifEnv *env, const S1ChordAngle &var);

} // namespace nifpp

#include "nifpp.h"

double get_double_from_term(ErlNifEnv *env, ERL_NIF_TERM term);

#endif
