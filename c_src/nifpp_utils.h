#ifndef S2GEO_C_SRC_NIFPP_UTILS_H
#define S2GEO_C_SRC_NIFPP_UTILS_H

#include "erl_nif.h"

#include "s1angle.h"
#include "r1interval.h"
#include "s1interval.h"
#include "s2region.h"
#include "s2latlng.h"
#include "s2cellid.h"
#include "s2cell.h"
#include "util/math/vector2.h"


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

int get(ErlNifEnv *env, ERL_NIF_TERM term, S2LatLng &var);
nifpp::TERM make(ErlNifEnv *env, const S2LatLng &var);

int get(ErlNifEnv *env, ERL_NIF_TERM term, S2CellId &var);
nifpp::TERM make(ErlNifEnv *env, const S2CellId &var);

} // namespace nifpp

#include "nifpp.h"

double get_double_from_term(ErlNifEnv *env, ERL_NIF_TERM term);

#endif
