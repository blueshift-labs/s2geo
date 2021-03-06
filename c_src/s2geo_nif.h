#ifndef S2GEO_C_SRC_S2GEO_H
#define S2GEO_C_SRC_S2GEO_H

#include "erl_nif.h"

struct atoms
{
    ERL_NIF_TERM atomOk;
    ERL_NIF_TERM atomError;
    ERL_NIF_TERM atomTrue;
    ERL_NIF_TERM atomFalse;
    ERL_NIF_TERM atomNull;
    ERL_NIF_TERM atomBadArg;
    ERL_NIF_TERM atomOptions;
    ERL_NIF_TERM atomNotImplemented;
    ERL_NIF_TERM atomInternalError;
};

extern atoms ATOMS;

enum class S2RegionCoveringType {
  cellid_covering                 = 1,
  cellid_interior_covering        = 2,
  cell_union_covering             = 3,
  interior_cell_union_covering    = 4,
};
#endif
