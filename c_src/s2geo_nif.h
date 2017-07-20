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
};

extern atoms ATOMS;
#endif
