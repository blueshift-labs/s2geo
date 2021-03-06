-module(s2cellid).

-include("s2cellid_internals.hrl").


%% API exports
-export([
    new/0,
    sentinel/0,
    new_from_lat_lng/1,
    new_from_face_pos_level/3,
    new_from_face_ij/3,
    level_begin/1,
    level_end/1,
    from_token/1,

    max_level/0,
    num_faces/0,
    id/1,

    is_valid/1,

    face/1,
    pos/1,
    level/1,
    get_size_ij/2,
    get_size_ij/1,
    get_size_st/1,
    get_size_st/2,

    is_leaf/1,
    is_face/1,
    range_min/1,
    range_max/1,

    parent/1,
    child_begin/1,
    child_end/1,
    next/1,
    prev/1,
    next_wrap/1,
    prev_wrap/1,
    lsb/1,

    to_token/1,
    to_string/1,
    to_point/1,
    to_lat_lng/1,
    to_face_ij_orientation/1,


    child_position/2,
    contains/2,
    intersects/2,
    parent/2,
    child/2,
    child_begin/2,
    child_end/2,
    advance/2,
    advance_wrap/2,

    get_edge_neighbors/1,
    get_vertex_neighbors/2,
    get_all_neighbors/2
    ]).

-compile({inline, [max_level/0, num_faces/0, id/1]}).

max_level() -> 30.

num_faces() -> 6.

id({s2cellid, Id}) -> Id.

%%====================================================================
%% API functions
%%====================================================================

new() ->
  {s2cellid, s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_NONE)}.

sentinel() ->
  {s2cellid, s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_SENTINEL)}.

new_from_lat_lng({Latitude, Longitude}) ->
    {s2cellid, s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_FROM_LAT_LNG, s2geo:to_float({Latitude, Longitude}) )}.

new_from_face_pos_level(Face, Pos, Level) when is_integer(Face), is_integer(Level) ->
    {s2cellid, s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_FROM_FACE_POS_LEVEL, Face, Pos, Level)}.

new_from_face_ij(Face, I, J) when is_integer(Face), is_integer(I), is_integer(J) ->
    {s2cellid, s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_FROM_FACE_IJ, Face, I, J)}.

level_begin(Level) when is_integer(Level) ->
    {s2cellid, s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_BEGIN, Level)}.

level_end(Level) when is_integer(Level) ->
    {s2cellid, s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_END, Level)}.

from_token(Token) ->
    {s2cellid, s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_FROM_TOKEN, Token)}.


is_valid({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_IS_VALID).

face({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_FACE).

pos({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_POS).

level({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_LEVEL).


get_size_ij({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_GET_SIZE_IJ);
get_size_ij(Level) ->
  s2geo_nif:s2cellid_get_size_ij(Level).

get_size_ij({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_GET_SIZE_IJ, Level).


get_size_st({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_GET_SIZE_ST);
get_size_st(Level) ->
  s2geo_nif:s2cellid_get_size_st(Level).

get_size_st({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_GET_SIZE_ST, Level).


is_leaf({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_IS_LEAF).

is_face({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_IS_FACE).

range_min({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_RANGE_MIN)}.

range_max({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_RANGE_MAX)}.

parent({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_PARENT)}.

child_begin({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_CHILD_BEGIN)}.

child_end({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_CHILD_END)}.

next({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_NEXT)}.

prev({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_PREV)}.

next_wrap({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_NEXT_WRAP)}.

prev_wrap({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_PREV_WRAP)}.

lsb({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_LSB);
lsb(Level) ->
    s2geo_nif:s2cellid_lsb_for_level(Level).

to_token({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_TO_TOKEN).

to_string({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_TO_STRING).

to_point({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2point, s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_TO_POINT)}.

to_lat_lng({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_TO_LAT_LNG).

to_face_ij_orientation({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_to_face_ij_orientation(S2CellId).

child_position({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_CHILD_POSITION, Level).

contains({s2cellid, S2CellId}, {s2cellid, OtherS2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_CONTAINS, OtherS2CellId).

intersects({s2cellid, S2CellId}, {s2cellid, OtherS2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_INTERSECTS, OtherS2CellId).

parent({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_PARENT, Level)}.

child({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_CHILD, Level)}.

child_begin({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_CHILD_BEGIN, Level)}.

child_end({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_CHILD_END, Level)}.

advance({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_ADVANCE, Level)}.

advance_wrap({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_ADVANCE_WRAP, Level)}.

get_edge_neighbors({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_get_edge_neighbors(S2CellId).

get_vertex_neighbors({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_get_vertex_neighbors(S2CellId, Level).

get_all_neighbors({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_get_all_neighbors(S2CellId, Level).
