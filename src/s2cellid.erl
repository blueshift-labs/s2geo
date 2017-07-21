-module(s2cellid).

-include("s2cellid_internals.hrl").

%% API exports
-export([
    s2cellid_get_size_ij/1,
    s2cellid_get_size_st/1,
    s2cellid_lsb_for_level/1,

    new/0,
    new_sentinel/0,
    new_from_lat_lng_degrees/2,
    new_from_face_pos_level/3,
    new_from_face_ij/3,
    level_begin/1,
    level_end/1,
    from_token/1,

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
    to_lat_lng/1,

    child_position/2,
    contains/2,
    intersects/2,
    parent/2,
    child/2,
    child_begin/2,
    child_end/2,
    advance/2,
    advance_wrap/2
    ]).

%-on_load(init/0).

-define(APPNAME, s2geo_nif).
-define(LIBNAME, s2geo).

%%====================================================================
%% API functions
%%====================================================================

s2cellid_get_size_ij(Level) ->
  s2geo_nif:s2cellid_get_size_ij(Level).

s2cellid_get_size_st(Level) ->
  s2geo_nif:s2cellid_get_size_st(Level).

s2cellid_lsb_for_level(Level) ->
  s2geo_nif:s2cellid_lsb_for_level(Level).

new() ->
  s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_NONE).

new_sentinel() ->
  s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_SENTINEL).

new_from_lat_lng_degrees(Latitude, Longitude) ->
    s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_FROM_LAT_LNG, Latitude, Longitude).

new_from_face_pos_level(Face, Pos, Level) when is_integer(Face), is_integer(Level) ->
    s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_FROM_FACE_POS_LEVEL, Face, Pos, Level).

new_from_face_ij(Face, I, J) when is_integer(Face), is_integer(I), is_integer(J) ->
    s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_FROM_FACE_IJ, Face, I, J).

level_begin(Level) when is_integer(Level) ->
    s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_BEGIN, Level).

level_end(Level) when is_integer(Level) ->
    s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_END, Level).

from_token(Token) when is_binary(Token) ->
    s2geo_nif:s2cellid_constructor(?S2CELLID_CONSTRUCTOR_FROM_TOKEN, Token).


is_valid(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_IS_VALID).

face(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_FACE).

pos(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_POS).

level(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_LEVEL).

get_size_ij(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_GET_SIZE_IJ).
get_size_ij(S2CellId, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_GET_SIZE_IJ, Level).

get_size_st(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_GET_SIZE_ST).
get_size_st(S2CellId, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_GET_SIZE_ST, Level).


is_leaf(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_IS_LEAF).

is_face(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_IS_FACE).

range_min(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_RANGE_MIN).

range_max(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_RANGE_MAX).

parent(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_PARENT).

child_begin(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_CHILD_BEGIN).

child_end(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_CHILD_END).

next(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_NEXT).

prev(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_PREV).

next_wrap(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_NEXT_WRAP).

prev_wrap(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_PREV_WRAP).

lsb(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_LSB).

to_token(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_TO_TOKEN).

to_string(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_TO_STRING).

to_lat_lng(S2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_zero_args_fn(S2CellId, ?S2CELLID_0_ARGS_TO_LAT_LNG).

child_position(S2CellId, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_CHILD_POSITION, Level).

contains(S2CellId, OtherS2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_CONTAINS, OtherS2CellId).

intersects(S2CellId, OtherS2CellId) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_INTERSECTS, OtherS2CellId).

parent(S2CellId, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_PARENT, Level).

child(S2CellId, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_CHILD, Level).

child_begin(S2CellId, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_CHILD_BEGIN, Level).

child_end(S2CellId, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_CHILD_END, Level).

advance(S2CellId, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_ADVANCE, Level).

advance_wrap(S2CellId, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_one_arg_fn(S2CellId, ?S2CELLID_1_ARG_ADVANCE_WRAP, Level).
