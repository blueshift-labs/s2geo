-module(s2cellid).

-include("s2cellid_internals.hrl").

%% TODO: Implement thesse:
% s2cellid_from_s2point/1,
% s2cellid_get_center_st/1,
% s2cellid_get_bound_st/1,
% s2cellid_get_center_uv/1,
% s2cellid_get_bound_uv/1,
% s2cellid_expanded_by_distance_uv/2,
% s2cellid_get_center_si_ti/1,
% s2cellid_distance_from_begin/1,
% s2cellid_maximum_tile/2,
% s2cellid_get_common_ancestor_level/2,
% s2cellid_ij_level_to_bound_uv/1,

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
    get_size_ij/1,
    get_size_st/1,

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
  {s2cellid, s2geo_nif:s2cellid_none()}.

sentinel() ->
  {s2cellid, s2geo_nif:s2cellid_sentinel()}.

new_from_lat_lng({Latitude, Longitude}) ->
    {s2cellid, s2geo_nif:s2cellid_from_lat_lng(s2geo:to_float({Latitude, Longitude}) )}.

new_from_face(Face) when is_integer(Face) ->
    {s2cellid, s2geo_nif:s2cellid_from_face(Face)}.

new_from_face_pos_level(Face, Pos, Level) when is_integer(Face), is_integer(Level) ->
    {s2cellid, s2geo_nif:s2cellid_from_face_pos_level(Face, Pos, Level)}.

new_from_face_ij(Face, I, J) when is_integer(Face), is_integer(I), is_integer(J) ->
    {s2cellid, s2geo_nif:s2cellid_from_face_ij(Face, I, J)}.

level_begin(Level) when is_integer(Level) ->
    {s2cellid, s2geo_nif:s2cellid_begin(Level)}.

level_end(Level) when is_integer(Level) ->
    {s2cellid, s2geo_nif:s2cellid_end(Level)}.

from_token(Token) ->
    {s2cellid, s2geo_nif:s2cellid_from_token(Token)}.


is_valid({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_is_valid(S2CellId).

face({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_face(S2CellId).

pos({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_pos(S2CellId).

level({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_level(S2CellId).


get_size_ij({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_get_size_ij(S2CellId);
get_size_ij(Level) ->
  s2geo_nif:s2cellid_get_size_ij_level(Level).

get_size_st({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_get_size_st(S2CellId);
get_size_st(Level) ->
  s2geo_nif:s2cellid_get_size_st_level(Level).

is_leaf({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_is_leaf(S2CellId).

is_face({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_is_face(S2CellId).

range_min({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_range_min(S2CellId)}.

range_max({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_range_max(S2CellId)}.

parent({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_parent(S2CellId)}.

parent({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_parent(S2CellId, Level)}.

child({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_child(S2CellId, Level)}.

child_begin({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_child_begin(S2CellId)}.

child_begin({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_child_begin(S2CellId, Level)}.

child_end({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_child_end(S2CellId)}.

child_end({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_child_end(S2CellId, Level)}.

next({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_next(S2CellId)}.

prev({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_prev(S2CellId)}.

next_wrap({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_next_wrap(S2CellId)}.

prev_wrap({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_prev_wrap(S2CellId)}.

lsb({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_lsb(S2CellId);
lsb(Level) ->
    s2geo_nif:s2cellid_lsb_for_level(Level).

to_token({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_to_token(S2CellId).

to_string({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_to_string(S2CellId).

to_point({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2point, s2geo_nif:s2cellid_to_point(S2CellId)}.

to_point_raw({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    {s2point, s2geo_nif:s2cellid_to_point_raw(S2CellId)}.

to_lat_lng({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_to_lat_lng(S2CellId).

to_face_ij_orientation({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_to_face_ij_orientation(S2CellId).

child_position({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_child_position(S2CellId, Level).

contains({s2cellid, S2CellId}, {s2cellid, OtherS2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_contains(S2CellId, OtherS2CellId).

intersects({s2cellid, S2CellId}, {s2cellid, OtherS2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_intersects(S2CellId, OtherS2CellId).

advance({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_advance(S2CellId, Level)}.

advance_wrap({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    {s2cellid, s2geo_nif:s2cellid_advance_wrap(S2CellId, Level)}.

get_edge_neighbors({s2cellid, S2CellId}) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_get_edge_neighbors(S2CellId).

get_vertex_neighbors({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_get_vertex_neighbors(S2CellId, Level).

get_all_neighbors({s2cellid, S2CellId}, Level) when is_integer(S2CellId) ->
    s2geo_nif:s2cellid_get_all_neighbors(S2CellId, Level).
