-module(test_s2cellid).
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

face_bits() -> 3.

to_float_test() ->
  {10.0, 10.0} = s2geo:to_float({10, 10}),
  {10.0, 10.0} = s2geo:to_float({10, 10.0}),
  {10.0, 10.0} = s2geo:to_float({10.0, 10}),
  {10.0, 10.0} = s2geo:to_float({10.0, 10.0}).

create_s2cell_test() ->
    {s2cellid, 9263400817301046493} = s2cellid:new_from_lat_lng({37.2271363, -121.9877517}),

    {s2cellid,9264334513346095871} = s2cellid:new_from_lat_lng({37.0, -121.0}),
    {s2cellid,9264334513346095871} = s2cellid:new_from_lat_lng({37, -121}),
    ok.

default_constructor_test() ->
    {s2cellid, 0} = s2cellid:new(),
    ?assert(s2cellid:is_valid({s2cellid, 0}) =:= false).

s2cell_is_valid_test() ->
    true = s2cellid:is_valid({s2cellid, 1}),
    false = s2cellid:is_valid({s2cellid, 2}),
    true = s2cellid:is_valid({s2cellid, 3}),
    true = s2cellid:is_valid({s2cellid, 4}),
    true = s2cellid:is_valid({s2cellid, 5}),
    false = s2cellid:is_valid({s2cellid, 6}),
    true = s2cellid:is_valid({s2cellid, 7}),
    false = s2cellid:is_valid({s2cellid, 8}),
    ok.

face_definitions_test() ->
    0 = s2cellid:face(s2cellid:new_from_lat_lng({0, 0})),
    1 = s2cellid:face(s2cellid:new_from_lat_lng({0, 90})),
    2 = s2cellid:face(s2cellid:new_from_lat_lng({90, 0})),
    3 = s2cellid:face(s2cellid:new_from_lat_lng({0, 180})),
    4 = s2cellid:face(s2cellid:new_from_lat_lng({0, -90})),
    5 = s2cellid:face(s2cellid:new_from_lat_lng({-90, 0})).


parent_child_relationships_test() ->
     {s2cellid, CellId} = s2cellid:new_from_face_pos_level(3,
                                          16#12345678, % = 16#12345678
                                          s2cellid:max_level() - 4),
      S2Cellid = {s2cellid, CellId},
      true = s2cellid:is_valid(S2Cellid),
      3 = s2cellid:face(S2Cellid),
      16#12345700 = s2cellid:pos(S2Cellid),
      ?assert(s2cellid:level(S2Cellid) =:= s2cellid:max_level() - 4),
      false = s2cellid:is_leaf(S2Cellid),

      16#12345610 = s2cellid:pos(s2cellid:child_begin(S2Cellid,
                                 s2cellid:level(S2Cellid) + 2)),
      16#12345640 = s2cellid:pos(s2cellid:child_begin(S2Cellid)),
      16#12345400 = s2cellid:pos(s2cellid:parent(S2Cellid)),
      16#12345000 = s2cellid:pos(s2cellid:parent(S2Cellid,
                                 s2cellid:level(S2Cellid) - 2)),


      % Check ordering of children relative to parents.
      {s2cellid, CellIdChildBegin} = s2cellid:child_begin(S2Cellid),
      ?assert(CellIdChildBegin < CellId),
      {s2cellid, CellIdChildEnd} = s2cellid:child_end(S2Cellid),
      ?assert(CellIdChildEnd > CellId),
      ?assert(s2cellid:child_end(S2Cellid) == s2cellid:next(
                                                    s2cellid:next(
                                                        s2cellid:next(
                                                            s2cellid:next(
                                                                s2cellid:child_begin(S2Cellid))))) ),
      ?assert(s2cellid:child_begin(S2Cellid, s2cellid:max_level()) == s2cellid:range_min(S2Cellid)),
      ?assert(s2cellid:child_end(S2Cellid, s2cellid:max_level()) == s2cellid:next(s2cellid:range_max(S2Cellid))),
      ?assert(2*s2cellid:id(S2Cellid) == s2cellid:id(s2cellid:range_min(S2Cellid)) +
                                            s2cellid:id(s2cellid:range_max(S2Cellid)) ).

wrapping_test() ->
    ?assert(s2cellid:prev_wrap(s2cellid:level_begin(0)) == s2cellid:prev(s2cellid:level_end(0))),

    ?assert(s2cellid:prev_wrap(
                    s2cellid:level_begin(
                            s2cellid:max_level()))
            ==
            s2cellid:new_from_face_pos_level(5,
                                         16#ffffffffffffffff bsr face_bits(),
                                         s2cellid:max_level()) ),

    ?assert(s2cellid:advance_wrap(
                    s2cellid:level_begin(
                            s2cellid:max_level()), -1)
            ==
            s2cellid:new_from_face_pos_level(5,
                                         16#ffffffffffffffff bsr face_bits(),
                                         s2cellid:max_level()) ),

    ?assert(s2cellid:advance_wrap(
                                  s2cellid:advance(
                                        s2cellid:level_end(4),
                                        -1),
                                  1)
            ==
            s2cellid:level_begin(4)),

    ?assert(s2cellid:advance_wrap(
                                  s2cellid:advance(
                                        s2cellid:level_end(s2cellid:max_level()),
                                        -1),
                                  1)
            ==
            s2cellid:new_from_face_pos_level(0, 0, s2cellid:max_level())),


    ?assert(s2cellid:next_wrap(s2cellid:prev(s2cellid:level_end(4))) == s2cellid:level_begin(4)),

    ?assert(s2cellid:next_wrap(s2cellid:prev(s2cellid:level_end(s2cellid:max_level()))) == s2cellid:new_from_face_pos_level(0, 0, s2cellid:max_level())).



advance_test() ->

    ?assert(s2cellid:advance(s2cellid:level_begin(0), 7) == s2cellid:level_end(0)),
    ?assert(s2cellid:advance(s2cellid:level_begin(0), 12) == s2cellid:level_end(0)),
    ?assert(s2cellid:advance(s2cellid:level_end(0), -7) == s2cellid:level_begin(0)),
    ?assert(s2cellid:advance(s2cellid:level_end(0), -12000000) == s2cellid:level_begin(0)),

    Cellid = s2cellid:new_from_face_pos_level(3, 16#12345678,
                                         s2cellid:max_level() - 4),
    NumLevel5Cells = 6 bsl (2 * 5),
    ?assert(s2cellid:advance(s2cellid:level_begin(5), 500)
            ==
            s2cellid:advance(s2cellid:level_end(5), 500 - NumLevel5Cells)),

    ?assert(s2cellid:advance(s2cellid:child_begin(Cellid, s2cellid:max_level()), 256)
            ==
            s2cellid:child_begin(s2cellid:next(Cellid), s2cellid:max_level())),

    ?assert(s2cellid:advance(
                s2cellid:new_from_face_pos_level(1, 0, s2cellid:max_level()),
                4 bsl (2* s2cellid:max_level()) )
            ==
            s2cellid:new_from_face_pos_level(5, 0, s2cellid:max_level()) ),

    % Check basic properties of advance_wrap().
    ?assert(s2cellid:advance_wrap(s2cellid:level_begin(0), 7)
            ==
            s2cellid:new_from_face_pos_level(1, 0, 0)),
    ?assert(s2cellid:advance_wrap(s2cellid:level_begin(0), 12)
            ==
            s2cellid:level_begin(0)),

    ?assert(s2cellid:advance_wrap(s2cellid:new_from_face_pos_level(5, 0, 0), -7)
            ==
            s2cellid:new_from_face_pos_level(4, 0, 0)),

    ?assert(s2cellid:advance_wrap(s2cellid:level_begin(0), -12000000)
            ==
            s2cellid:level_begin(0)),

    ?assert(s2cellid:advance_wrap(s2cellid:level_begin(5), 6644)
            ==
            s2cellid:advance_wrap(s2cellid:level_begin(5), -11788)),

    ?assert(s2cellid:advance_wrap(s2cellid:child_begin(Cellid, s2cellid:max_level()), 256)
            ==
            s2cellid:child_begin(s2cellid:next(Cellid), s2cellid:max_level())),

    ?assert(s2cellid:advance_wrap(
                s2cellid:new_from_face_pos_level(5, 0, s2cellid:max_level()),
                2 bsl (2 * s2cellid:max_level()) )
            ==
            s2cellid:new_from_face_pos_level(1, 0, s2cellid:max_level()) ).

inverse_iterations() -> 200000.

get_random_cell_id() ->
  Level = rand:uniform(s2cellid:max_level() + 1),
  get_random_cell_id(Level).

get_random_cell_id(Level) ->
        Face = rand:uniform(s2cellid:num_faces()),
        Pos = (rand:uniform(16#ffffffffffffffff) band
               ((1 bsl (2 * s2cellid:max_level() )) - 1)),
        s2cellid:new_from_face_pos_level(Face, Pos, Level).

inverse_random_cellid_check(_Elem) ->
    Cellid = get_random_cell_id(s2cellid:max_level()),
    inverse_random_cellid_check_valid(s2cellid:is_valid(Cellid), Cellid).

inverse_random_cellid_check_valid(false, _) -> ok;
inverse_random_cellid_check_valid(true, Cellid) ->
    ?assert(s2cellid:is_leaf(Cellid) == true),
    ?assert(s2cellid:level(Cellid) == s2cellid:max_level()),
    Center = s2cellid:to_lat_lng(Cellid),
    ?assert(s2cellid:new_from_lat_lng(Center) == Cellid).

inverse_test() ->
  lists:foreach( fun(Elem) -> inverse_random_cellid_check(Elem) end, lists:seq(1, 20000)).

tokens_random_cellid_check(_Elem) ->
    Cellid = get_random_cell_id(),
    tokens_random_cellid_check_valid(s2cellid:is_valid(Cellid), Cellid).

tokens_random_cellid_check_valid(false, _) -> ok;
tokens_random_cellid_check_valid(true, Cellid) ->
    Token = s2cellid:to_token(Cellid),
    ?assert(length(Token) < 17),
    ?assert(s2cellid:from_token(Token) == Cellid).

tokens_test() ->
  lists:foreach( fun(Elem) -> tokens_random_cellid_check(Elem) end, lists:seq(1, 20000)).


-define(SWAP_MASK, 16#01).
-define(INVERT_MASK, 16#02).

-define(POS_TO_ORIENTATION, {?SWAP_MASK, 0, 0, ?INVERT_MASK bor ?SWAP_MASK}).

pos_to_orientation(Pos) -> element(Pos + 1, ?POS_TO_ORIENTATION).

expand_cells(_Parent, 3, Cells, ParentMap) -> {ParentMap, Cells};
expand_cells(Parent, _, Cells, ParentMap) ->
  {Face, I, J, Orientation} = s2cellid:to_face_ij_orientation(Parent),
  ?assert(Face == s2cellid:face(Parent)),

  Child = s2cellid:child_begin(Parent),
  ChildEnd = s2cellid:child_end(Parent),
  Pos = 0,
  check_child(Pos, Parent, ParentMap, Cells, Child, ChildEnd, {Face, I, J, Orientation}).

check_child(_, _, ParentMap, Cells, ChildEnd, ChildEnd, _ ) -> {ParentMap, Cells};
check_child(Pos, Parent, ParentMap, Cells, Child, ChildEnd, {Face, I, J, Orientation}) ->
  ?assert(s2cellid:child(Parent, Pos) == Child),
  ?assert(s2cellid:level(Child) == s2cellid:level(Parent) + 1),
  ?assert(s2cellid:is_leaf(Child) == false),
  {CFace, _CI, _CJ, COrientation} = s2cellid:to_face_ij_orientation(Child),
  ?assert(CFace == Face),
  ?assert(COrientation == (Orientation bxor pos_to_orientation(Pos))),
  {ChildLevelMap, ChildLevelCells} = expand_cells(Child, s2cellid:level(Child), [Parent | Cells], maps:put(Child, Parent, ParentMap)),
  check_child(Pos + 1, Parent, ChildLevelMap, ChildLevelCells, s2cellid:next(Child), ChildEnd, {Face, I, J, Orientation}).



% def expand_cells(self, parent, cells, parent_map):
%     cells.append(parent)
%     if parent.level() == 3:  # max level for expand
%         return

%     face, i, j, orientation = parent.to_face_ij_orientation()
%     ?assert(face, parent.face())

%     child = parent.child_begin()
%     child_end = parent.child_end()
%     pos = 0
%     while child != child_end:
%         ?assert(parent.child(pos), child)
%         ?assert(child.level(), parent.level() + 1)
%         false = child.is_leaf()
%         cface, ci, cj, corientation = child.to_face_ij_orientation()
%         ?assert(cface, face)
%         ?assert(corientation,
%                          orientation ^ s2sphere.POS_TO_ORIENTATION[pos])

%         parent_map[child] = parent
%         self.expand_cells(child, cells, parent_map)
%         child = child.next()
%         pos = pos + 1

containment_test() ->
  Cellid = s2cellid:new_from_face_pos_level(0, 0, 0),
  {Map, RevCells} = expand_cells(Cellid, s2cellid:level(Cellid), [], #{}).

% def testContainment(self):
%     parent_map = {}
%     cells = []
%     for face in range(6):
%         self.expand_cells(CellId.from_face_pos_level(face, 0, 0),
%                           cells, parent_map)

%     for i, s2cellid:i in enumerate(cells):
%         for j, s2cellid:j in enumerate(cells):
%             contained = True
%             s2cellid:= s2cellid:j
%             while s2cellid:!= s2cellid:i:
%                 next_s2cellid:= parent_map.get(s2cellid:
%                 if next_s2cellid:is None:
%                     contained = False
%                     break
%                 s2cellid:= next_s2cellid:
%             ?assert(cells[i].contains(cells[j]), contained)
%             ?assert(cells[j] >= cells[i].range_min() and
%                              cells[j] <= cells[i].range_max(), contained)
%             ?assert(cells[i].intersects(cells[j]),
%                              cells[i].contains(cells[j]) or
%                              cells[j].contains(cells[i]))

% def test_walk_fast_and_slow(self):
%     slow = [c.to_token() for c in CellId.walk(2)]
%     fast = [c.to_token() for c in CellId.walk_fast(2)]
%     ?assert(slow, fast)

% def testContinuity(self):
%     # Make sure that sequentially increasing cell ids form a continuous
%     # path over the surface of the sphere, i.e. there are no
%     # discontinuous jumps from one region to another.

%     max_walk_level = 8
%     cell_size = 1 / (1 << max_walk_level)
%     max_dist = CellId.max_edge().get_value(max_walk_level)

%     for s2cellid:in CellId.walk_fast(max_walk_level):
%         self.assertLessEqual(
%             s2cellid:to_point_raw().angle(
%                 s2cellid:next_wrap().to_point_raw()), max_dist)

%         ?assert(s2cellid:advance_wrap(1), s2cellid:next_wrap())
%         ?assert(s2cellid:next_wrap().advance_wrap(-1), s2cellid:

%         # Check that the ToPointRaw() returns the center of each cell
%         # in (s,t) coordinates.
%         face, u, v = s2sphere.xyz_to_face_uv(s2cellid:to_point_raw())
%         self.assertAlmostEqual(
%             CellId.uv_to_st(u) % (0.5 * cell_size), 0, delta=1e-15)
%         self.assertAlmostEqual(
%             CellId.uv_to_st(v) % (0.5 * cell_size), 0, delta=1e-15)

% def testCoverage(self):
%     max_dist = 0.5 * CellId.max_diag().get_value(CellId.MAX_LEVEL)
%     for i in xrange(COVERAGE_ITERATIONS):
%         p = self.get_random_point()
%         q = CellId.from_point(p).to_point_raw()
%         self.assertLessEqual(p.angle(q), max_dist)

% def testNeighbors(self):
%     # Check the edge neighbors of face 1.
%     out_faces = (5, 3, 2, 0)
%     face_nbrs = CellId.from_face_pos_level(1, 0, 0).get_edge_neighbors()
%     for i, face_nbr in enumerate(face_nbrs):
%         true = face_nbr.is_face()
%         ?assert(face_nbr.face(), out_faces[i])

%     # Check the vertex neighbors of the center of face 2 at level 5.
%     neighbors = CellId.from_point(Point(0, 0, 1)).get_vertex_neighbors(5)
%     neighbors.sort()
%     for i, neighbor in enumerate(neighbors):
%         ?assert(
%             neighbor,
%             CellId.from_face_ij(
%                 2,
%                 (1 << 29) - (i < 2), (1 << 29) - (i == 0 or i == 3)
%             ).parent(5)
%         )

%     # Check the vertex neighbors of the corner of faces 0, 4, and 5.
%     s2cellid:= CellId.from_face_pos_level(0, 0, CellId.MAX_LEVEL)
%     neighbors = s2cellid:get_vertex_neighbors(0)
%     neighbors.sort()
%     ?assert(len(neighbors), 3)
%     ?assert(neighbors[0], CellId.from_face_pos_level(0, 0, 0))
%     ?assert(neighbors[1], CellId.from_face_pos_level(4, 0, 0))
%     ?assert(neighbors[2], CellId.from_face_pos_level(5, 0, 0))

%     for i in xrange(NEIGHBORS_ITERATIONS):
%         s2cellid:= TestCellId.get_random_s2cellid:)
%         if s2cellid:is_leaf():
%             s2cellid:= s2cellid:parent()

%         max_diff = min(6, CellId.MAX_LEVEL - s2cellid:level() - 1)
%         if max_diff == 0:
%             level = s2cellid:level()
%         else:
%             level = s2cellid:level() + random.randrange(max_diff)

%         self.check_all_neighbors(s2cellid: level)

% def check_all_neighbors(self, s2cellid: level):
%     self.assertGreaterEqual(level, s2cellid:level())
%     self.assertLess(level, CellId.MAX_LEVEL)

%     all, expected = set(), set()

%     neighbors = s2cellid:get_all_neighbors(level)
%     all.update(neighbors)
%     for c in s2cellid:children(level + 1):
%         all.add(c.parent())
%         expected.update(c.get_vertex_neighbors(level))

%     ?assert(expected, all)