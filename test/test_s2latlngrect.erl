-module(test_s2latlngrect).
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

rect_from_degrees(LatLo, LngLo, LatHi, LngHi) ->
    s2latlngrect:new({LatLo, LngLo}, {LatHi, LngHi}).

empty_and_full_test() ->
    Empty = s2latlngrect:empty(),
    Full = s2latlngrect:full(),

    ?assert(s2latlngrect:is_valid(Empty) == true),
    ?assert(s2latlngrect:is_empty(Empty) == true),
    ?assert(s2latlngrect:is_point(Empty) == false),

    ?assert(s2latlngrect:is_valid(Full) == true),
    ?assert(s2latlngrect:is_full(Full) == true),
    ?assert(s2latlngrect:is_point(Full) == false),

    DefaultEmpty = s2latlngrect:new(),
    ?assert(s2latlngrect:is_valid(DefaultEmpty) == true),
    ?assert(s2latlngrect:is_empty(DefaultEmpty) == true),
    ?assert(s2latlngrect:is_point(Full) == false).
    %DefaultEmpty



%     def testEmptyAndFull(self):
%         empty = LatLngRect.empty()
%         full = LatLngRect.full()
%         self.assertTrue(empty.is_valid())
%         self.assertTrue(empty.is_empty())
%         self.assertFalse(empty.is_point())
%         self.assertTrue(full.is_valid())
%         self.assertTrue(full.is_full())
%         self.assertFalse(full.is_point())

%         default_empty = LatLngRect()
%         self.assertTrue(default_empty.is_valid())
%         self.assertTrue(default_empty.is_empty())
%         self.assertEqual(empty.lat().bounds(), default_empty.lat().bounds())
%         self.assertEqual(empty.lng().bounds(), default_empty.lng().bounds())

%     def testAccessors(self):
%         d1 = self.rect_from_degrees(-90, 0, -45, 180)
%         self.assertEqual(d1.lat_lo().degrees, -90)
%         self.assertEqual(d1.lat_hi().degrees, -45)
%         self.assertEqual(d1.lng_lo().degrees, 0)
%         self.assertEqual(d1.lng_hi().degrees, 180)
%         self.assertEqual(d1.lat(),
%                          LineInterval(-math.pi / 2.0, -math.pi / 4.0))
%         self.assertEqual(d1.lng(),
%                          SphereInterval(0, math.pi))

%     def testFromCenterSize(self):
%         self.assertTrue(
%             LatLngRect.from_center_size(
%                 LatLng.from_degrees(80, 170),
%                 LatLng.from_degrees(40, 60),
%             ).approx_equals(self.rect_from_degrees(60, 140, 90, -160))
%         )

%         self.assertTrue(LatLngRect.from_center_size(
%             LatLng.from_degrees(10, 40),
%             LatLng.from_degrees(210, 400)).is_full()) \

%         self.assertTrue(
%             LatLngRect.from_center_size(
%                 LatLng.from_degrees(-90, 180),
%                 LatLng.from_degrees(20, 50),
%             ).approx_equals(self.rect_from_degrees(-90, 155, -80, -155))
%         )

%     def testFromPoint(self):
%         p = LatLng.from_degrees(23, 47)
%         self.assertEqual(LatLngRect.from_point(p), LatLngRect(p, p))
%         self.assertTrue(LatLngRect.from_point(p).is_point())

%     def testFromPointPair(self):
%         self.assertEqual(LatLngRect.from_point_pair(
%             LatLng.from_degrees(-35, -140), LatLng.from_degrees(15, 155)),
%             self.rect_from_degrees(-35, 155, 15, -140))
%         self.assertEqual(LatLngRect.from_point_pair(
%             LatLng.from_degrees(25, -70), LatLng.from_degrees(-90, 80)),
%             self.rect_from_degrees(-90, -70, 25, 80))

%     def testGetCenterSize(self):
%         r1 = LatLngRect(LineInterval(0, math.pi / 2.0),
%                         SphereInterval(-math.pi, 0))
%         self.assertEqual(r1.get_center(),
%                          LatLng.from_radians(math.pi / 4.0, -math.pi / 2.0))
%         self.assertEqual(r1.get_size(),
%                          LatLng.from_radians(math.pi / 2.0, math.pi))
%         self.assertLess(
%             LatLngRect.empty().get_size().lat().radians, 0)
%         self.assertLess(
%             LatLngRect.empty().get_size().lng().radians, 0)

%     def testGetVertex(self):
%         r1 = LatLngRect(LineInterval(0, math.pi / 2.0),
%                         SphereInterval(-math.pi, 0))
%         self.assertEqual(r1.get_vertex(0), LatLng.from_radians(0, math.pi))
%         self.assertEqual(r1.get_vertex(1), LatLng.from_radians(0, 0))
%         self.assertEqual(r1.get_vertex(2),
%                          LatLng.from_radians(math.pi / 2.0, 0))
%         self.assertEqual(r1.get_vertex(3),
%                          LatLng.from_radians(math.pi / 2.0, math.pi))

%         # Make sure the get_vertex() returns vertices in CCW order.
%         for i in range(4):
%             lat = math.pi / 4.0 * (i - 2)
%             lng = math.pi / 2.0 * (i - 2) + 0.2
%             r = LatLngRect(LineInterval(lat, lat + math.pi / 4.0),
%                            SphereInterval(s2sphere.drem(lng, 2 * math.pi),
%                            s2sphere.drem(lng + math.pi / 2.0, 2 * math.pi)))
%             for k in range(4):
%                 self.assertTrue(
%                     s2sphere.simple_ccw(r.get_vertex((k - 1) & 3).to_point(),
%                                         r.get_vertex(k).to_point(),
%                                         r.get_vertex((k + 1) & 3).to_point())
%                 )

%     def testArea(self):
%         self.assertEqual(s2sphere.LatLngRect.empty().area(), 0.0)
%         self.assertEqual(s2sphere.LatLngRect.full().area(), 4 * math.pi)
%         self.assertEqual(self.rect_from_degrees(0, 0, 90, 90).area(),
%                          math.pi / 2)

%     def testContains(self):
%         eq_m180 = LatLng.from_radians(0, -math.pi)
%         north_pole = LatLng.from_radians(math.pi / 2.0, 0)
%         r1 = LatLngRect(eq_m180, north_pole)

%         self.assertTrue(r1.contains(LatLng.from_degrees(30, -45)))
%         self.assertTrue(r1.interior_contains(LatLng.from_degrees(30, -45)))
%         self.assertFalse(r1.contains(LatLng.from_degrees(30, 45)))
%         self.assertFalse(r1.interior_contains(LatLng.from_degrees(30, 45)))
%         self.assertTrue(r1.contains(eq_m180))
%         self.assertFalse(r1.interior_contains(eq_m180))
%         self.assertTrue(r1.contains(north_pole))
%         self.assertFalse(r1.interior_contains(north_pole))
%         self.assertTrue(r1.contains(Point(0.5, -0.3, 0.1)))
%         self.assertFalse(r1.contains(Point(0.5, 0.2, 0.1)))

%     def check_interval_ops(self, x, y, expected_relation,
%                            expected_union, expected_intersection):
%         self.assertEqual(x.contains(y), expected_relation[0] == 'T')
%         self.assertEqual(x.interior_contains(y), expected_relation[1] == 'T')
%         self.assertEqual(x.intersects(y), expected_relation[2] == 'T')
%         self.assertEqual(x.interior_intersects(y),
%                          expected_relation[3] == 'T')

%         self.assertEqual(x.contains(y), x.union(y) == x)
%         self.assertEqual(x.intersects(y), not x.intersection(y).is_empty())

%         self.assertEqual(x.union(y), expected_union)
%         self.assertEqual(x.intersection(y), expected_intersection)

%     def testIntervalOps(self):
%         r1 = self.rect_from_degrees(0, -180, 90, 0)

%         # Test operations where one rectangle consists of a single point.
%         r1_mid = self.rect_from_degrees(45, -90, 45, -90)
%         self.check_interval_ops(r1, r1_mid, "TTTT", r1, r1_mid)

%         req_m180 = self.rect_from_degrees(0, -180, 0, -180)
%         self.check_interval_ops(r1, req_m180, "TFTF", r1, req_m180)

%         rnorth_pole = self.rect_from_degrees(90, 0, 90, 0)
%         self.check_interval_ops(r1, rnorth_pole, "TFTF", r1, rnorth_pole)

%         self.check_interval_ops(
%             r1,
%             self.rect_from_degrees(-10, -1, 1, 20), "FFTT",
%             self.rect_from_degrees(-10, 180, 90, 20),
%             self.rect_from_degrees(0, -1, 1, 0),
%         )
%         self.check_interval_ops(
%             r1,
%             self.rect_from_degrees(-10, -1, 0, 20), "FFTF",
%             self.rect_from_degrees(-10, 180, 90, 20),
%             self.rect_from_degrees(0, -1, 0, 0))
%         self.check_interval_ops(
%             r1,
%             self.rect_from_degrees(-10, 0, 1, 20), "FFTF",
%             self.rect_from_degrees(-10, 180, 90, 20),
%             self.rect_from_degrees(0, 0, 1, 0),
%         )

%         self.check_interval_ops(
%             self.rect_from_degrees(-15, -160, -15, -150),
%             self.rect_from_degrees(20, 145, 25, 155), "FFFF",
%             self.rect_from_degrees(-15, 145, 25, -150),
%             LatLngRect.empty(),
%         )
%         self.check_interval_ops(
%             self.rect_from_degrees(70, -10, 90, -140),
%             self.rect_from_degrees(60, 175, 80, 5), "FFTT",
%             self.rect_from_degrees(60, -180, 90, 180),
%             self.rect_from_degrees(70, 175, 80, 5),
%         )

%         # Check that the intersection of two rectangles that overlap in
%         # latitude but not longitude is valid, and vice versa.
%         self.check_interval_ops(
%             self.rect_from_degrees(12, 30, 60, 60),
%             self.rect_from_degrees(0, 0, 30, 18), "FFFF",
%             self.rect_from_degrees(0, 0, 60, 60),
%             LatLngRect.empty(),
%         )
%         self.check_interval_ops(
%             self.rect_from_degrees(0, 0, 18, 42),
%             self.rect_from_degrees(30, 12, 42, 60), "FFFF",
%             self.rect_from_degrees(0, 0, 42, 60),
%             LatLngRect.empty(),
%         )

%     def testExpanded(self):
%         self.assertTrue(self.rect_from_degrees(70, 150, 80, 170)
%                         .expanded(LatLng.from_degrees(20, 30))
%                         .approx_equals(
%                             self.rect_from_degrees(50, 120, 90, -160))
%                         )
%         self.assertTrue(LatLngRect.empty().expanded(
%             LatLng.from_degrees(20, 30)).is_empty())
%         self.assertTrue(LatLngRect.full().expanded(
%             LatLng.from_degrees(20, 30)).is_full())

%         self.assertTrue(self.rect_from_degrees(-90, 170, 10, 20)
%                         .expanded(LatLng.from_degrees(30, 80))
%                         .approx_equals(
%                             self.rect_from_degrees(-90, -180, 40, 180))
%                         )

%     def testConvolveWithCap(self):
%         self.assertTrue(self.rect_from_degrees(0, 170, 0, -170)
%                         .convolve_with_cap(Angle.from_degrees(15))
%                         .approx_equals(
%                             self.rect_from_degrees(-15, 155, 15, -155))
%                         )

%         self.assertTrue(self.rect_from_degrees(60, 150, 80, 10)
%                         .convolve_with_cap(Angle.from_degrees(15))
%                         .approx_equals(
%                             self.rect_from_degrees(45, -180, 90, 180))
%                         )

%     def testGetCapBound(self):

%         # Bounding cap at center is smaller:
%         self.assertTrue(
%             self.rect_from_degrees(-45, -45, 45, 45)
%             .get_cap_bound()
%             .approx_equals(Cap.from_axis_height(Point(1, 0, 0), 0.5)))

%         # Bounding cap at north pole is smaller:
%         self.assertTrue(
%             self.rect_from_degrees(88, -80, 89, 80)
%             .get_cap_bound()
%             .approx_equals(Cap.from_axis_angle(Point(0, 0, 1),
%                            Angle.from_degrees(2))))

%         # Longitude span > 180 degrees:
%         self.assertTrue(
%             self.rect_from_degrees(-30, -150, -10, 50)
%             .get_cap_bound()
%             .approx_equals(Cap.from_axis_angle(Point(0, 0, -1),
%                            Angle.from_degrees(80))))

%     def check_cell_ops(self, r, cell, level):
%         # Test the relationship between the given rectangle and cell:
%         # 0 == no intersection, 1 == MayIntersect, 2 == Intersects,
%         # 3 == Vertex Containment, 4 == Contains
%         vertex_contained = False
%         for i in range(4):
%             if r.contains(cell.get_vertex_raw(i)) or \
%                     (not r.is_empty() and
%                      cell.contains(r.get_vertex(i).to_point())):
%                 vertex_contained = True
%         self.assertEqual(r.may_intersect(cell), level >= 1)
%         self.assertEqual(r.intersects(cell), level >= 2)
%         self.assertEqual(vertex_contained, level >= 3)
%         self.assertEqual(r.contains(cell), level >= 4)

%     def testCellOps(self):

%         # Contains(S2Cell), MayIntersect(S2Cell), Intersects(S2Cell)

%         # Special cases.
%         self.check_cell_ops(
%             LatLngRect.empty(),
%             Cell.from_face_pos_level(3, 0, 0), 0)
%         self.check_cell_ops(
%             LatLngRect.full(),
%             Cell.from_face_pos_level(2, 0, 0), 4)
%         self.check_cell_ops(
%             LatLngRect.full(),
%             Cell.from_face_pos_level(5, 0, 25), 4)

%         # This rectangle includes the first quadrant of face 0.  It's expanded
%         # slightly because cell bounding rectangles are slightly conservative.
%         r4 = self.rect_from_degrees(-45.1, -45.1, 0.1, 0.1)
%         self.check_cell_ops(r4, Cell.from_face_pos_level(0, 0, 0), 3)
%         self.check_cell_ops(r4, Cell.from_face_pos_level(0, 0, 1), 4)
%         self.check_cell_ops(r4, Cell.from_face_pos_level(1, 0, 1), 0)

%         # This rectangle intersects the first quadrant of face 0.
%         r5 = self.rect_from_degrees(-10, -45, 10, 0)
%         self.check_cell_ops(r5, Cell.from_face_pos_level(0, 0, 0), 3)
%         self.check_cell_ops(r5, Cell.from_face_pos_level(0, 0, 1), 3)
%         self.check_cell_ops(r5, Cell.from_face_pos_level(1, 0, 1), 0)

%         # Rectangle consisting of a single point.
%         self.check_cell_ops(self.rect_from_degrees(4, 4, 4, 4),
%                             Cell.from_face_pos_level(0, 0, 0), 3)

%         # Rectangles that intersect the bounding rectangle of a face
%         # but not the face itself.
%         self.check_cell_ops(self.rect_from_degrees(41, -87, 42, -79),
%                             Cell.from_face_pos_level(2, 0, 0), 1)
%         self.check_cell_ops(self.rect_from_degrees(-41, 160, -40, -160),
%                             Cell.from_face_pos_level(5, 0, 0), 1)

%         # This is the leaf cell at the top right hand corner of face 0.
%         # It has two angles of 60 degrees and two of 120 degrees.
%         cell0tr = Cell.from_point(Point(1 + 1e-12, 1, 1))
%         cell0tr.get_rect_bound()
%         v0 = LatLng.from_point(cell0tr.get_vertex_raw(0))
%         self.check_cell_ops(self.rect_from_degrees(v0.lat().degrees - 1e-8,
%                             v0.lng().degrees - 1e-8,
%                             v0.lat().degrees - 2e-10,
%                             v0.lng().degrees + 1e-10),
%                             cell0tr, 1)

%         # Rectangles that intersect a face but where no vertex of one region
%         # is contained by the other region.  The first one passes through
%         # a corner of one of the face cells.
%         self.check_cell_ops(self.rect_from_degrees(-37, -70, -36, -20),
%                             Cell.from_face_pos_level(5, 0, 0), 2)

%         # These two intersect like a diamond and a square.
%         cell202 = Cell.from_face_pos_level(2, 0, 2)
%         bound202 = cell202.get_rect_bound()
%         self.check_cell_ops(
%             self.rect_from_degrees(bound202.lo().lat().degrees + 3,
%                                    bound202.lo().lng().degrees + 3,
%                                    bound202.hi().lat().degrees - 3,
%                                    bound202.hi().lng().degrees - 3),
%             cell202, 2)

