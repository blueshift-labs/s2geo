
// //
// // S2RegionCoverer coverer;
// // coverer.set_max_cells(5);
// // S2Cap cap = S2Cap::FromAxisAngle(...);
// // vector<S2CellId> covering;
// // coverer.GetCovering(cap, &covering);
// //
//   // Return a vector of cell ids that covers (GetCovering) or is contained
//   // within (GetInteriorCovering) the given region and satisfies the various
//   // restrictions specified above.
//   void GetCovering(S2Region const& region, vector<S2CellId>* covering);
//   void GetInteriorCovering(S2Region const& region, vector<S2CellId>* interior);

//   // Return a normalized cell union that covers (GetCellUnion) or is contained
//   // within (GetInteriorCellUnion) the given region and satisfies the
//   // restrictions *EXCEPT* for min_level() and level_mod().  These criteria
//   // cannot be satisfied using a cell union because cell unions are
//   // automatically normalized by replacing four child cells with their parent
//   // whenever possible.  (Note that the list of cell ids passed to the cell
//   // union constructor does in fact satisfy all the given restrictions.)
//   void GetCellUnion(S2Region const& region, S2CellUnion* covering);
//   void GetInteriorCellUnion(S2Region const& region, S2CellUnion* interior);
