template<typename T>
ERL_NIF_TERM s2region_coverer_get_covering(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if(argc < 2) // Need atleast one arg
        return enif_make_badarg(env);
    try{
        T *self;
        nifpp::get_throws(env, argv[0], self);

        S2RegionCoverer coverer;
        auto max_cells = nifpp::get<int>(env, argv[1]);

        int min_level;
        int max_level;
        auto levels = make_tuple(ref(min_level), ref(max_level));
        nifpp::get(env, argv[2], levels);

        coverer.set_max_cells(max_cells);
        coverer.set_min_level(min_level);
        coverer.set_max_level(max_level);

        vector<S2CellId> covering;
        coverer.GetCovering(*self, &covering);
        return nifpp::make(env, covering);
        }
    }
    catch(nifpp::badarg) {}
    return enif_make_badarg(env);
}

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
