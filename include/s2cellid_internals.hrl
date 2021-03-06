-define(S2CELLID_CONSTRUCTOR_NONE,                   1).
-define(S2CELLID_CONSTRUCTOR_SENTINEL,               2).
-define(S2CELLID_CONSTRUCTOR_FROM_FACE_POS_LEVEL,    3).
-define(S2CELLID_CONSTRUCTOR_FROM_POINT,             4).
-define(S2CELLID_CONSTRUCTOR_FROM_LAT_LNG,           5).
-define(S2CELLID_CONSTRUCTOR_BEGIN,                  6).
-define(S2CELLID_CONSTRUCTOR_END,                    7).
-define(S2CELLID_CONSTRUCTOR_FROM_TOKEN,             8).
-define(S2CELLID_CONSTRUCTOR_FROM_FACE_IJ,           9).

% functions which take no extra arguments other than cellid
-define(S2CELLID_0_ARGS_IS_VALID,       10).
-define(S2CELLID_0_ARGS_TO_POINT,       11).
-define(S2CELLID_0_ARGS_TO_LAT_LNG,     12).
-define(S2CELLID_0_ARGS_FACE,           13).
-define(S2CELLID_0_ARGS_POS,            14).
-define(S2CELLID_0_ARGS_LEVEL,          15).
-define(S2CELLID_0_ARGS_GET_SIZE_IJ,    16).
-define(S2CELLID_0_ARGS_GET_SIZE_ST,    17).
-define(S2CELLID_0_ARGS_IS_LEAF,        18).
-define(S2CELLID_0_ARGS_IS_FACE,        19).
-define(S2CELLID_0_ARGS_RANGE_MIN,      20).
-define(S2CELLID_0_ARGS_RANGE_MAX,      21).
-define(S2CELLID_0_ARGS_PARENT,         22).
-define(S2CELLID_0_ARGS_CHILD_BEGIN,    23).
-define(S2CELLID_0_ARGS_CHILD_END,      24).
-define(S2CELLID_0_ARGS_NEXT,           25).
-define(S2CELLID_0_ARGS_PREV,           26).
-define(S2CELLID_0_ARGS_NEXT_WRAP,      27).
-define(S2CELLID_0_ARGS_PREV_WRAP,      28).
-define(S2CELLID_0_ARGS_TO_TOKEN,       29).
-define(S2CELLID_0_ARGS_TO_STRING,      30).
-define(S2CELLID_0_ARGS_LSB,            31).

% functions which take a single argument other than cellid

-define(S2CELLID_1_ARG_GET_SIZE_IJ,        100).
-define(S2CELLID_1_ARG_GET_SIZE_ST,        101).
-define(S2CELLID_1_ARG_CHILD_POSITION,     102).
-define(S2CELLID_1_ARG_CONTAINS,           103).
-define(S2CELLID_1_ARG_INTERSECTS,         104).
-define(S2CELLID_1_ARG_PARENT,             105).
-define(S2CELLID_1_ARG_CHILD,              106).
-define(S2CELLID_1_ARG_CHILD_BEGIN,        107).
-define(S2CELLID_1_ARG_CHILD_END,          108).
-define(S2CELLID_1_ARG_ADVANCE,            109).
-define(S2CELLID_1_ARG_ADVANCE_WRAP,       110).

