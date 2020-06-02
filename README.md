# factory-planner
To create library, run `make`.
You may need to run `stack build` first.
To install, copy hsmatrixsolver.so and libluasolve.so,
making sure that libluasolve.so is on `LD_LIBRARY_PATH`,
and that `LD_LIBRARY_PATH` is exported.
To use, do

    require "hsmatrixsolver"
    hs_init()
    code containing haskell_matrix_solver(fixed, free, matrix)
    hs_exit()

Currently segfaults.
Exporting many outputs or a `Map` of outputs
instead of a list of outputs may fix that.

