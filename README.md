# factory-planner
Can't get makefile to work.
To compile without, sequentially run

    stack ghc -- src/LuaSolve.hs -o libluasolve.so -shared -fPIC -dynamic -lHSrts-ghc8.8.3
    stack ghc -- hsmatrixhelper.c -no-hs-main -o hsmatrixsolver.so -shared -fPIC -dynamic -L. -lluasolve -Isrc -I/usr/include/lua5.3

you may need to run `stack build` first.
To install, copy hsmatrixsolver.so and libluasolve.so,
making sure that libluasolve.so is on LD_LIBRARY_PATH.
To use, do

    require "hsmatrixsolver"
    hs_init()
    code containing haskell_matrix_solver(fixed, free, matrix)
    hs_exit()
