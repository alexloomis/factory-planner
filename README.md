# factory-planner
Can't get makefile to work.
To compile without, sequentially run

    stack ghc -- src/LuaSolve.hs -o libluasolve.so -shared -fPIC -dynamic -lHSrts-ghc8.8.3
    stack ghc -- hsmatrixhelper.c -no-hs-main -o hsmatrixsolver.so -shared -fPIC -dynamic -L. -lluasolve -Isrc -I/usr/include/lua5.3

you may need to run `stack build` first.

