# factory-planner
Can't get makefile to work.
To compile without, sequentially run

    stack ghc -- src/LuaSolve.hs -o luasolve.so -shared -fPIC -dynamic -lHSrts-ghc8.8.3

