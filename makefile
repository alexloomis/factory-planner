ALL: libluasolve.so hsmatrixsolver.so

libluasolve.so: LuaSolve.hs
	stack ghc -- $^ -o $@ -shared -fPIC -dynamic -lHSrts-ghc8.8.3

hsmatrixsolver.so: luasolve.so hsmatrixhelper.c
	stack ghc -- hsmatrixhelper.c -no-hs-main -o $@ -shared -fPIC -dynamic \
		-L. -lluasolve -I/usr/include/lua5.3

clean:
	rm -f *.hi
	rm -f *.o
	rm -f *.so
	rm -f *_stub.h

