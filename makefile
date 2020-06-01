ALL: libluasolve.so hsmatrixsolver.so

libluasolve.so: src/LuaSolve.hs
	stack ghc -- $< -o $@ -shared -fPIC -dynamic -lHSrts-ghc8.8.3

hsmatrixsolver.so: libluasolve.so hsmatrixhelper.c
	stack ghc -- hsmatrixhelper.c -no-hs-main -o $@ -shared -fPIC -dynamic -L. -lluasolve -Isrc -I/usr/include/lua5.3

clean:
	rm -f *.hi
	rm -f *.o
	rm -f *.so
	rm -f *_stub.h
	rm -f src/*.hi
	rm -f src/*.o
	rm -f src/*.so
	rm -f src/*_stub.h

