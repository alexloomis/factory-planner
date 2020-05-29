#include "LuaSolve_stub.h"
#include "lua.h"

int hs_init_lua(lua_State *L)
{
  hs_init(NULL, NULL);
  return 0;
}

int hs_exit_lua(lua_State *L)
{
  hs_exit();
  return 0;
}

int luaopen_hsmatrixsolver(lua_State *L)
{
  lua_pushcfunction(L, (int (*)(lua_State*))luaSolve);
  lua_setglobal(L, "haskell_matrix_solver");
  lua_pushcfunction(L, hs_init_lua);
  lua_setglobal(L, "hs_init");
  lua_pushcfunction(L, hs_exit_lua);
  lua_setglobal(L, "hs_exit");
  return 0;
}

