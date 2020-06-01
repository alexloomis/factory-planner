require "hsmatrixsolver"

fixed = {steel = 1}

free = {}

matrix = {
  iron = {
    make_iron = 1,
    make_steel = -5
  },
  steel = {
    make_steel = 1
  }
}

hs_init()
print(haskell_matrix_solver(fixed, free, matrix))
print("seg fault here?")
hs_exit()

