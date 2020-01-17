# Compiler
FC 				:= gfortran
FC_FLAGS	:= -g -Og -march=native

# Source
APP = a.out
SRC = xslib_cubio.f90 main.f90
OBJ = $(SRC:.f90=.o)

default: $(OBJ)
	$(FC) $^ -o $(APP) $(FC_FLAGS)

%.o: %.f90
	$(FC) -c $< $(FC_FLAGS)

.PHONY: clean

clean:
	@rm -rf *.o *.mod a.out
