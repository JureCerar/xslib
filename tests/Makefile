FC			:= gfortran
FC_FLAGS	:= -cpp -O3 -pipe -fopenmp -funroll-loops -finline-functions -flto
#FC_DEBUG    := -g -O0 -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow
LIBS		:= $(shell pkg-config xslib --libs --cflags )

SRC 		:= $(wildcard *.f90 *.F90)
#SRC		= test_files.f90 # test_simple.f90 test_utilities.f90

default: clean $(SRC)
	$(foreach name, $(SRC), $(FC) $(FC_FLAGS) $(FC_DEBUG) $(name) $(LIBS) && ./a.out;)

.PHONY: clean

clean:
	@rm -f a.out *.mod *.o
