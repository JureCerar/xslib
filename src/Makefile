# Fortran & C compilers
FC 				:= gfortran
FC_FLAGS	:= -shared -fPIC -O2 -march=native -fopenmp
CC 				:= gcc
CC_FLAGS	:= -shared -fPIC -O2 -march=native -fopenmp

# Install
INSTALL 	:= install

# -------------------------
# Library
LIB = libxslib.so
DIR := /usr/local
# Source
SRC = xdrfile.c \
xdrfor.f90 \
xslib_cstring.f90 \
xslib_vector.f90 \
xslib_time.f90 \
xslib_error.f90 \
xslib_xmalloc.f90 \
xslib_list.f90 \
xslib_xyzio.f90 \
xslib_groio.f90 \
xslib_pdbio.f90 \
xslib_xtcio.f90 \
xslib_trrio.f90 \
xslib_dcdio.f90 \
xslib_cubio.f90 \
xslib_ndxio.f90 \
xslib_tplio.f90 \
xslib_pdhio.f90 \
xslib_csvio.f90 \
xslib_fileio.f90 \
xslib.f90
# Objects
OBJ = $(SRC)
OBJ := $(OBJ:.f90=.o)
OBJ := $(OBJ:.c=.o)

# -------------------------
# Build

default: $(LIB)

$(LIB): $(OBJ)
	$(FC) $^ -o $(LIB) $(FC_FLAGS)

%.o: %.f90
	$(FC) -c $< $(FC_FLAGS)

%.o: %.F90
	$(FC) -c $< $(FC_FLAGS)

%.o: %.c
	$(CC) -c $< $(CC_FLAGS)

.PHONY: clean install

install:
	$(INSTALL) $(LIB) $(DIR)/lib
	$(INSTALL) *.mod  $(DIR)/include

clean:
	@rm -rf *.o *.mod $(LIB)
