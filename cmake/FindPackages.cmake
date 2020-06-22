## Required packages

# OpenMP library
find_package( OpenMP )
if ( OpenMP_FOUND )
  list ( APPEND CMAKE_Fortran_FLAGS ${OpenMP_Fortran_FLAGS} )
  list ( APPEND CMAKE_C_FLAGS ${OpenMP_C_FLAGS} )
endif ()
