# Default build type
if ( NOT CMAKE_BUILD_TYPE )
  set ( CMAKE_BUILD_TYPE RELEASE CACHE STRING "Build configuration 'Release' or 'Debug'." FORCE )
  message ( STATUS "CMAKE_BUILD_TYPE not given, defaulting to RELEASE." )
else ()
  message ( STATUS "CMAKE_BUILD_TYPE is: ${CMAKE_BUILD_TYPE}" )
endif ()

# Compiler options
IF ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" )
	set ( CMAKE_Fortran_FLAGS "-cpp -O3 -march=native -pipe -fopenmp -funroll-loops -finline-functions -flto ${CMAKE_Fortran_FLAGS}" )
ELSEIF ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" )
	set ( CMAKE_Fortran_FLAGS "-openmp -fpp -O3 -FR -xHost -ipo -ip -unroll -vec-report0 -trace ${CMAKE_Fortran_FLAGS}" )
ELSEIF ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI" )
	set ( CMAKE_Fortran_FLAGS "-O3 -Munroll -Mipa -Minline -Mvect -mp ${CMAKE_Fortran_FLAGS}" ) # -Mcuda -ta=tesla,cc70,fastmath
ENDIF ()

# DEBUG compiler options
IF ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" )
	set ( CMAKE_Fortran_FLAGS_DEBUG "-g3 -fbounds-check -fbacktrace  -ffpe-trap=invalid,zero,overflow,underflow" )
ELSEIF ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" )
	set ( CMAKE_Fortran_FLAGS_DEBUG "-g -C -traceback -check all -debug all -fpe-model strict -fpe-all=0 –no-ftz" )
ELSEIF ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI" )
	set ( CMAKE_Fortran_FLAGS_DEBUG "-g -C -Mbounds -Mchkptr -Mchkstk -traceback -Ktrap=fp –Minform=inform")
ENDIF ()

# Message back flags
message ( STATUS "CMAKE_Fortran_FLAGS: ${CMAKE_Fortran_FLAGS}" )
IF ( ${CMAKE_BUILD_TYPE} STREQUAL "DEBUG" )
	message ( STATUS "CMAKE_Fortran_FLAGS_DEBUG: ${CMAKE_Fortran_FLAGS_DEBUG}" )
ENDIF ()
