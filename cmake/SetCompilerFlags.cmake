## Define default build type
if ( NOT CMAKE_BUILD_TYPE )
  set ( CMAKE_BUILD_TYPE "RELEASE" CACHE STRING "Build configuration 'RELEASE' or 'DEBUG'." FORCE )
  message ( STATUS "CMAKE_BUILD_TYPE not given, defaulting to RELEASE.")
else ()
  message ( STATUS "CMAKE_BUILD_TYPE is: ${CMAKE_BUILD_TYPE}")
endif ()

## FORTRAN Compiler
if ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" )
  set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -march=native" )
  set ( CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -fexcess-precision=fast -funroll-loops -finline-functions" )
  set ( CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -Og -Wall -fcheck=all -fbacktrace" )
endif ()

## C Compiler
if ( ${CMAKE_C_COMPILER_ID} STREQUAL "GNU" )
  set ( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -march=native" )
  set ( CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -fexcess-precision=fast -funroll-loops -finline-functions" )
  set ( CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -Og -Wall" )
endif ()
