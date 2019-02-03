project (xdrfile C)

# Source
file (GLOB XDRSRC "src/*.c")

# Project version
file ( STRINGS ".VERSION" VERSION )
string ( REPLACE "." ";" VERSION_LIST ${VERSION} )
list ( GET VERSION_LIST 0 VERSION_MAJOR )
list ( GET VERSION_LIST 1 VERSION_MINOR )
list ( GET VERSION_LIST 2 VERSION_PATCH )
set ( PROJECT_VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}" )

# Compile
add_library ( xdrfile SHARED ${XDRSRC} )
target_include_directories ( xdrfile PUBLIC headers )
