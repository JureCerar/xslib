# xslib - Extra-Small Library

[![Release](https://img.shields.io/github/release/JureCerar/xslib.svg?label=Release&color=blue)](https://github.com/JureCerar/xslib/releases)
![Language](https://img.shields.io/badge/Language-Fortran,_C-brightgreen.svg)
[![License](https://img.shields.io/badge/License-GNU_GPL_v3.0-red.svg)](https://www.gnu.org/licenses/gpl-3.0.html)

The Extra-Small Library (xslib) is a modern Fortran library consisting of useful utilities and functions as stand-in for Fortran "standard" library. The library is written with primary purpose of learning modern Fortran language, good coding practices, and in hopes that it helps someone else on their quest of learning Fortran.

## Build & Install

For latest build clone repository from GitHub (or download other [releases](https://github.com/JureCerar/xslib/releases)):

```bash
git clone https://github.com/JureCerar/xslib
```

Move into directory and type the following:

```bash
mkdir build
cd build
cmake ..
make
make check
```

To install the library type the following as _root_ (or _sudo_):

```bash
make install
```

You should consider using the following [CMake options](https://cmake.org/cmake/help/v3.6/manual/cmake.1.html) with the appropriate value instead of `xxx`:
- `-DCMAKE_Fortran_COMPILER=xxx` equal to the Fortran compiler (or use ENV variable `FC`)
- `-DCMAKE_C_COMPILER=xxx` equal to the C compiler (or use ENV variable `CC`)
- `-DCMAKE_INSTALL_PREFIX=xxx` to install xslib to a non-standard location (default `/usr/local/lib/`)  
- `-DCMAKE_BUILD_TYPE=xxx` equal to `RELEASE` for normal build or `DEBUG` for debugging purposes.

## Documentation

Documentation is available at [doc/README.md](doc/README.md).  

## Usage

To use the library in your project add `use xslib` in the modules section of your program:

```fortran
program main
  use xslib
  implicit none
  ! ...
end program main
```

When compiling your program add `-lxslib` flag to compiler options. You may also need to use `-I` flag to point to where the modules files are (default `-I/usr/local/include`) even with all of the right environment variables set. When linking use `-L` to point to library file (default `-L/usr/local/lib`).

To make things easier __pkg-config__ file is also included to help you with your program compilation. You may need to add the config file to `PKG_CONFIG_PATH` environment variable (default '/usr/local/lib/pkgconfig').

```bash
pkg-config xslib --libs --cflags
```

Alternatively, the library can be added with [CMake](https://cmake.org/). First, find the library on your computer:

```cmake
find_package ( xslib 3.0 REQUIRED )
include_directories ( ${xslib_INCLUDE_DIRS} )
```

Then link _shared_ or _static_ library to your target build:

```cmake
# Link shared library
target_link_libraries ( ${CMAKE_PROJECT_NAME} ${xslib_LIBRARIES} )
# OR link static library
target_link_libraries ( ${CMAKE_PROJECT_NAME} ${xslib_STATIC_LIBRARIES} )
```

__NOTE:__ In case of non-standard installation path use the following CMake option (with the appropriate value instead of `xxx`):
- `-Dxslib_DIR=xxx` equal to CMake config file path (default is `/usr/local/lib/cmake/xslib-X.X.X`).

## Notes

Functions for handling molecular files are now available separately. See [atomlib](https://github.com/JureCerar/atomlib) for more information.

Also check out (arguably _much_ better) [Fortran-lang/stdlib](https://github.com/fortran-lang/stdlib) project.

## License

This program is licensed under the __GNU General Public License v3.0__

Copyright (C) 2019-2023 [Jure Cerar](https://github.com/JureCerar)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/.
