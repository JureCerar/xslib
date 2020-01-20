# xslib - Extra-Small Library

[![GitHub version](https://img.shields.io/github/release/JureCerar/xslib.svg?label=Version&color=blue)](https://github.com/JureCerar/xslib/releases)
![Language](https://img.shields.io/badge/Language-Fortran,_C-brightgreen.svg)
[![License](https://img.shields.io/badge/License-GNU_GPL_v3.0-red.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
<!-- [![Generic badge](https://img.shields.io/badge/<SUBJECT>-<STATUS>-<COLOR>.svg)](https://shields.io/) -->

## Synopsis
The Extra-Small Library (xslib) is a modern Fortran library consisting of useful utilities and functions that "computer chemist" might need during his/her work. The library features functions to handle standard molecular coordinate files (**.gro**, **.pdb**, **.cub**, and **.xyz**), trajectory files (**.dcd** and **.xtc**), supporting files (**.ndx** and **.tpl**), and data files (**.csv** and **.pdh**). It uses an object-oriented philosophy for reading and storing data for analysis. The library is written with primary purpose of learning modern Fortran language, good coding practices and in hopes that it helps someone else in their pursuit of knowledge.

## Build & Install
Clone or download repository from GitHub:
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
To install library type the following as *root* (or *sudo*):
```bash
make install
```
You should consider using the following [CMake options](https://cmake.org/cmake/help/v3.6/manual/cmake.1.html) with the appropriate value instead of `xxx`:
- `-DCMAKE_Fortran_COMPILER=xxx` equal to the name of the Fortran Compiler you wish to use (or the ENV variable `FC`)
- `-DCMAKE_C_COMPILER=xxx` equal to the name of the C Compiler you wish to use (or the ENV variable `CC`)
- `-DCMAKE_INSTALL_PREFIX=xxx` to install xslib to a non-standard location (default `/usr/local/lib/`)  
- `-DCMAKE_BUILD_TYPE=xxx` equal to `RELEASE` for normal build or `DEBUG` for debugging purposes.

## Usage
On how to interface with the xslib library, please read [API documentation](doc/API.md) below.  
To use the library put `use xslib` in the module section of your program:
```fortran
program main
  use xslib
  implicit none
  ! ...
end program main
```
When compiling your program add `-lxslib` flag. You may also need to use `-I` flag to point to where the modules files are (default *-I/usr/local/include*) even with all of the right environment variables set. When linking use `-L` to point to library file (default *-L/usr/local/lib*).

To make things easier **pkg-config** file is also included to help you with your program compilation. You may need to add the config file to `PKG_CONFIG_PATH` environment variable (default */usr/local/lib/pkgconfig*).
```bash
pkg-config xslib --libs --cflags
```

Alternatively, the library can be added via **CMake**:
```cmake
# Find xslib package
find_package ( xslib 2.0.0 REQUIRED )
include_directories ( ${xslib_INCLUDE_DIRS} )

...

# Link xslib library
target_link_libraries ( ${CMAKE_PROJECT_NAME} ${xslib_LIBRARIES} )

# or link STATIC xslib libraries
target_link_libraries ( ${CMAKE_PROJECT_NAME} ${xslib_STATIC_LIBRARIES} )
```
**NOTE:** In case of non-standard installation path use the following CMake option (with the appropriate value instead of `xxx`):
- `-Dxslib_DIR=xxx` equal to path to `xslib-config.cmake` (default is */usr/local/lib/cmake/xslib-X.X.X/* ).

## [API documentation](doc/README.md)

## [Changelog](doc/CHANGELOG.md)

## Acknowledgments
This library includes modified versions of [libxdrfile](https://github.com/wesbarnett/libxdrfile) and its Fortran wrapper [libgmxfort](https://github.com/wesbarnett/libgmxfort) for reading GROMACS .ndx, .trr, and .xtc files created by [James W. Barnett](https://github.com/wesbarnett).

<!-- Basically, he is my hero. -->

## License
This program is licensed under the **GNU General Public License v3.0**

Copyright (C) 2019-2020 [Jure Cerar](https://github.com/JureCerar)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/.
