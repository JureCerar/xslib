---
project: JureCerar/xslib
src_dir: src
include: src
         include
output_dir: html
preprocess: true
macro: PROJECT_VERSION="0.0.0"
       PROJECT_VERSION_MAJOR=0
       PROJECT_VERSION_MINOR=0
       PROJECT_VERSION_PATCH=0
source: False
md_extensions: markdown.extensions.toc
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html
print_creation_date: true
creation_date: %Y.%m.%d %H:%M:%z
license: gpl
dbg: true
---

[TOC]

{!README.md!}