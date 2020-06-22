# xslib API
## Table of contents
### xslib file I/O modules
- [Molecular file types: General](#molecular-file-types-general)
- [Molecular file types: Interface](#molecular-file-types-interface)
- [Molecular file types: Structure](#molecular-file-types-structure)  
  - [`xyz_t` type definition](#xyz_t-type-definition)
  - [`gro_t` type definition](#gro_t-type-definition)
  - [`pdb_t` type definition](#pdb_t-type-definition)
  - [`xtc_t` type definition](#xtc_t-type-definition)
  - [`trr_t` type definition](#trr_t-type-definition)
  - [`dcd_t` type definition](#dcd_t-type-definition)
  - [`cub_t` type definition](#cub_t-type-definition)
  - [`file_t` type definition](#file_t-type-definition)
- [Supporting file types: General](#supporting-file-types-general)
- [Supporting file types: Interface](#supporting-file-types-interface)
- [Supporting file types: Structure](#supporting-file-types-structure)
  - [`ndx_t` type definition](#ndx_t-type-definition)
  - [`tpl_t` type definition](#tpl_t-type-definition)
- [Data file types](#data-file-types)
  - [`pdh_t` type definition](#pdh_t-type-definition)
  - [`csv_t` type definition](#csv_t-type-definition)

### xslib utility modules
- [xslib](#xslib)
  - [`xslibInfo()`](#xslibinfo)
  - [`xslibAbout()`](#xslibabout)
- [xslib_cstring](#xslib_cstring)
  - [`str()`](#str)
  - [`smerge()`](#smerge)
  - [`toLower()` and `toUpper()`](#tolower-and-toupper)
  - [`stripComment()`](#stripcomment)
  - [`isWord()`](#isword)
  - [`isNumber()`](#isnumber)
  - [`replaceText()`](#replacetext)
  - [`getColor()` and `setColor()`](#getcolor-and-setcolor)
  - [`strtok()`](#strtok)
  - [`cnttok()`](#cnttok)
  - [`basename()`](#basename)
  - [`pathname()`](#pathname)
  - [`extension()`](#extension)
  - [`backup()`](#backup)
  - [`progressBar()`](#progressbar)
- [xslib_vector](#xslib_vector)
  - [`cross()`](#cross)
  - [`rotate()`](#rotate)
  - [`deg2rad()` and  `rad2deg()`](#deg2rad-and-rad2deg)
  - [`crt2sph()`, `sph2cart()`, `crt2cyl()`, and `cyl2crt()`](#crt2sph-sph2cart-crt2cyl-and-cyl2crt)
  - [`minImg()`](#minimg)
  - [`getDistance()`](#getdistance)
  - [`getAngle()`](#getangle)
  - [`getDihedral()`](#getdihedral)
  - [`variance()`](#variance)
  - [`lerp()`](#lerp)
  - [`findKClosest()`](#findkclosest)
  - [`findCrossOver()`](#findcrossover)
  - [`swap()`](#swap)
- [xslib_time](#xslib_time)
  - [`wtime()`](#wtime)
  - [`write_wtime()`](#write_wtime)
  - [`msleep()`](#msleep)
- [xslib_list](#xslib_list)
- [xslib_xmalloc](#xslib_xmalloc)
  - [`xmalloc()`](#xmalloc)  
  - [`xcalloc()`](#xcalloc)  
  - [`xrealloc()`](#xrealloc)  
- [xslib_error](#xslib_error)
  - [`error()` and `error_()`](#error-and-error_)
  - [`warning()` and `warning_()`](#warning-and-warning_)
  - [`xslibErrMsg()`](#xsliberrmsg)
  - [`assert()` and `assert_()`](#assert-and-assert_)
  <!-- - [`set_env_colors()`](#set_env_colors) -->
- [Notes](#notes)

-----------------------------------------------
# xslib file I/O modules
## Molecular file types: General
xslib supports the use of multiple molecular coordinate files:  
- [.xyz](https://en.wikipedia.org/wiki/XYZ_file_format) - Simple *name,x,y,z* file that gets the job done in most cases.
- [.gro](http://manual.gromacs.org/archive/5.0.3/online/gro.html) - GROMACS coordinate file.  
- [.pdb](https://www.rcsb.org/pdb/static.do?p=file_formats/pdb/index.html) - Protein Data Bank which is most commonly used and widely spread molecular coordinate file.
- [.xtc](http://manual.gromacs.org/archive/5.0.3/online/xtc.html) - GROMACS compressed binary trajectory files.
- [.trr](http://manual.gromacs.org/archive/5.0.3/online/trr.html) - GROMACS binary trajectory files containing coordinates/velocities/forces .
- [.dcd](https://www.ks.uiuc.edu/Research/namd/2.9/ug/node11.html) - Single precision binary trajectory used by NAMD, CHARMM and LAMMPS.
- [.cub](https://h5cube-spec.readthedocs.io/en/latest/cubeformat.html) - Gaussian CUBE file format.

The xslib library uses object oriented approach for handling molecular files *i.e.* each object contains all the data as well procedures bound to that file type. In order to use each object you must first define it Fortran declaration section as `type(obj_t) :: <obj-name>`, for example:
```Fortran
type(xyz_t) :: xyz
```
All objects (with exception of CUBE cub_t, but we will return to that latter) are structured in same way *i.e.* each object is composed of multiple frames (or just one) where the all data is stored. Each frame consists of number of atoms, their coordinates, and simulation box size, plus additional data that is specific to each file type. So in order to access for example the coordinates of the 2nd atoms in 1st frame:
```Fortran
obj%frame(1)%coor(:,2)
```
**NOTE** the Fortran language uses **row-major** indexing *i.e.* `array(row,column)` and that convention is retained here.

The general API is the same for all derived types. All procedures are defined as functions with an integer return value that contains error code:
```Fortran
type(obj_t) :: obj
integer     :: error

error = obj%procedure(arg)
```
If zero is returned the execution was successful. The error code can be obtained with [`xslibErrMsg()`](#xslibErrMsg).

The general structure of object is as follows:
```Fortran
type obj_frame
  integer           :: natoms
  real, allocatable :: coor(:,:)
  real              :: box(3,3)
  ! ... file specific data.
contains
  generic   :: assignment(=)
  procedure :: allocate
end type obj_frame

type obj_t
  integer                      :: nframes
  type(obj_frame), allocatable :: frame(:)
contains
  generic   :: assignment(=)
  generic   :: allocate
  procedure :: read
  procedure :: open
  procedure :: read_next
  procedure :: skip_next
  procedure :: close
  procedure :: write
  procedure :: getAllframes
  procedure :: getNatoms
  procedure :: getBox
end type obj_t
```

## Molecular file types: Interface
The most simple way to read molecular file is using read method. This opens a file, reads all of the data, and then closes it.
```Fortran
error = obj%read( "path/to/file.ext" )
```
This method is not recommended for large files as it consumes too much memory. The more typical way of reading file is to open it, read it frame-by-frame and then close it when you are done. To open a file use:
```Fortran
error = obj%open( "path/to/file.ext" )
```
Opening a file additionally preforms file check (for formatting and corrupted/missing data) so it can take a while to open large files.  
To read frame use:
```Fortran
error = obj%read_next()
! or
error = obj%read_next(nframes)
```
where `nframes` is optional parameter that defines how many frames to read (default is one). Note that when you call `read_next()` all previous data is deallocated.  
Similarly, in order to skip frames use `skip_next()`, that works same way, but does not store or allocate any data:
```Fortran
error = obj%skip_next()
! or
error = obj%skip_next(nframes)
```
**NOTE:** If you read/skip more frames that are present/left in file, only the remaining number of frames will be allocated and read. It is up to the user to check if how many frames were actually read. Example:
```Fortran
error = obj%read_next(10)
if ( obj%nframes /= 10 ) then
  ! ...
```

Finally, when you are done you should close file with:
```Fortran
error = obj%close()
```
Closing the file does not effect any stored data.

Additionally, when file is opened you can obtain general information about the file *i.e.* total num. of frames, num. of atoms, and simulation box size:
```Fortran
integer :: nframes, natoms
real    :: box(3)

nframes = obj%getAllFrames()
natoms = obj%getNatoms()
box(:) = obj%getBox()
```
Calling this functions does not effect your positioning in file. For practical reasons `getBox()` returns cubic box data (size=3) instead of 3x3 size matrix.  

All derived types have defined assignment(=) function, which allows you to copy data, either the whole object or frame-by-frame:
```Fortran
type(obj_t) :: obj, cpy
cpy = obj
! or
cpy%frame(1) = obj%frame(1)
```
You don't have worry about memory allocation as it automatically.

If you want to create completely new object you first need to allocate the memory. This too can be done either whole object or frame-by-frame:
```Fortran
! Allocate memory in one call
error = obj%allocate(natoms,nframes)
! or frame-by-frame; Both cases yield same result
error = obj%allocate(nframes)
do i = 1, obj%nframes
  error = obj%frame(i)%allocate(natoms)
end do
```
where `nframes` and `natoms` denote number of frames and number of atoms to allocate, respectively. Note that data is not initialized upon memory allocation.

**NOTE:** `trr_t` type consists of coordinates, velocities, and forces. In order to save memory you can choose which will be allocated via "mode" optional argument:
```Fortran
! x=coordinates, v = velocities, and f = forces
error = trr%allocate(natoms,nframes,"xvf") ! Allocate all
error = trr%allocate(natoms,nframes,"xv") ! Only coor and vel
error = trr%allocate(natoms,nframes) ! Default is only coor
! or
error = trr%frame(1)%allocate(natoms,"xvf")
```

**NOTE:** In `cub_t` type you can allocate grid or grid/atoms by:
```Fortran
! Allocate only grid of size: NX x NY x NZ
error = cub%allocate( nx, ny, nz )
! or allocate grid and natoms.
error = cub%allocate( nx, ny, nz, natoms )
```


The stored data can be written to STDOUT, FILE, or file UNIT by:
```Fortran
! Default write is to STDOUT
error = obj%write()
! or write to file
error = obj%write(FILE="path/to/file.obj")
! or write to file unit (must be opened by user)
error = obj%write(UNIT=unit)
```
**NOTE:** The STDOUT and file UNIT methods do not work for `xtc_t`, `trr_t`, and `dcd_t` objects, as they are not human readable formats. Alternatively, in latter cases you can use dump method, to obtain human readable format (useful for debugging):
```Fortran
! Default write is to STDOUT
error = obj%dump()
! or write to file
error = obj%dump(FILE="path/to/file.obj")
! or write to file unit (must be opened by user)
error = obj%dump(UNIT=unit)
```
When either write or dump procedure is called all frames are written.

---------------------
# Molecular file types: Structure
### `xyz_t` type definition
For more information please read [XYZ manual](https://en.wikipedia.org/wiki/XYZ_file_format).

#### Example
```
128
This is comment line
C      14.84292  11.24345  10.00000
O       5.15708   8.75655  10.00000
...
```
**NOTE:** In .xyz format [name] is optional. Will be empty string if not present in file.

#### Structure
```Fortran
type xyz_frame
  integer                   :: natoms
  character(:), allocatable :: comment
  character(6), allocatable :: name(:)
  real, allocatable         :: coor(:,:)
  real                      :: box(3,3)
end type xyz_frame
```

### `gro_t` type definition
For more information please read [GRO manual](http://manual.gromacs.org/archive/5.0.3/online/gro.html).

#### Example
```
This is title, t=0.00000
 128
    1LIG      C    1   1.484   1.124   1.000
    2LIG      O    2   0.516   0.876   1.000
...
   2.00000   2.00000   2.00000
```
**NOTE:** In .gro format velocities are optional.

#### Structure
```Fortran
type gro_frame
  character(:), allocatable  :: title
  real                       :: time
  integer                    :: natoms
  integer, allocatable       :: resn(:), atomn(:)
  character(6), allocatable  :: resnm(:), atomnm(:)
  real, allocatable          :: coor(:,:), vel(:,:)
  real                       :: box(3,3)
end type gro_frame
```

### `pdb_t` type definition
For more information please read [PDB manual](https://www.rcsb.org/pdb/static.do?p=file_formats/pdb/index.html).
**NOTE:** This is only partial implementation i.e. not all PDB directives are supported.
<!-- To be honest we only care about names and coordinates, everything other is pretty much useless -->

#### Example
```none
REMARK    THIS IS A REMARK
...
CRYST1   20.000   20.000   20.000  90.00  90.00  90.00 P 1           1
...
ATOM     36  C  AARG A  -3      13.559  86.257  95.222  0.50 37.37           C
...
END
```

#### Structure
```Fortran
type pdb_frame
  real                      :: box(3,3) = 0.000
  integer                   :: natoms = 0
  character(6), allocatable :: type(:), atomnm(:), altloc(:), resnm(:), ch(:), resic(:), segnm(:), elem(:)
  integer, allocatable      :: atomn(:), resn(:)
  real, allocatable         :: coor(:,:), occup(:), bfac(:), charge(:)
end type pdb_frame
```
**Legend:**  
type    = record_type  
atomn   = atom_serial_number  
atomnm  = atom_name  
altloc  = alternate_location_indicator  
resnm   = residue_name  
ch      = chain_identifier  
resn    = residue_sequence_number  
resic   = residue_insertion_code  
coor    = x, y, z coordinates  
occup   = occupancy  
bfac    = beta_factor  
elem    = element_name  
charge  = element_partial_charge  
segnm   = segment_identifier  

### `xtc_t` type definition
For more information please read [XTC "manual"](http://manual.gromacs.org/archive/5.0.3/online/xtc.html). For any additional information take a crash course in C/C++ and start reading the original code, because the documentation is basically non-existent. And even with that I wish you best of luck.

#### Structure
```Fortran
type xtc_frame
  integer           :: natoms
  integer           :: step
  real              :: prec
  real              :: time
  real              :: box(3,3)
  real, allocatable :: coor(:,:)
end type xtc_frame
```

### `trr_t` type definition
For more information please read [TRR "manual"](http://manual.gromacs.org/archive/5.0.3/online/trr.html). Same here... good luck.

#### Structure
```Fortran
type xtc_frame
  integer           :: natoms, step
  real              :: lambda, time, box(3,3)
  real, allocatable :: coor(:,:), vel(:,:), force(:,:)
end type xtc_frame
```

### `dcd_t` type definition
For more information please read [DCD manual](https://www.ks.uiuc.edu/Research/namd/2.9/ug/node11.html).  
**NOTE:** DCD file type has some additional data stored in `dcd_t` definition.

#### Structure
```Fortran
type dcd_frame
  integer           :: natoms = 0
  real, allocatable :: coor(:,:)
  real              :: box(3,3) = 0.000
end type dcd_frame

type dct_t
  integer                       :: start_time, every_time, end_time
  real                          :: timestep
  character(80), allocatable    :: remarks(:)
  integer                       :: nframes
  type(dcd_frame), allocatable  :: frame(:)
end type dcd_t
```

### `cub_t` type definition
For more information please read [CUBE manual](http://paulbourke.net/dataformats/cube/).
**NOTE:** CUBE file is different from other file formats as data is not presented as coordinates but rather as voxel grid.

**IMPORTANT**  
Note loop order in CUBE format (it is 'inverted') *i.e.* `cube%grid(z,y,x)`.  


#### Example
```
CPMD CUBE FILE.
OUTER LOOP: X, MIDDLE LOOP: Y, INNER LOOP: Z
    3    0.000000    0.000000    0.000000
   40    0.283459    0.000000    0.000000
   40    0.000000    0.283459    0.000000
   40    0.000000    0.000000    0.283459
    8    0.000000    5.570575    5.669178    5.593517
    1    0.000000    5.562867    5.669178    7.428055
    1    0.000000    7.340606    5.669178    5.111259
 -0.25568E-04  0.59213E-05  0.81068E-05  0.10868E-04  0.11313E-04  0.35999E-05
      :             :             :           :            :            :
      :             :             :           :            :            :
      :             :             :           :            :            :
        In this case there will be 40 x 40 x 40 floating point values
      :             :             :           :            :            :
      :             :             :           :            :            :
      :             :             :           :            :            :
```

#### Structure
```Fortran
type cub_atom
  integer :: z ! Atomic number
  real    :: pcharge ! Partial charge
  real    :: coor(3) ! Coordinates
end type cub_atom

type cub_t
  character(256)              :: comment(2)
  integer                     :: natoms
  type(cub_atom), allocatable :: atom(:)
  real                        :: origin(3)
  real                        :: voxel(3,3)
  integer                     :: nx, ny, nz
  real, allocatable           :: grid(:,:,:)
end type cub_t
```

### `file_t` type definition
> "One object to rule them all, one object to find them,  
> one object to bring them all, and in the darkness bind them."

The `file_t` object combines all other molecular coordinate files (.gro, .pdb, .xyz, .dcd, and .xtc) into one simple to use API. **NOTE:** As this is not yet fully implemented it currently only supports one frame at the time in order to be more compatible with OpenMP programming ("one thread" = "one frame" paradigm). This means that `read()` method does not work, only `read_next()` and `skip_next()`.

#### Structure
```Fortran
type file_t
  integer           :: natoms
  real              :: box(3,3)
  real, allocatable :: coor(:,:)
end type file_t
```

---------------------

## Supporting file types: General

xslib contains two "supporting file" types:
- [.ndx](http://manual.gromacs.org/archive/5.0.3/online/ndx.html) - GROMACS index file containing user defined sets of atoms.
- [.tpl](#More-INFO) - Our own proprietary file type, containing condensed configuration information (created by [A. Lajovic](https://github.com/alajovic)).

### Supporting file types: Interface

To read any supporting file simply use:
```Fortran
error = obj%read("path/to/file.ext")
```
If you want to create completely new supporting file you first need to allocate the memory. In case of .ndx:
```Fortran
error = obj%allocate([natoms,natoms,...])
! of (only works for .ndx)
error = ndx%allocate(ngroups)
do i = 1, ngroups/ntypes
  error = obj%group(i)%allocate(natoms)
end do
```
The stored data can be written to STDOUT, FILE, or file UNIT by:
```Fortran
! Default write is to STDOUT
error = obj%write()
! or write to file
error = obj%write(FILE="path/to/file.obj")
! or write to file unit (must be opened by user)
error = obj%write(UNIT=unit)
```

### Supporting file types: Structure

### `ndx_t` type definition
For more information please read [NDX manual](http://manual.gromacs.org/archive/5.0.3/online/ndx.html).

#### Example
```
[ System ]
1    2    3    4    5    6    7    8    9   10   11   12   13   14   15
16   17   18   19   20   21   22   23   24   25
...
```

#### Structure
```Fortran
type ndx_group
  character(:), allocatable :: title
  integer                   :: natoms
  integer, allocatable      :: loc(:)
end type ndx_group

type ndx_t
  integer                       :: ngroups
  type(ndx_group), allocatable  :: group(:)
contains
  generic   :: assignment(=)
  generic   :: allocate
  procedure :: read
  procedure :: write
  procedure :: display
end type ndx_t
```
To help with UI use display() utility to display all present index groups:
```Fortran
error = ndx%display()
! Present static index groups:
!  Group  X "System" (xxxxx atoms)
!  ...
```

### `tpl_t` type definition
For more information please read format definition below.

#### Example
```
# Example of two component system
side 1.234 1.234 1.234
moltype 2

molecule 4 400  #EtOH
H   0.435  1
O  -0.700  1
CH2 0.265  0
CH3 0.000  0

molecule 3 100  #Water
O
H
H
```

#### Structure
```Fortran
type tpl_frame
  integer               :: natoms, nmol
  integer, pointer      :: id(:)
  character(3), pointer :: name(:)
  real, pointer         :: pcharge(:)
end type tpl_frame

type tpl_file
  real                         :: box(3)
  integer                      :: ntypes
  type(tpl_frame), allocatable :: type(:)
  integer                      :: natoms
  ! Use for sequential data access
  integer, pointer             :: id(:)
  character(3), pointer        :: name(:)
  real, pointer                :: pcharge(:)
end type tpl_file
```

#### More INFO
The .tpl file consists of three directives:  
- `side` (optional) directive contains information about the simulation box size. If cubic box is used only one box-side needs to be present. Units of box side must be the same as in accompanying molecular file. **NOTE:** Setting side any value (other than 0.) usually overrides the configuration box side (depends on implementation).  
- `moltype` (optional) specifies how many *molecule* directives are present in the file.  
- `molecule` directive must be specified along with **number of atoms** (in molecule) and **number of molecules**. This directive is followed by description of each particle: *name*, *pcharge*, and *id*.
  - `Name` (3-characters) parameter is required and provides general description of the particle,
  - `pcharge` (optional, float) descriptor contains partial-charge of particle, and
  - `id` (optional, integer) descriptor denotes particle ID.  

Everything after `#` character is threated as comment. Empty lines and excess spaces are ignored.  

Once file is read you can access the data in two ways:  
- directly for each molecule type,  
- or sequentially for all molecule types at the same time.  

Example (both cases yield same result):
```Fortran
! Sequential access
do i = 1, tpl%natoms
  write (*,*)  tpl%name(i), tpl%pcharge(i), tpl%id(i)
end do

! or direct access
do n = 1, tpl%ntypes
  do i = 1, tpl%type(n)%natoms
    write (*,*) tpl%type(n)%name(i), tpl%type(n)%pcharge(i), tpl%type(n)%id(i)
  end do
end do
```
**NOTE:** Data is stored using pointers; This means by changing the data in "direct access" also changes the data in "sequential access".

---------------------

## Data file types

### `pdh_t` type definition
[PDH file](http://goldenberg.biology.utah.edu/downloads/usTooDocs_Sept2012.pdf) format was created by O. Glatter *et al.* for storing Small-Angle X-Ray Scattering (SAXS) data. It is generally accepted as standard format by the scattering community.

#### Example
```
COMMENT LINE                                                 
KEY1 KEY2 ....                                                                               
      100         0         0         0         0         0         0         0
  0.000000E+00   0.000000E+00   0.000000E+00   0.000000E+00   0.000000E+00
  0.000000E+00   0.000000E+00   0.000000E+00   0.000000E+00   0.000000E+00
  1.000000E+00   1.000000E+00  -1.000000E+00
  2.000000E+00   2.000000E+00  -1.000000E+00
  ...
```

#### Interface
Similarly with other data types one can read file with:
```Fortran
error = pdh%read("path/to/file.pdh")
```
Copy data with:
```Fortran
type(pdh_t) :: pdh, cpy
cpy = pdh
```
Allocate memory with:
```Fortran
error = pdh%allocate( npoints )
```
Where `npoints` is number of data points, or write with:
```Fortran
! Default is Write to STDOUT
error = pdh%write()
! or write to FILE
error = pdh%write(FILE="path/to/file.pdh")
! or write to file UNIT
error = pdh%write(UNIT=unit)
```

#### Structure
```Fortran
type pdh_t
  character(80)     :: text
  character(4)      :: key_words(16)
  integer           :: int_const(8)
  integer           :: num_points
  real              :: real_const(10)
  real, allocatable :: x(:), y(:), y_error(:)
end type pdh_t
```

### `csv_t` type definition
xslib also includes *crude* implementation of [comma-separated values](https://en.wikipedia.org/wiki/Comma-separated_values) files. Unlike in the rest of the library the `csv_file` objects does NOT contain any data; only procedures. The data is obtained as returning argument to function call. To read .csv file use:
```Fortran
type(csv_t)               :: csv
integer                   :: error
character(:), allocatable :: header(:)
real, allocatable         :: data(:,:)

error = csv%read( "file.txt", data, header, DELIM="," )
```
similarly, you can write data to STDOUT, FILE, or file UNIT by:
```Fortran
! Default write is to STDOUT
error = csv%write( data, header )
! or write to FILE
error = csv%write( data, header, FILE="file.csv" )
! or write to file UNIT
error = csv%write( data, header, UNIT=unit )
```
**NOTE:** Default delimiter is `,` (as in **comma**-separated values).

#### Example
```
this,is,header
...
0.000,-1.000,0.000
1.000,0.000,0.000
...
```

#### Structure
```Fortran
type csv_t
contains
  procedure, nopass :: read
  procedure, nopass :: write
end type csv_t
```
```Fortran
integer function csv_read( file, data, header, delim )
  character(*), intent(in)            :: file
  real, allocatable, intent(inout)    :: data(:,:)
  character(*), optional, intent(in)  :: delim
  character(:), optional, allocatable, intent(inout) :: header(:)
  ! Got any more of them attributes?
```
```Fortran
integer function write( data, header, unit, file, delimiter )
  real, intent(in)                    :: data(:,:)
  character(*), optional, intent(in)  :: header(:), file, delim
  integer, optional, intent(in)       :: unit
```

---------------------
# xslib utility modules

## xslib
Master module *i.e.* it contains all other modules. Additionally it contains information about the library itself.

### `xslibInfo()`
Returns xslib library version and compile date in format: "vX.Y.Z -- MMM DD YYYY HH:MM:SS".  
```Fortran
function xslibInfo()
  character(:), allocatable :: xslibinfo
```

### `xslibAbout()`
Writes more extensive xslib library information along with license info to STDOUT.  
```Fortran
subroutine xslibAbout()
```

## xslib_cstring
Module containing routines for string manipulations.

### `str()`
Transform SCALAR or ARRAY of any kind to character of shortest possible length. Optionally output format can be defined with `FMT` argument. In case of ARRAY a custom `DELIMITER` can be defined (default is " ").
```Fortran
function str( value, fmt ) result( string )
  class(*)                  :: value
  character(*), optional    :: fmt
  character(:), allocatable :: string
```
```Fortran
function str( array, fmt, delim ) result( string )
  class(*)                  :: array(:)
  character(*), optional    :: fmt, delim
  character(:), allocatable :: string
```

### `smerge()`
Merge two arbitrarily long strings based on logical mask.  
```Fortran
character(:) function smerge( tsource, fsource, mask )
  allocatable               :: smerge
  character(*), intent(in)  :: tsource, fsource
  logical, intent(in)       :: mask
```

### `toLower()` and `toUpper()`
Returns string to all lower or all upper case, respectively.  
Example: "This IS foo BAR." &rarr; "this is foo bar." / "THIS IS FOO BAR."
```Fortran
character(:) function toLower( string )
  allocatable               :: toLower
  character(*), intent(in)  :: string
```
```Fortran
character(:) function toUpper( string )
  allocatable               :: toUpper
  character(*), intent(in)  :: string
```

### `stripComment()`
Removes all characters trailing comment sign `cmt`.  
Example: "text #comment" &rarr; "text "
```Fortran
character(:) function stripComment( string, cmt )
  allocatable               :: stripComment
  character(*), intent(in)  :: string, cmt
```

### `isWord()`
Check if string contains any ASCII letters.
```Fortran
logical function isWord( string )
  character(*), intent(in)  :: string
```

### `isNumber()`
Check if string is number that can be interpreted.
```Fortran
logical function isNumber( string )
  character(*), intent(in)  :: string
```

### `replaceText()`
Replaces `text` with `rep` within `string`.  
Example: "This is bad." &rarr; "This is good."
```Fortran
character(:) function replaceText( string, old, new )
  allocatable               :: replaceText
  character(*), intent(in)  :: string, old, new
```

### `getColor()` and `setColor()`
Add some colours in your life and set terminal colour.  
ATTR=attribute, FG=foreground, BG=background  
**Attributes:** bold, dim, underline, blink, reverse and hidden  
**Colors:** black, red, yellow, blue, magenta, cyan, light grey, dark grey, light red, light yellow, light blue, light magenta, light cyan and white.  
Example: "This is red" &rarr; <font color="red">"This is red."</font>
```Fortran
! NOTE: Max len. of output string is 11; "e[x;xx;xxxm"
character(:) function getColor( attr, fg, bg )
  allocatable                         :: getColor
  character(*), intent(in), optional  :: attr, fg, bg
```
```Fortran
character(:) function setColor( string, attr, fg, bg )
  allocatable                         :: setColor
  character(*), intent(in)            :: string
  character(*), intent(in), optional  :: attr, fg, bg
```

### `strtok()`
Tokenize string i.e. breaks string into a series of tokens using the delimiter. Works in same way as ANSI C strtok, except instead of NULL feed char(0)
```Fortran
character(:) function strtok( string, delim )
  allocatable               :: strtok
  character(*), intent(in)  :: string
  character(*), intent(in)  :: delim
```

### `cnttok`
Count number of tokens in string.
```Fortran
integer function cnttok( string, delim )
  implicit none
  character(*), intent(in)  :: string
  character(*), intent(in)  :: delim
```

### `baseName()`
Returns base name of file.  
Example: "path/to/file.txt" &rarr; "file"
```Fortran
character(:) function baseName( name )
  allocatable               :: basename
  character(*), intent(in)  :: file
```

### `pathName()`
Returns path name of file.  
Example: "path/to/file.txt" &rarr; "path/to/"
```Fortran
character(:) function pathName( name )
  allocatable               :: pathname
  character(*), intent(in)  :: file
```

### `extension()`
Returns file extension.    
Example: "path/to/file.txt" &rarr; "txt"
```Fortran
character(:) function extension( file )
  allocatable               :: extension
  character(*), intent(in)  :: file
```

### `backup()`
Checks if "file.txt" exists and renames it to "#file.txt.n#" (n=1,2,..).
```Fortran
subroutine backup( file, status )
  character(*), intent(in)       :: file
  integer, intent(out), optional :: status
```

### `progressBar()`
Writes gradual progress bar to STDOUT (on the same output line). The size of the progress bar is determined by (optional) `size` variable. Write to STDOUT is "forbidden" until progress reaches 1.0.  
Example: " 50.0% |############------------|"
```Fortran
subroutine progressBar( x, size )
  real, intent(in)              :: x
  integer, intent(in), optional :: size
```

## xslib_vector
Module containing routines for scalar/vector operations.

**NOTE:** All functions are by default single precision `dp = REAL32`. If you are in need of double precision routines change `dp = REAL64` and recompile.

### `cross()`
Returns vector product: v &times; u.
```Fortran
real(dp) function cross( v, u )
  dimension             :: cross(3)
  real(dp), intent(in)  :: v(3), u(3)
```

### `rotate()`
[Rotates](https://en.wikipedia.org/wiki/Rotation_matrix) vector `x` around `vector` by `angle` in [rad].   
```Fortran
real(dp) function rotate( x, vector, angle )
  dimension            :: rotate(3)
  real(dp), intent(in) :: x(3), vector(3), angle
```

### `minImg()`
Returns reduced coordinates according to [minimal image convention](https://en.wikipedia.org/wiki/Periodic_boundary_conditions).  
_I.e. a=a-box*floor(a/box+0.5) <.OR.> a=a-sign(box/2,r-box/2)-sign(box/2,r+box/2)_  
```Fortran
real(dp) function minImg( r, box )
  dimension             :: minImg(3)
  real(dp), intent(in)  :: r(3), box(3)
```

### `getDistance()`
Returns distance between two points.  
**NOTE:** This is (awkward) alternative to `norm2(a-b)`; use the latter if possible.
```Fortran
real(dp) function getDistance( a, b )
  real(dp), intent(in) :: a(3), b(3)
```

### `getAngle()`
Returns angle between three points (A-B-C).
```Fortran
real(dp) function getAngle( a, b, c )
  real(dp), intent(in) :: a(3), b(3), c(3)
```

### `getDihedral()`
Return [dihedral angle (theta)](https://en.wikipedia.org/wiki/Dihedral_angle) between four points (A-B-|-C-D).
```Fortran
real(dp) function getDihedral( a, b, c, d )
  real(dp), intent(in) :: a(3), b(3), c(3), d(3)
```

### `deg2rad()` and  `rad2deg()`
Translates angle from degrees to radians and back.
```Fortran
real(dp) function deg2rad (deg)
  real(dp), intent(in) :: deg
```
```Fortran
real function rad2deg (rad)
  real(dp), intent(in) :: rad
```

### `crt2sph()`, `sph2cart()`, `crt2cyl()`, and `cyl2crt()`
Coordinate system transformations, where:  
*crt* = cartesian - x,y,z  
*sph* = spherical - r,theta,phi  
*cyl* = cylindrical - r,theta,z  
```Fortran
function(dp) crt2sph( crt ) result( sph )
  real(dp), intent(in)  :: crt(3)
  real(dp), intent(out) :: sph(3)
```
```Fortran
function(dp) sph2crt( sph ) result( crt )
  real(dp), intent(in)  :: sph(3)
  real(dp), intent(out) :: crt(3)
```
```Fortran
function(dp) crt2cyl( crt ) result( cyl )
  real(dp), intent(in)  :: crt(3)
  real(dp), intent(out) :: cyl(3)
```
```Fortran
function(dp) cyl2crt( cyl ) result( crt )
  real(dp), intent(in)  :: cyl(3)
  real(dp), intent(out) :: crt(3)
```

### `variance()`
Calculates [on-line variance](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance) for any real scalar or array, where `n` is data counter for variance aggregate. Works for both single and double precession (`dp`).  
**NOTE:** True variance must be "corrected" after calculation as `var = merge( var/(n-1), -1.0, n>1 )`
```Fortran
subroutine variance( value, mean, var, n )
  real(dp), intent(in)    :: value
  real(dp), intent(inout) :: mean, var
  integer, intent(in)     :: n          
```
```Fortran
subroutine variance( value, mean, var, n )
  real(dp), intent(in)    :: value(:)
  real(dp), intent(inout) :: mean(:), var(:)
  integer, intent(in)     :: n        
```

### `lerp()`
Linear interpolation. Works for both single and double precision.
```Fortran
real(dp) function lerp( v0, v1, x )
  real(dp), intent(in) :: v0, v1, x
```

### `findKClosest()`
Return `k` closest elements to `val` in `array`. Works for both single and double precision.
```Fortran
function findKClosest( val, array, k )
  integer(dp), intent(in)  :: val, array(:)
  integer, intent(in)      :: k
  integer(dp), intent(out) :: findKClosest(k)
```
```Fortran
function findKClosest( val, array, k )
  real(dp), intent(in)  :: val, array(:)
  integer, intent(in)   :: k
  real(dp), intent(out) :: findKClosest(k)
```

### `findKClosest()`
Return `k` closest elements to `val` in `array(:)`. Works for both single and double precision.
```Fortran
function findKClosest( val, array, k )
  integer(dp), intent(in)  :: val, array(:)
  integer, intent(in)      :: k
  integer(dp), intent(out) :: findKClosest(k)
```
```Fortran
function findKClosest( val, array, k )
  real(dp), intent(in)  :: val, array(:)
  integer, intent(in)   :: k
  real(dp), intent(out) :: findKClosest(k)
```

### `findCrossOver()`
Returns the cross over point of `array` i.e. the point at which elements are smaller than or equal to `x` and after which are greater than `x`.  Works for both single and double precision.
```Fortran
integer(dp) function findCrossOver( array, low, high, x )
 integer(dp), intent(in) :: array(:), x
 integer, intent(in)     :: low, high
```
```Fortran
integer(dp) function findCrossOver( array, low, high, x )
 real(dp), intent(in) :: array(:), x
 integer, intent(in)  :: low, high
```

### `swap()`
Swap values of A and B. Works for both single and double precision.
```Fortran
subroutine swap( a, b )
  class(*), intent(inout) :: a, b
```

## xslib_time
Module containing routines for time manipulation (sadly, not time travel).

### `wtime()`
Return high precision time in sec.
```Fortran
real(REAL64) function wtime()
```

### `write_wtime()`
Transforms time in seconds to string in format: *"ddd:hh:mm:ss.sss"*. Use with `OMP_get_wtime()` or `wtime()`.  
```Fortran
character(16) function write_wtime( time )
  real(REAL64), intent(in) :: time
```

### `msleep()`
Suspend execution for millisecond intervals.
```Fortran
integer function msleep( time )
  integer, intent(in) :: time
```

## xslib_list
Module containing contains my implementation of unlimited polymorphic double-linked list (DLL).
```Fortran
type list_t
contains
  ! List add/ remove elements
  procedure :: add
  procedure :: insert
  procedure :: pop
  procedure :: remove
  procedure :: clear
  ! Check list
  procedure :: isEmpty
  ! Basic I/O
  generic   :: write(formatted)
  generic   :: read(formatted)
  ! Moving around the list
  procedure :: next
  procedure :: prev
  procedure :: start
  procedure :: end
  ! Execute command on linked list
  procedure :: execute
  ! Print all elements to STDOUT
  procedure :: printAll
end type list_t
```
In order to use this list you must first define it Fortran declaration:
```Fortran
type(list_t) :: list
```
This linked list uses unlimited polymorphic variables `class(*)`, therefore, you can store any *type* (not class) of item.

To add item to the list use add or insert functions. `add` functions adds item to the end of list, where as `insert` adds an item before current pointed by the list.
```Fortran
call list%add( 1 )
! List: 1
call list%insert( 0 )
! List: 0, 1
```
To remove item from the list use either pop or remove functions. `pop` removes the last item on the list, where as `remove` removes current item on the list.
```Fortran
! [] - indicates where the list is currently pointing
! list: 1, [2], 3, 4
call list%pop()
! list: 1, [2], 3
call list%remove()
! list: [1], 3; List now points to the item previous from deleted item.
```
To remove all items form the list use:
```Fortran
call list%clear()
! list: *empty*
```
To retrive item from the list use assignment(=):
```Fortran
call list%add(1)
int = list
```
To move around the list use start, next, prev, end:
```Fortran
call list%start()
! list: [1], 2, 3, 4
call list%next()
! list: 1, [2], 3, 4
call list%prev()
! list: [1], 2, 3, 4
call list%end()
! list: 1, 2, 3, [4]
```
To check if there are any items on the list use:
```Fortran
if ( list%isEmpty() ) then
  ! ...
```
This implementation has defined basic I/O, so you can read/write to/from list directly:
```Fortran
write (*,*) list
! or
read (unit,*) list
```
Reading to the list adds new item to the end of the list. **NOTE:** When calling read the new item is always stored as `character(:)`.

Use execute functionality to preform operation on all items on the list:
```Fortran
! Used function must have this interface
interface
  subroutine addOne( value )
    class(*), intent(inout) :: value
  end subroutine addOne
end interface

call list%execute( addOne )
```

## xslib_xmalloc
Module containing custom allocation/reallocation routines.  
**NOTE:** Only supports arrays of rank up to 2 AND variables of type INT32, INT64, REAL32, and REAL64.  
```Fortran
integer, allocatable :: array(:), array2d(:,:)
integer              :: error
character(128)       :: errmsg

error = xmalloc( array, [2] )
error = xcalloc( array2d, [2,2], ERRMSG=errmsg )
```

### `xmalloc()`
Allocates a block of memory for an array of num. elements. The content of the newly allocated block of memory is not initialized, remaining with indeterminate values.  
```Fortran
integer function xmalloc( object, spec, errmsg )
  type(*), allocatable, intent(inout) :: object(..)
  integer, intent(in)                 :: spec(:)
  integer, intent(out), optional      :: errmsg
```

### `xcalloc()`
Allocates a block of memory for an array of num. elements, and initializes all its bits to zero.  
```Fortran
integer function xcalloc( object, spec, errmsg )
  type(*), allocatable, intent(inout) :: object(..)
  integer, intent(in)                 :: spec(:)
  integer, intent(out), optional      :: errmsg
```

### `xrealloc()`
Changes the size of memory block. The content of the memory block is preserved up to the lesser of the new and old sizes, even if the block is moved to a new location. If the new size is larger, the value of the newly allocated portion is indeterminate.
```Fortran
integer function xrealloc( object, spec, errmsg )
  type(*), allocatable, intent(inout) :: object(..)
  integer, intent(in)                 :: spec(:)
  integer, intent(out), optional      :: errmsg
```

## xslib_error
Module containing routines for error/warning handling.  

### `error()` and `error_()`
Write error message to ERROUT and terminates the program.  
Example: "<font color="red">ERROR:</font> This is error message."  
```Fortran
subroutine error( message )
  character(*), intent(in) :: message
```
or extended error message.  
Example: "main.f90:10: <font color="red">ERROR:</font> This is error message."  
```Fortran
#define error(x) error_( x, __FILE__, __LINE__ )
subroutine error_( message, file, line )
  character(*), intent(in)  :: message, file
  integer, intent(in)       :: line
```

### `warning()` and `warning_()`
Write warning message to ERROUT.  
Example: "<font color="Fuchsia">WARNING:</font> This is warning message."  
```Fortran
subroutine warning( message )
  character(*), intent(in) :: message
```
or extended warning message.  
Example: "main.f90:10: <font color="Fuchsia">WARNING:</font> This is warning message."  
```Fortran
#define warning(x) warning_( x, __FILE__, __LINE__ )
subroutine warning_( message, file, line )
  character(*), intent(in)  :: message, file
  integer, intent(in)       :: line
```

### `xslibErrMsg()`
Returns error message for xslib error code.
```Fortran
character(:) function xslibErrMsg( errnum )
  allocatable         :: xslibErrMsg
  integer, intent(in) :: errnum
```
**Legend:**    
0 - OK  
1 - Cannot read/write file header.  
2 - Cannot read/write variable of kind: character.  
4 - Cannot read/write variable of kind: double.  
5 - Cannot read/write variable of kind: integer.  
6 - Cannot read/write variable of kind: float.  
7 - Cannot read/write variable of kind: unsigned integer.  
8 - Cannot read/write coordinate data.  
9 - Cannot close file.  
10 - Cannot read/write magic number.  
11 - Memory allocation failure.  
12 - End of file.  
13 - File not found.  
14 - File unit/pointer not assigned.  
15 - Wrong number of atoms.  
16 - Unknown error.  

### `assert() and assert_()`
If any of the argument expressions is false, a message is written to the STDERR and abort is called, terminating the program execution.  
Example: "Assertion failed at [i,j]!"
```Fortran
subroutine assert( expression )
  logical, intent(in) :: expression(..)
```
or extended abort message:  
Example: "main.f90:10: Assertion failed at [i,j]!"
```Fortran
subroutine assert( expression, file, line )
  logical, intent(in)       :: expression(..)
  character(*), intent(in)  :: file
  integer, intent(in)       :: line
```
----------------------------------------------

## Notes
For more examples of xslib usage check [`examples`](../examples).

<!-- Most of the routines in xsLib won't allow you to do stupid things and will either promptly stop you from hurting yourself by terminating the program or will politely protest with a warning (and most likely crash afterwards). If don't like being told what you can and can't do simply delete everything in `error` and `warning` subroutines located in *src/xslib_common.f90*. -->
