# API

## Table of contents

- [Molecular file types: General](#molecular-file-types-general)
- [Molecular file types: File specific data](#molecular-file-types-file-specific-data)
  - [`pdb_file` format](#pdb_file-format)
  - [`gro_file` format](#gro_file-format)
  - [`trj_file` format](#trj_file-format)
  - [`xyz_file` format](#xyz_file-format)
  - [`frame_file` object format](#frame_file-object-format)
- [Supporting file types](#supporting-file-types)
  - [`ndx_file` format](#ndx_file-format)
  - [`tpl_file` format](#tpl_file-format)
- [Data file types](#data-file-types)
  - [`pdh_file` format](#pdh_file-format)
  - [`csv_file` format](#csv_file-format)
- [Functions and Subroutines](#functions-and-subroutines)
  - [`xslibINFO`](#xslibinfo)
  - [`str()`](#str)
  - [`error()` and `warning()`](#error-and-warning)
  - [`newUnit()`](#newunit)
  - [`cross()`](#cross)
  - [`minImg()`](#minimg)
  - [`getDistance()`](#getdistance)
  - [`getAngle()`](#getangle)
  - [`getDihedral()`](#getdihedral)
  - [`deg2rad()` and  `rad2deg()`](#deg2rad-and-rad2deg)
  - [`crt2sph()`, `sph2cart()`, `crt2cyl()`, and `cyl2crt()`](#crt2sph-sph2cart-crt2cyl-and-cyl2crt)
  - [`timeStamp()`](#timestamp)
  - [`getTime()`](#gettime)
  - [`elapsedTime()`](#elapsedtime)
  - [`variance()`](#variance)
  - [`baseName()`](#basename)
  - [`extension()`](#extension)
  - [`stripComment()`](#stripcomment)
  - [`backup()`](#backup)
  - [`nextFreeName()`](#nextfreename)
  - [`isEmpty()`](#isempty)
  - [`isWord()`](#isword)
  - [`replaceText()`](#replacetext)
  - [`tab2space()`](#tab2space)
  - [`toLower()` and `toUpper()`](#tolower-and-toupper)
  - [`progressBar()`](#progressbar)
  - [`linest()`](#linest)
- [Notes](#notes)

## Molecular file types: General

XsLib supports the use of multiple molecular coordinate files:  
- [.gro](http://manual.gromacs.org/archive/5.0.3/online/gro.html) - GROMACS default coordinate file.  
- [.pdb](https://www.rcsb.org/pdb/static.do?p=file_formats/pdb/index.html) - Protein Data Bank which is most commonly used and widely spread molecular coordinate file.
- [.xyz](https://en.wikipedia.org/wiki/XYZ_file_format) - Simple *"name,x,y,z"* file that gets the job done in many cases.
- [.trr/.xtc](https://en.wikipedia.org/wiki/XYZ_file_format) - GROMACS binary trajectory and compressed trajectory files, respectively.

They are implemented as derived type variables consisting of data and procedure bound to that type. They can be used with `type(obj_file)`, for example:
```fortran
type(xyz_file) :: xyz
type(pdb_file) :: pdb
type(gro_file) :: gro
type(trj_file) :: trj !.xtc and .trr
```
**NOTE:** For procedures that are common to all derived types, the derived type will be refered to as `obj`.  

All molecular coordinate files are generally structured in same manner *i.e.* each file is composed of multiple frames. These frames stored as "frame arrays" - `obj%frameArray(n)%...`. Each frame array is composed simulation box size, number of atoms, and their coordinates - `...%box(3)`, `...%natoms`, `...%coor(3,natoms)`, respectively, and additional data that is specific to each file type.

The general API is the same for all derived types. Each file can be opened with:
```fortran
call obj%open("path/to/file.obj")
```
Each frame can be read individually or in multiples with:
```fortran
int = obj%read_next(n)
```
where `n` is optional parameter denoting the number of frames to read. Returning argument (in this case *int*) contains the actual number of frames read (0 if no frames could be read). If multiple files were read the data can accesed as `obj%frameArray(n)%...`. Alternatively, entire file can be read at once by using:
```fortran
call obj%read("path/to/file.obj")
```
Once you are done with reading the file it should be closed with:
and closed with:
```fortran
call obj%close()
```
Closing the file does not affect the data stored in `obj` (*i.e.* it does not deallocate the data).
<!-- ======================================================================================================================== -->
After calling `obj%read()` or `obj%read_next()` every atom's coordinates are accessible as `obj%frameArray(n)%coor(:,:)`, for example:
```Fortran
! For 1st frame write the 3rd particles's y-coordinate
write (*,*) obj%frameArray(1)%coor(2,3)
```
**NOTE:** Fortran language uses row-major indexing *i.e.* `array(row, column)` and that convention is retained here.

If you want to inquire the box size and number of atoms prior to actually reading the file it can be done with:
```fortran
integer :: natoms
real    :: box(3)

box = obj%box()
natoms = obj%natoms()
```
**NOTE:** Calling this function retains your current position in file.

If you want to create your own data, you have to firstly allocate frameArray and than each frame individually:

```fortran
! Create 10 frames consisting of 100 atoms
nframes = 10
natoms = 100
call obj%allocate(nframes)
do i = 1, nframes
  call obj%frameArray(i)%allocate(natoms, INITIALIZE=.true.)

end do
```
The optional `INITIALIZE` argument formats the newly allocated data to empty values or 0. Alternatively, it can be done with `...%initialize()` procedure.
The stored data can be printed to file UNIT, FILE or STDOUT:
```fortran
call obj%write(UNIT=unit)
! or
call obj%write(FILE="path/to/file.obj")
! or
call obj%write()
```
**NOTE:** This does not work for `trj_file`.  

To deallocate the data use:    
```fortran
call obj%frameArray(1)%deallocate()
! and
call obj%deallocate()
```

## Molecular file types: File specific data

Each file type contains additional information (unique to format) that is listed down bellow:

### `pdb_file` format

For more information on variables please read [PDB Manual](https://www.rcsb.org/pdb/static.do?p=file_formats/pdb/index.html).

```fortran
type, private :: pdb_frame
  real                       :: box(3)
  integer                    :: natoms
  character*20, allocatable  :: record_type(:), atom_name(:), alt_loc_indicator(:), residue_name(:), chain_identifier(:)
  character*20, allocatable  :: res_insert_code(:), segment_identifier(:), element_symbol(:)
  integer, allocatable       :: atom_serial_number(:), residue_sequence_number(:)
  real, allocatable          :: coor(:,:)
  real, allocatable          :: occupancy(:), temp_factor(:), charge(:)

type pdb_file
  integer                       :: nframes
  type(pdb_frame), allocatable  :: frameArray(:)
contains
  procedure  :: open
  procedure  :: close
  procedure  :: allocate
  procedure  :: read_next
  procedure  :: read
  procedure  :: box
  procedure  :: natoms
  procedure  :: write
```

Because .pdb file contains a lot (and I mean really A LOT) of usually "unnecessary" data an option to read only coordinates can used:
```fortran
stat = pdb%read_next(ONLYCOOR=.true.)
```
This is useful when reading speed is a priority or you need to save on RAM.

### `gro_file` format

For more information please read [GRO manual](link).

```fortran
type, private :: gro_frame
  character*512             :: title
  real                      :: time
  integer                   :: natoms
  real                      :: box(3)
  integer, allocatable      :: res_num(:), atom_num(:)
  character*5, allocatable  :: res_name(:), atom_name(:)
  real, allocatable         :: coor(:,:), vel(:,:)

type gro_file
  integer                       :: nframes, framesLeft
  type(gro_frame), allocatable  :: frameArray(:)
contains
  procedure  :: open
  procedure  :: close
  procedure  :: allocate
  procedure  :: read_next
  procedure  :: read
  procedure  :: write
  procedure  :: natoms
  procedure  :: box
```

To read only coordinates use:
```fortran
stat = gro%read_next(ONLYCOOR=.true.)
```

### `trj_file` format

This is a direct port of gmxfort library so for more detailed API please read [this](https://github.com/wesbarnett/libgmxfort).

```fortran
type, private :: trj_frame
  real(C_FLOAT), allocatable  :: xyz(:,:)
  integer(C_INT)              :: steep
  real(C_FLOAT)               :: box(3,3), prec, time

type, public :: trj_file
  type(xdrfile), pointer        :: xd
  type(trj_frame), allocatable  :: frameArray(:)
  type(ndx_file)                :: ndx
  integer                       :: NFRAMES, FRAMES_REMAINING
  integer                       :: NUMATOMS, N
  logical                       :: read_only_index_group
contains
  procedure :: open
  procedure :: read
  procedure :: read_next
  procedure :: close
  procedure :: x
  procedure :: natoms
  procedure :: box
  procedure :: time
  procedure :: step
```

### `xyz_file` format

More information about the format can be read [here](link).

```fortran
type, private :: xyz_frame
  integer                   :: natoms
  character*512             :: comment
  character*5, allocatable  :: name(:)
  real, allocatable         :: coor(:,:)
  real                      :: box(3)  !FUN FACT: .xyz file does not actually contain box info
```

### `frame_file` object format

> "One object to rule them all, one object to find them,
  one object to bring them all, and in the darkness bind them."

`frame_file` object combines all other molecular coordinate files into one simple to use API.

```Fortran
type frame_data
  integer            :: natoms
  real               :: box(3)
  real, allocatable  :: coor(:,:)
contains
  procedure :: open
  procedure :: read_next
  procedure :: close
  procedure :: get_nframes
  procedure :: get_box
```
To open file use:
```fortran
call frame%open("path/to/file.obj") ! .gro, .pdb, .xyz, .trr or .xtc
```
The file can be read with:
```fortran
stat = frame%read_next()
```
Additionally, the data on box size and num. of atoms can be obtained with:
```fortran
box = frame%get_box()
natoms = frame%get_natoms()
```
**NOTE:** Calling this function retains your current position in file.

And finally when done file is closed with:
```fortran
call frame%close()
```

## Supporting file types

XsLib supports two "supporting" file types:
- [.ndx](http://manual.gromacs.org/archive/5.0.3/online/ndx.html) - GROMACS index file containing user defined sets of atoms.
- .tpl - Our own proprietary file type, created by [A. Lajovic](https://github.com/alajovic).

### `ndx_file` format

GROMACS index (.ndx) file containing user defined sets of atoms.

To open and read index file use:
```Fortran
call ndx%read("path/to/file.ndx")
```
Data can be accessed as:
```Fortran
do n = 1, ndx%ngroups
  write (*,"(3(a,i0))") "Group ", n, " '"//ndx%group(n)%title//"' (", &
  ndx%group(n)%numatoms), " atoms)"
  do i = 1, ndx%group(n)%numatoms
    write (*,*) ndx%group(n)%loc(i)

  end do
end do
```
To write data to file UNIT, FILE or STDOUT use:
```fortran
call ndx%write(UNIT=unit)
! or
call ndx%write(FILE="path/to/file.ndx")
! or
call ndx%write()
```

### `tpl_file` format

Template (.tpl) file contains condensed information regarding the particles in "coordinate files" and it is meant to support them. It consist of three directives:  
- `side` (optional) directive contains information about the simulation box size. If cubic box is used only one box-side needs to be present. Units of box side must be the same as supporting file.  
- `moltype` (optional, legacy) specifies how many *molecule* directives are present in the file.  
- `molecule` directive must be specified along with **num. of atoms** (in molecule) and **num. of molecules**. This directive is followed by description of each particle.
- `name`, `pcharge`, and `id`. **Name** (3-characters) parameter is required and provides general description of the particle. **pcharge** - (real) descriptor is optional and contains partial-charge of particle. **ID** (integer) descriptor is optional and denotes particle ID.  
<!-- Internally supported particle types are: *H, O, C, N, S, CH, CH2, and CH3* -->

Everything after `#` character is threated as comment. Empty lines and excess spaces are ignored.  
Example:
```
# 80 mol. % mixture of EtOH/Water
side 1.234 5.678 9.100
moltype 2
molecule 4 400 # TraPPE-UA EtOH
H   0.435  1
O  -0.700  2
CH2 0.265  3
CH3 0.000  4
molecule 4 100 # TIP4P water
O   0.0000  2
H   0.5564  1
H   0.5564  1
M  −1.1128  0
```

To read .tpl file use:
```fortran
call tpl%read("path/to/file.tpl")
```
Once file is read you can access the data in two ways: access particle of each molecule type individually or all particles sequentially as "list":
```Fortran
! Access all particles sequentially as list
do i = 1, tpl%natoms
  write (*,*)  tpl%name(i) ! pcharge(i), id(i)

end do
! or access particle of each molecule type
do n = 1, tpl%nTypes
  do i = 1, tpl%type(n)%natoms
    write (*,*) tpl%type(n)%name(i) ! pcharge(i), id(i)

  end do
end do
! Both cases yield same result  
```
**NOTE:** Data is stored by using pointers, which means by changing the data for "each type" also changes the data in "list".

You can print the read data to file UNIT, FILE or STDOUT by using:
```Fortran
! Write to file UNIT
call tpl%write(UNIT=unit)
! or
call tpl%write(FILE="path/to/file.tpl")
! or
call tpl%write()
```

```fortran
type, private :: tpl_frame
  integer              :: natoms, nmol
  integer, pointer     :: id(:)
  character*3, pointer :: name(:)
  real, pointer        :: pcharge(:)

type tpl_file
  integer, private             :: unit
  real                         :: box(3)     ! Simulation box side
  integer                      :: nTypes    ! Number of different types
  type(tpl_frame), allocatable :: type(:)    ! Type of molecule
  ! Contains actual data
  integer                      :: natoms
  integer, pointer             :: id(:)
  character*3, pointer         :: name(:)
  real, pointer                :: pcharge(:)
contains
  procedure  :: read
  procedure  :: write
```

## Data file types

### `pdh_file` format

PDH file was created by Glatter *et al.* in order to store Small-Angle X-Ray Scattering (SAXS) data. More on the file structure and format can be read [here](link) (page xyz).

To open and read the .pdh file use:
```Fortran
call pdh%read("path/to/file.pdh")
```

The data is stored as:
```fortran
type pdh_file
  character*80      :: text
  character*4       :: key_words(16)
  integer           :: int_const(8)
  integer           :: num_points
  real              :: real_const(10)
  real, allocatable :: x(:), y(:), y_error(:)
```

File can be also written to file UNIT, FILE, or STDOUT
```Fortran
call pdh%write(UNIT=unit)
! or
call pdh%write(FILE="path/to/file.tpl")
! or
call pdh%write()
```

### `csv_file` format
xslib also includes crude implementation of [Comma Separated Value (.csv)](https://en.wikipedia.org/wiki/Comma-separated_values) files. Unlike in the rest of the library the `csv_file` objects does NOT contain any data, but rather it returned/imputed it as argument to subroutine.

You can read the file by using:
```fortran
real, allocatable           :: data(:,:)
character*(n), allocatable  :: header(:)

call csv%read("path/to/file/.csv", data, HEADER=header)
```
**NOTE:** Header and data must be an allocatable variables, where the character length of `HEADER` is arbitrary.

The data can also be written (with custom data delimiter) to file UNIT, FILE or STDOUT:  

```fortran
call csv%write(data(:,:), HEADER=header(:), FILE="out.csv")
! or
call csv%write(data(:,:), HEADER=header(:), UNIT=unit)
! or (here with custom data delimiter)
call csv%write(data(:,:), HEADER=header(:), DELIMITER=";")
```

<!-- ==================================================== -->
<!-- ==================================================== -->
<!-- ==================================================== -->

## Functions and Subroutines

<!-- xslib.F90 -->

### `xslibINFO`
Basic information about xslib library: version, compile date and time.  
*v0.0.0 -- May 18 2018 12:34:56*
```fortran
character*64, parameter  :: xslibINFO
```

<!-- common.f90 -->

### `str()`
Transform scalar of any kind to character of shortest possible length. Optionally output format can be defined with `FMT` argument.
```fortran
interface str
  procedure  :: itoa, i8toa, ftoa, f8toa, ctoa, btoa

function str (value, fmt) result (string)
  integer, real, complex, logical  :: value
  character*(*)                    :: fmt
  character*(:), allocatable       :: string
```

### `error()` and `warning()`
Writes error/warning message to STDERR. Calling `error()` also terminates the program and returns exit status 1 to shell.
*i.e. <name\> ERROR/WARNING - <message\>*  
```fortran
subroutine error (message, name)
  character*(*)            :: message
  character*(*), optional  :: name

subroutine warning (message, name)
  character*(*)            :: message
  character*(*), optional  :: name
```

### `newUnit()`
Finds next free file unit and returns it as `newUnit`. Optionally the output can be stored as `unit`. Use with `open (UNIT=...)`   
**DEPRECATED:** Use `open (NEWUNIT=unit, ...)`
```fortran
integer function newUnit (unit)
  integer, optional, intent(out)  :: unit
```

<!-- ### utilities.f90 ### -->

### `cross()`
Returns vector product: v &times; u.
```fortran
function cross (v, u)
  real, dimmension(3)  :: v, u, cross
```

### `minImg()`
Return reduced coordinates according to [minimal image convention](https://en.wikipedia.org/wiki/Periodic_boundary_conditions).  
_i.e a=a-box*floor(a/box+0.5)_
```fortran
function minImg (r, box)
  real, dimension(3)  :: r, box, minImg
```

### `getDistance()`
Returns distance between two points.  
**NOTE:** This is (awkward) alternative to `norm2(a-b)`; use the latter if possible.
```fortran
function getDistance (a, b) result (distance)
  real, dimension(3)  :: a, b
  real                :: distance
```

### `getAngle()`
Returns angle between three points (A-B-C).
```fortran
function getAngle (a, b, c) result (angle)
  real, dimension(3)  :: a, b, c
  real                :: angle
```

### `getDihedral()`
Return [dihedral angle (theta)](https://en.wikipedia.org/wiki/Dihedral_angle) between four points (A-B-|-C-D).
```fortran
function getDihedral (a, b, c, d) result (dihedral)
  real, dimension(3)  :: a, b, c, d
  real                :: dihedral
```

### `deg2rad()` and  `rad2deg()`
Translates angle from degrees to radians and back.
```fortran
function deg2rad (in) result (out)
   real :: in, out

function rad2deg (in) result (out)
  real :: in, out
```

### `crt2sph()`, `sph2cart()`, `crt2cyl()`, and `cyl2crt()`
Coordinate system transformations, where:  
_crt_ = cartesian (x,y,z),  
_sph_ = spherical (r,theta,phi), and   
_cyl_ = cylindrical (r,theta,z)  
```fortran
function crt2sph (in) result (out)
  real, dimension(3) :: in, out

function sph2cart (in) result (out)
  real, dimension(3) :: in, out

function crt2cyl (in) result (out)
  real, dimension(3) :: in, out

function cyl2crt (in) result (out)
  real, dimension(3) :: in, out
```
<!-- ------------------------------------------------------------------------------ -->

### `timeStamp()`
Returns time and date in format *hh:mm:ss dd-mm-yyyy*.
```fortran
function timeStamp () result (string)
  character*19   :: string
```

### `getTime()`
Function wrapper for `system_clock(COUNT=time)`.
```fortran
function getTime () result (time)
  integer :: time
```

### `elapsedTime()`
Converts time in [ms] or [s] to string in format *ddd:hh:mm:ss.sss*. Use with `system_clock(COUNT=time)`, `getTime()` or `OMP_get_wtime()`.  
**NOTE:** `integer` = [ms] and `real*8` = [s]
```fortran
function elapsedTime (time) result (string)
  integer or real*8  :: time
  character*16       :: string
```

### `variance()`
Calculates [on-line variance](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance) for any real scalar or array, where `n` is data counter for variance aggregate.  
**NOTE:** True variance must be "corrected" after calculation as `var=var/(n-1)`
```fortran
subroutine variance (value, mean, var, n)
  real or real*8  :: value(:),  mean(:), var(:)
  integer         :: n
```
<!-- ------------------------------------------------------------------------- -->

### `baseName()`
Returns base name of file.  
*e.g. "/path/to/file.txt" &rarr; "file"*
```fortran
function baseName (name) result (base)
  character*(*)               :: name
  character*(:), allocatable  :: base
```

### `extension()`
Returns file extension.    
*e.g. "/path/to/file.txt" &rarr; "txt"*
```fortran
function extension (name) result (ext)
  character*(*)               :: name
  character*(:), allocatable  :: ext
```

### `stripComment()`
Removes all characters trailing comment sign `cmt`.  
*e.g. "text #comment" &rarr; "text"*
```fortran
function stripComment (string, cmt) result (strip)
  character*(*)               :: string, cmt
  character*(:), allocatable  :: strip
```

### `backup()`
Renames file if it already exits.    
*e.g. "file.txt" &rarr; "#file.txt.n#" (n=1,2,..)*
```fortran
subroutine backup (file)
  character*(*) :: file
```

### `nextFreeName()`
Returns next free available variation of the file name.  
*e.g. "out.txt" &rarr; "out.n.txt" (n=1,2,..)*
```fortran
function nextFreeName (name) result (freeName)
  character*(*)              :: name
  character(:), allocatable  :: freeName
```

### `isEmpty()`
Check if string is empty ("tab" or "space" do not count).   
**DEPRECATED:** Use `verify(string, " ")==0`
```fortran
logical function isEmpty (string)
  character*(*)  :: string
```

### `isWord()`
Check if string is a word *i.e.* contains any ASCII letters.
```fortran
logical function isWord (string)
  character*(*)  :: string
```

### `replaceText()`
Replaces `text` with `rep` within `string`.  
*e.g. "This is bad." &rarr; "This is good."*
```fortran
function replaceText (string, text, rep) result (out)
  character(*)                :: string, text, rep
  character*(:), allocatable  :: out
```

### `tab2space()`
Transforms *tab* to four *space* characters.
```fortran
function tab2space (string) result (out)
  character(*)                :: string
  character*(:), allocatable  :: out
```

### `toLower()` and `toUpper()`
Transforms string to lower or all upper case, respectively.  
*e.g. "This IS foo BAR." &rarr; "this is foo bar." / "THIS IS FOO BAR."*
```fortran
function toLower (string) result (out)
  character*(*)           :: string
  character(len(string))  :: out

function toUpper (string) result (out)
  character*(*)           :: string
  character(len(string))  :: out
```

### `progressBar()`
Writes gradual progress bar to STDOUT (on the same output line). The size of the progress bar is determined by (optional) `size` variable. Write to STDOUT is "forbidden" until progress reaches 1.0 - use (optional) `message` instead.  
*e.g. 50.0% |############------------| [message]*
```fortran
subroutine progressBar (progress, size, message)
  real,                   :: progress
  integer, optional       :: size
  character(*), optional  :: message
```

<!-- ------------------------------------------------------------------------- -->

### `linest()`
Calculates [simple linear regression](https://en.wikipedia.org/wiki/Simple_linear_regression) (k&middot;y+n) for array of kind real - `c=[k,n]`. Variance (optional) is returned as `d=[dk,dn]`, and R<sup>2</sup> value (optional) as `R2`.
```fortran
function linest (x, y, d, R2) result(c)
  real,           :: x(:), y(:), c(2)
  real, optional  :: d(2), R2
```

<!-- ============================================== -->

## Notes
Most of the routines in xslib won't allow you to do stupid things and will either promptly stop you from hurting yourself by the terminating the program or will politely protest with a warning (and most likely crash afterwards). If don't like being told what you can and can't do simply delete everything in `error` and `warning` subroutines located in *src/common.f90*.
