# API

## Table of contents

- [Molecular file types - General](#molecular-file-types-general)
- [Molecular file types - File specific data](#molecular-file-types-file-specific-data)
	- [`pdb_file` format](#pdb_file-format)
      <!-- - [Example](#Example)
      - [Structure](#Structure)
      - [Interface](#Interface) -->
	- [`gro_file` format](#gro_file-format)
      <!-- - [Example](#Example)
      - [Structure](#Structure)
      - [Interface](#Interface) -->
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
	- [`xslibinfo()`](#xslibinfo)
	- [`str()`](#str)
	- [`error()` and `warning()`](#error-and-warning)
	- [`newUnit()`](#newunit)
	- [`cross()`](#cross)
	- [`minImg()`](#minimg)
	- [`getDistance()`](#getdistance)
	- [`getAngle()`](#getangle)
	- [`getDihedral()`](#getdihedral)
	- [`rotate()`](#rotate)
	- [`deg2rad()` and  `rad2deg()`](#deg2rad-and-rad2deg)
	- [`crt2sph()`, `sph2cart()`, `crt2cyl()`, and `cyl2crt()`](#crt2sph-sph2cart-crt2cyl-and-cyl2crt)
	- [`get_wtime()`](#get_wtime)
	- [`write_time()`](#write_time)
	- [`msleep()`](#msleep)
	- [`variance()`](#variance)
	- [`pathname()`](#pathname)
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
- [Notes](#notes)

-----------------------------------------------

## Molecular file types: General

XsLib supports the use of multiple molecular coordinate files:  
- [.gro](http://manual.gromacs.org/archive/5.0.3/online/gro.html) - GROMACS coordinate file.  
- [.pdb](https://www.rcsb.org/pdb/static.do?p=file_formats/pdb/index.html) - Protein Data Bank which is most commonly used and widely spread molecular coordinate file.
- [.xyz](https://en.wikipedia.org/wiki/XYZ_file_format) - Simple *"name,x,y,z"* file that gets the job done in most cases.
- [.trr/.xtc](https://en.wikipedia.org/wiki/XYZ_file_format) - GROMACS binary trajectory and compressed trajectory files, respectively.

They are implemented as derived type variables consisting of data and procedure bound to that type. They can be used with `type(obj_file)`:
```fortran
type(xyz_file) :: xyz
type(pdb_file) :: pdb
type(gro_file) :: gro
type(trj_file) :: trj !.xtc and .trr
```

All molecular coordinate files are generally structured in same manner *i.e.* each file is composed of multiple frames. These frames stored as "frame arrays" - `obj%frameArray(n)%...`. Each frame array is composed of simulation box size, number of atoms, and atom coordinates - `...%box(3)`, `...%natoms`, `...%coor(1:3,:)`, respectively, and additional data that is specific to each file type.

The general API is the same for all derived types. Each file is opened with:
```fortran
call obj%open("path/to/file.obj")
```
Each frame can be read individually or in multiples with:
```fortran
int = obj%read_next(n)
```
where `n` is optional parameter denoting the number of frames to read. Returning argument (in this case *int*) contains the actual number of frames that were read (0 if no frames could be read). Data in each frame can be accessed as `obj%frameArray(n)%...`. Alternatively, entire file can be read at once by using:
```fortran
call obj%read("path/to/file.obj")
```
Once you are done with reading the file it is closed with:
```fortran
call obj%close()
```
Closing the file does not affect the data stored in `obj` (it does not deallocate the data).

The starting/ending frame and stride between frames can be changed using `obj%set`:
```fortran
call obj%set(FIRST=first, LAST=last, STRIDE=stride)
```
**NOTE:** `obj%set` must be called only ONCE and BEFORE reading any data (`obj%read()` or `obj%read_next()`).

After calling `obj%read()` or `obj%read_next()` every atom's coordinates are accessible as `obj%frameArray(n)%coor(:,:)`. Note that the Fortran language uses **row-major indexing** *i.e.* `array(row, column)` and that convention is retained here.

To inquire the box size and number of atoms prior to actually reading the file it can be done with:
```fortran
integer :: natoms
real    :: box(3)

box = obj%box()
natoms = obj%natoms()
```
**NOTE:** Calling this function retains your current position in file.  
**NOTE:** This does not work for `trj_file`... dont ask why...

If you want to allocate new data, you have to first allocate number of frames and then each frame individually, as:

```fortran
! Create 10 frames consisting of 100 atoms
nframes = 10
natoms = 100
call obj%allocate(nframes)
do i = 1, nframes
  call obj%frameArray(i)%allocate(natoms, INITIALIZE=.true.)

end do
```
The optional `INITIALIZE` argument formats the newly allocated data to empty values or 0. Alternatively, it can be done with `...%initialize()` procedure call.
The stored data can be outputted to file UNIT, FILE or STDOUT:
```fortran
call obj%write(UNIT=unit)
! or
call obj%write(FILE="path/to/file.obj")
! or
call obj%write()
```
**NOTE:** This does not work for `trj_file`.  

--------------------------------------------------------------------------------

## Molecular file types: File specific data

Each file type contains additional information (unique to format) that is listed down bellow:

### `pdb_file` format

For more information on variables please read [PDB Manual](https://www.rcsb.org/pdb/static.do?p=file_formats/pdb/index.html).

#### Example
```
         1         2         3         4         5         6         7         8
12345678901234567890123456789012345678901234567890123456789012345678901234567890
CRYST1   20.000   20.000   20.000  90.00  90.00  90.00 P 1           1
...
ATOM     36  C  AARG A  -3      13.559  86.257  95.222  0.50 37.37           C
...
END
```

#### Structure
```fortran
type, private :: pdb_frame
  real                       :: box(3)
  integer                    :: natoms
  character*20, allocatable  :: record_type(:), atom_name(:), alt_loc_indicator(:), residue_name(:), chain_identifier(:)
  character*20, allocatable  :: res_insert_code(:), segment_identifier(:), element_symbol(:)
  integer, allocatable       :: atom_serial_number(:), residue_sequence_number(:)
  real, allocatable          :: coor(:,:)
  real, allocatable          :: occupancy(:), temp_factor(:), charge(:)
contains
  procedure :: allocate
  procedure :: deallocate
  procedure :: initialize
end type pdb_frame

type pdb_file
  integer                       :: nframes
  type(pdb_frame), allocatable  :: frameArray(:)
contains
  procedure :: open
  procedure :: close
  procedure :: allocate
  procedure :: set
  procedure :: next
  procedure :: read_next
  procedure :: read
  procedure :: box
  procedure :: natoms
  procedure :: write
end type pdb_file
```

#### Interface
```Fortran
! Allocate npoints data; Use ONLYCOOR option to allocate only coor. data;
! INITIALIZE options initializes all allocated values.
subroutine allocate (npoints, onlycoor, initialize)
  integer, intent(in) :: npoints
  logical, optional   :: onlycoor, initialize
end subroutine allocate

! Deallocate data.
subroutine deallocate ()
end subroutine deallocate

! Initialize all allocated variables.
subroutine initialize ()
end subroutine initialize
```

```Fortran
! Open a file.
subroutine open (file)
  character*(*) :: file
end subroutine open

! Close a file.
subroutine close ()
end subroutine close

! Allocate num. of frames.
subroutine allocate (nframes)
  integer :: nframes
end subroutine allocate

! Set first/last frame and frame stride
subroutine set (first, last, stride)
  integer, optional :: first, last, stride
end subroutine set

! Skip one or 'nframes' number of frames
integer function next (nframes)
  integer, optional :: nframes
end function next

! Read next frame, where returned integer is the actual number of frames that were read
! Use ONLYCOOR option to read only coordinate data (to speedup read process and save memory).
integer function read_next (nframes, onlycoor)
  integer, optional :: nframes
  logical, optional :: onlycoor
end function read_next

! Read entire file.
subroutine read (file)
  character*(*) :: file
end subroutine read

! Get simulation box size of current frame (without changing the position in file).
function box () result (box)
  real :: box(3)
end function box

! Get number of atoms in current frame (without changing the position in file).
function natoms () result (natoms)
  integer :: natoms
end function natoms

! Write all data to stdout, unit or file.
subroutine write (unit, file)
  character*(*), optional :: file
  integer, optional       :: unit
end subroutine write
```

----------------------------------------

### `gro_file` format

For more information please read [GRO manual](link).

#### Example
```
         1         2         3         4         5         6         7         8
12345678901234567890123456789012345678901234567890123456789012345678901234567890
COMMENT
 12889
    1LIG      C    1   1.484   1.124   1.000
    2LIG      O    2   0.516   0.876   1.000
...
   2.00000   2.00000   2.00000
```

#### Structure
```fortran
type, private :: gro_frame
  character*512             :: title
  real                      :: time
  integer                   :: natoms
  real                      :: box(3)
  integer, allocatable      :: res_num(:), atom_num(:)
  character*5, allocatable  :: res_name(:), atom_name(:)
  real, allocatable         :: coor(:,:), vel(:,:)
contains
  procedure :: allocate
  procedure :: deallocate
  procedure :: initialize

end type gro_frame

type gro_file
  integer                       :: nframes, framesLeft
  type(gro_frame), allocatable  :: frameArray(:)
contains
  procedure :: open
  procedure :: close
  procedure :: allocate
  procedure :: set
  procedure :: next
  procedure :: read_next
  procedure :: read
  procedure :: write
  procedure :: natoms
  procedure :: box
end type gro_file
```

#### Interface
```Fortran
! Allocate npoints data; Use ONLYCOOR option to allocate only coor. data;
! INITIALIZE options initializes all allocated values.
subroutine allocate (npoints, onlycoor, initialize)
  integer, intent(in) :: npoints
  logical, optional   :: onlycoor, initialize
end subroutine allocate

! Deallocate data.
subroutine deallocate ()
end subroutine deallocate

! Initialize all allocated variables.
subroutine initialize ()
end subroutine initialize
```

```Fortran
! Open a file.
subroutine open (file)
  character*(*) :: file
end subroutine open

! Close a file.
subroutine close ()
end subroutine close

! Allocate num. of frames.
subroutine allocate (nframes)
  integer :: nframes
end subroutine allocate

! Set first/last frame and frame stride
subroutine set (first, last, stride)
  integer, optional :: first, last, stride
end subroutine set

! Skip one or 'nframes' number of frames
integer function next (nframes)
  integer, optional :: nframes
end function next

! Read next frame, where returned integer contains the actual number of frames read;
! Use ONLYCOOR option to read only coor. data (to speedup read process and save memory)
integer function read_next (nframes, onlycoor)
  integer, optional  :: nframes
  logical, optional  :: onlycoor
end function read_next

! Read entire file.
subroutine read (file)
  character*(*) :: file
end subroutine read

! Get simulation box size of current frame (without changing the position in file).
function box () result (box)
  real :: box(3)
end function box

! Get number of atoms in current frame (without changing the position in file).
function natoms () result (natoms)
  integer :: natoms
end function natoms

! Write all data to stdout, unit or file.
subroutine write (unit, file)
  character*(*), optional :: file
  integer, optional       :: unit
end subroutine write
```

----------------------------------------

### `trj_file` format

This is a direct port of [gmxfort](https://github.com/wesbarnett/libgmxfort) library (for more details please read the provided API).

#### Structure
```fortran
type, private :: frame_trj
  real(C_FLOAT), allocatable :: coor(:,:)
  integer(C_INT)             :: STEP
  real(C_FLOAT)              :: box(3,3), prec, time
end type

type trj_file
  type(xdrfile), pointer        :: xd
  type(frame_trj), allocatable  :: frameArray(:)
  type(ndx_file)                :: ndx
  integer                       :: NFRAMES
  integer                       :: NUMATOMS, N
  integer                       :: FRAMES_REMAINING
  logical                       :: read_only_index_group
contains
  procedure :: open
  procedure :: read
  procedure :: read_next
  procedure :: set
  procedure :: next
  procedure :: close
  procedure :: x
  procedure :: natoms
  procedure :: box
  procedure :: time
  procedure :: step
end type
```

#### Interface

```Fortran
! Open file.
subroutine open (file, ndxfile)
  character*(*)            :: file
  character*(*), optional  :: ndxfile
end subroutine open

! Read entire trajectory.
subroutine read (file, ndxfile, ndxgroup)
  character*(*)            :: file
  character*(*), optional  :: ndxfile, ndxgroup
end subroutine read

! Set first/last frame and frame stride
subroutine set (first, last, stride)
  integer, optional :: first, last, stride
end subroutine set

! Skip one or 'nframes' number of frames
integer function next (nframes)
  integer, optional :: nframes
end function next

! Read next frame, where returned integer contains the actual number of frames read;
! Read more than one frame by passing nframes.
integer function read_next (nframes)
  integer, optional  :: nframes
end function read_next

! Close the file.
subroutine close ()
end subroutine close

! Get position of any atom in any frame (provided frames were already read).
function x (frame, atom, group)
  real                    :: x(3)
  integer                 :: frame, atom
  character*(*), optional :: group
end function x

! Return num. of atoms in current frame.
integer function natoms (group)
  character*(*), optional :: group
end function natoms

! Return box size of current frame.
! NOTE: The result is a diagonal 3x3 matrix.
function box (frame)
  real    :: box(3,3)
  integer :: frame
end function box

! Get the simulation time of any frame.
real function time (frame)
  integer :: frame
end function time

! Get the time step size of any frame.
integer function step (frame)
  integer :: frame
end function step
```

----------------------------------------------------

### `xyz_file` format

For more informations please read [XYZ manual](https://en.wikipedia.org/wiki/XYZ_file_format).

#### Example

```
         1         2         3         4         5         6         7         8
12345678901234567890123456789012345678901234567890123456789012345678901234567890
128
Comment line
...
C      14.84292  11.24345  10.00000
O       5.15708   8.75655  10.00000
...
```

#### Structure

```Fortran
type, provate :: xyz_frame
  integer                   :: natoms
  character*512             :: comment
  character*5, allocatable  :: name(:)
  real, allocatable         :: coor(:,:)
  real                      :: box(3)
contains
  procedure :: allocate
  procedure :: deallocate
  procedure :: initialize
end type

type xyz_file
  integer, private              :: unit
  integer                       :: nframes, framesLeft
  type(xyz_frame), allocatable  :: frameArray(:)
contains
  procedure :: open
  procedure :: close
  procedure :: allocate
  procedure :: set
  procedure :: next
  procedure :: read_next
  procedure :: read
  procedure :: box
  procedure :: natoms
  procedure :: write
end type
```

#### Interface

```Fortran
! Allocate npoints data; Use INITIALIZE option to  initializes all allocated values.
subroutine allocate (npoints, initialize)
  integer, intent(in) :: npoints
  logical, optional   :: initialize
end subroutine allocate

! Deallocate data.
subroutine deallocate ()
end subroutine deallocate

! Initialize all allocated variables.
subroutine initialize ()
end subroutine initialize
```

```Fortran
! Open a file.
subroutine open (file)
  character*(*) :: file
end subroutine open

! Close a file.
subroutine close ()
end subroutine close

! Allocate num. of frames.
subroutine allocate (nframes)
  integer :: nframes
end subroutine allocate

! Set first/last frame and frame stride
subroutine set (first, last, stride)
  integer, optional :: first, last, stride
end subroutine set

! Skip one or 'nframes' number of frames
integer function next (nframes)
  integer, optional :: nframes
end function next

! Read next frame, where returned integer contains the actual number of frames read.
integer function read_next (nframes)
  integer, optional  :: nframes
  class(*), optional :: onlycoor
end function read_next

! Read entire file.
subroutine read (file)
  character*(*) :: file
end subroutine read

! Get simulation box size of current frame (without changing the position in file).
! NOTE: .xyz format does note necessarily contain simulation box data.
function box () result (box)
  real :: box(3)
end function box

! Get number of atoms in current frame (without changing the position in file).
function natoms () result (natoms)
  integer :: natoms
end function natoms

! Write all data to stdout, unit or file.
subroutine write (unit, file)
  character*(*), optional :: file
  integer, optional       :: unit
end subroutine write
```

----------------------------------------------------------------

### `frame_file` object format

> "One object to rule them all, one object to find them,  
> one object to bring them all, and in the darkness bind them."

The `frame_file` object (not to be confused with other frame objects) combines all other molecular coordinate files (.gro, .pdb, .xyz, .trr and .xtc) into one simple to use API. It is a bit different, as contains only one frame at the time, in order to be more compatible with OpenMP programming (one thread = one frame).

#### Structure

```Fortran
type frame_data
  integer            :: natoms
  real               :: box(3)
  real, allocatable  :: coor(:,:)
contains
  procedure :: open
  procedure :: set
  procedure :: read_next
  procedure :: close
  procedure :: nframes
  procedure :: get_natoms
  procedure :: get_box
end type frame_data
```

#### Interface

```fortran
! Open file. Supported types: .gro, .pdb, .xyz, .trr or .xtc
subroutine open (file)
  character*(*) :: file
end subroutine open

! Set first/last frame and frame stride
subroutine set (first, last, stride)
  integer, optional :: first, last, stride
end subroutine set

! Read next frame, where returned integer contains the actual number of frames read.
integer function read_next ()
function read_next

! Close file.
subroutine close ()
end subroutine close

function nframes ()
  integer :: nframes
end function nframes

function get_natoms () result (natoms)
  integer :: natoms
end function get_natoms

function get_box () result (box)
  integer :: box(3)
end function get_box
```

--------------------------------------------------

## Supporting file types

XsLib contains two "supporting file" types:
- [.ndx](http://manual.gromacs.org/archive/5.0.3/online/ndx.html) - GROMACS index file containing user defined sets of atoms.
- .tpl - Our own proprietary file type, containing condensed configuration information (created by [A. Lajovic](https://github.com/alajovic)).

### `ndx_file` format

[GROMACS index](http://manual.gromacs.org/archive/5.0.3/online/ndx.html) (.ndx) file containing user defined sets of atoms.

#### Structure

```fortran
type, private :: ndxgroups
  integer, allocatable          :: loc(:)
  integer                       :: natoms
  character*(:), allocatable    :: title
end type ndxgroups

type ndx_file
  type (ndxgroups), allocatable :: group(:)
  integer                       :: ngroups=0
  logical                       :: group_warning=.true.
contains
  procedure :: read
  procedure :: write
  procedure :: tpl2ndx
  procedure :: display
  procedure :: get
end type ndx_file
```

#### Interface

```fortran
! Read file.
subroutine read (file)
  character*(*) :: file
end subroutine read

! Write file to stdout, file or unit.
subroutine write (file, unit)
  character*(*), optional :: file
  integer, optional       :: unit
end subroutine read

! Transform .tpl file format to .ndx file format for all groups that have non-zero index.
! Use SYSTEM option to add "System group" containing indices of all particles.  
subroutine tpl2ndx_ndx (tpl, system)
  implicit none
  type(tpl_file)     :: tpl
  class(*), optional :: system
end subroutine tpl2ndx

! Displays present static index groups.
! >Present static index groups:
! > Group  X "System" (xxxxx atoms)
! > ...
subroutine display ()
end subroutine display

! Gets the number of atoms in a group. If an atom is specified, integer returns the overall index for that atom.
integer function get (group, i)
  character*(*)     :: group_name
  integer, optional :: i
end function get
```

-----------------------------------------------------------

### `tpl_file` format

Template file (.tpl) contains condensed information about all particles in system.

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

molecule 4 100  # TIP4P water
O  −1.1128  2
H   0.5564  2
H   0.5564  2
```

#### Structure

```fortran
type, private :: tpl_frame
  integer              :: natoms, nmol
  integer, pointer     :: id(:)
  character*3, pointer :: name(:)
  real, pointer        :: pcharge(:)
end type tpl_frame

type tpl_file
  real                         :: box(3)
  integer                      :: ntypes=0
  type(tpl_frame), allocatable :: type(:)
  integer                      :: natoms
  integer, pointer             :: id(:)
  character*3, pointer         :: name(:)
  real, pointer                :: pcharge(:)
contains
  procedure :: read
  procedure :: write
end type tpl_file
```

#### Interface

```Fortran
subroutine read (file)
  character*(*) :: file
end subroutine read

subroutine write (file, unit)
  character*(*), optional :: file
  integer, optional       :: unit
end subroutine write
```

#### More INFO

The .tpl file consists of three directives:  
- `side` (optional) directive contains information about the simulation box size. If cubic box is used only one box-side needs to be present. Units of box side must be the same as in accompanying molecular file. **NOTE:** Setting side any value (other than 0.) usually overrides the configuration box side (depends on implementation).  
- `moltype` (optional, legacy) specifies how many *molecule* directives are present in the file.  
- `molecule` directive must be specified along with **number of atoms** (in molecule) and **number of molecules**. This directive is followed by description of each particle: *name*, *pcharge*, and *id*.
  - `Name` (3-characters) parameter is required and provides general description of the particle,
  - `pcharge` (optional, float) descriptor contains partial-charge of particle, and
  - `id` (optional, integer) descriptor denotes particle ID.  

<!-- Internally supported particle types are: *H, O, C, N, S, CH, CH2, and CH3* -->

Everything after `#` character is threated as comment. Empty lines and excess spaces are ignored.  

Once file is read you can access the data in two ways:  
- individually by each molecule type  
- or sequentially all at the same time as "list".  

Example (Both cases yield same result):
```Fortran
! Access all particles sequentially as list
do i = 1, tpl%natoms
  write (*,*)  tpl%name(i), tpl%pcharge(i), tpl%id(i)

end do

! or access particle of each molecule type
do n = 1, tpl%ntypes
  do i = 1, tpl%type(n)%natoms
    write (*,*) tpl%type(n)%name(i), tpl%type(n)%pcharge(i), tpl%type(n)%id(i)

  end do
end do
! Both cases yield same result  
```
**NOTE:** Data is stored using pointers; This means by changing the data for "each type" also changes the data in "list".

---------------------------------------------------------

## Data file types

### `pdh_file` format

[PDH file](http://goldenberg.biology.utah.edu/downloads/usTooDocs_Sept2012.pdf) format was created by O. Glatter *et al.* for storing Small-Angle X-Ray Scattering (SAXS) data. It is generally accepted as standard format by the scattering community.

#### Example

```
1         2         3         4         5         6         7         8
12345678901234567890123456789012345678901234567890123456789012345678901234567890
COMMENT LINE                                                 
KEY1 KEY2 ....                                                                               
      100         0         0         0         0         0         0         0
  0.000000E+00   0.000000E+00   0.000000E+00   0.000000E+00   0.000000E+00
  0.000000E+00   0.000000E+00   0.000000E+00   0.000000E+00   0.000000E+00
...
  1.000000E+00   1.000000E+00  -1.000000E+00
...
```

#### Structure

```fortran
type pdh_file
  character*80      :: text
  character*4       :: key_words(16)
  integer           :: int_const(8)
  integer           :: num_points
  real              :: real_const(10)
  real, allocatable :: x(:), y(:), y_error(:)
contains
  procedure :: allocate
  procedure :: deallocate
  procedure :: initialize
  procedure :: read
  procedure :: write
  procedure :: smearing
  procedure :: bining
  procedure :: normalize
end type pdh_file
```

#### Interface

```fortran
! Allocate data; Use INITIALIZE option to initializes all allocated values.
subroutine allocate (npoints, initialize)
  integer            :: npoints
  class(*), optional :: initialize
end subroutine allocate

! Deallocate data.
subroutine deallocate ()
end subroutine deallocate

! Initialize all allocated variables.
subroutine initialize ()
end subroutine initialize
```

```fortran
! Read file.
subroutine read (file)
  character*(*) :: file
end subroutine read

! write file to stdout, unit, or file.
subroutine write (file, unit)
  character*(*), optional :: file
  integer, optional       :: unit
end subroutine write

! Numerical width and length smearing operations.
subroutine smearing (width, length)
  type(pdh_file), optional :: width, length
end subroutine smearing

! Interpolate 'npoints' equidistant points between fist and last point.
subroutine bining (npoints)
  integer :: npoints
end subroutine bining

! Normalizes area under the curve to 1.0. Data must be binned.
subroutine normalize ()
end subroutine normalize
```

----------------------------------------------------------------

### `csv_file` format

XsLib also includes *crude* implementation of [Comma Separated Value](https://en.wikipedia.org/wiki/Comma-separated_values) (.csv) files. Unlike in the rest of the library the `csv_file` objects does NOT contain any data; only procedures. The data is obtained as returning argument to function call.

#### Example

```
x,y,err
...
0.000,-1.000,0.000
1.000,0.000,0.000
...
```

#### Structure

```fortran
type csv_file
contains
  procedure :: read
  procedure :: write
end type csv_file
```

#### Interface

```fortran
! Read file. Header contains .csv header if present
subroutine read (file, data, header)
  character*(*)                         :: file
  real, allocatable                     :: data(:,:)
  character*(*), allocatable, optional  :: header(:)
end subroutine read

! Write data to stdout, file or unit. Custom data delimiter can be specified (default is ',').
subroutine write (data, header, unit, file, delimiter)
  real                    :: data(:,:)
  character*(*), optional :: file, delimiter, header(:)
  integer, optional       :: unit
end subroutine write

```

-----------------------------------------------------------------------------

## Functions and Subroutines

<!-- xslib.F90 -->

### `xslibinfo()`
Returns basic information about xslib library: version, compile date and time. Same info is available via variables `xslib_version` and `xslib_date`, respectivly.  
```fortran
character*16, parameter :: xslib_version
character*32, parameter :: xslib_date

function xslibinfo (version, date)
	logical, optional 				 :: version ! default=.true.
	logical, optional 				 :: date ! default=.false.
	character*(:), allocatable :: xslibinfo
end function xslibinfo
```

<!-- common.f90 -->

### `str()`
Transform SCALAR or ARRAY of any kind to character of shortest possible length. Optionally output format can be defined with `FMT` argument. In case of ARRAY a custom `DELIMITER` can be defined.
```fortran
function str (value, fmt) result (string)
  integer, real, complex, logical  :: value
  character*(*), optional          :: fmt
  character*(:), allocatable       :: string
end function str

function str (array, fmt, delimiter) result (string)
  integer, real, complex, logical  :: array(:)
  character*(*), optional          :: fmt, delimiter
  character*(:), allocatable       :: string
end function str
```

### `error()` and `warning()`
Writes error/warning message to STDERR. Calling `error()` also terminates the program and returns exit status 1 to shell.  
*i.e. <name\> ERROR/WARNING - <message\>*  
```fortran
subroutine error (message, name)
  character*(*)            :: message
  character*(*), optional  :: name
end subroutine error

subroutine warning (message, name)
  character*(*)            :: message
  character*(*), optional  :: name
end subroutine error
```

### `newUnit()`
Finds next free file unit and returns it as `newUnit`. Optionally the output can be stored as `unit`. Use with `open (UNIT=...)`   
**DEPRECATED:** Use `open (NEWUNIT=unit, ...)`
```fortran
integer function newUnit (unit)
  integer, optional, intent(out)  :: unit
end function newUnit
```

<!-- ### utilities.f90 ### -->

### `cross()`
Returns vector product: v &times; u.
```fortran
function cross (v, u)
  real, dimmension(3)  :: v, u, cross
end function cross
```

### `minImg()`
Return reduced coordinates according to [minimal image convention](https://en.wikipedia.org/wiki/Periodic_boundary_conditions).  
_I.e. a=a-box*floor(a/box+0.5) <.OR.> a=a-sign(box/2,r-box/2)-sign(box/2,r+box/2)_  
```fortran
function minImg (r, box)
  real, dimension(3)  :: r, box, minImg
end function minImg
```

### `getDistance()`
Returns distance between two points.  
**NOTE:** This is (awkward) alternative to `norm2(a-b)`; use the latter if possible.
```fortran
real function getDistance (a, b)
  real, dimension(3) :: a, b
end function getDistance
```

### `getAngle()`
Returns angle between three points (A-B-C).
```fortran
real function getAngle (a, b, c)
  real, dimension(3) :: a, b, c
end function getAngle

real function getAngle2 (a, b, c)
  real, dimension(3) :: a, b, c
end function getAngle2
```

### `getDihedral()`
Return [dihedral angle (theta)](https://en.wikipedia.org/wiki/Dihedral_angle) between four points (A-B-|-C-D).
```fortran
real function getDihedral (a, b, c, d)
  real, dimension(3) :: a, b, c, d
end function getDihedral
```

### `rotate()`
[Rotates vector](https://en.wikipedia.org/wiki/Rotation_matrix) around AXIS or specified VECTOR.   
```fortran
function rotate (in, axis, angle) result (out)
  real, dimension(3) :: in, out
  character*1        :: axis ! "X", "Y", or "Z"
  real               :: angle
end function rotate

function rotate (in, vector, angle) result (out)
  real, dimension(3) :: in, out, vector
  real               :: angle
end function rotate
```

### `deg2rad()` and  `rad2deg()`
Translates angle from degrees to radians and back.
```fortran
real function deg2rad (deg)
  real :: deg
end function deg2rad

real function rad2deg (rad)
  real :: rad
end function rad2deg
```

### `crt2sph()`, `sph2cart()`, `crt2cyl()`, and `cyl2crt()`
Coordinate system transformations, where:  
*crt* = cartesian (x,y,z),  
*sph* = spherical (r,theta,phi)   
*cyl* = cylindrical (r,theta,z)  
```fortran
function crt2sph (crt) result (sph)
  real, dimension(3) :: crt, sph
end function crt2sph

function sph2crt (sph) result (crt)
  real, dimension(3) :: sph, crt
end function sph2crt

function crt2cyl (crt) result (cyl)
  real, dimension(3) :: crt, cyl
end function crt2cyl

function cyl2crt (cyl) result (crt)
  real, dimension(3) :: cyl, crt
end function cyl2crt
```
<!-- ------------------------------------------------------------------------------ -->

### `get_wtime()`
Returns arbitrary high precision time in seconds.  
```fortran
real*8 function get_wtime ()
end function get_wtime
```

### `write_time()`
Transfors time in seconds to string in format: *"ddd:hh:mm:ss.sss"*. Use with `OMP_get_wtime()` or `get_wtime()`.  
```fortran
function write_time (time) result (string)
  real*8       :: time ! seconds
  character*16 :: string
end function write_time
```

### `msleep()`
Suspend execution for millisecond intervals.
```fortran
subroutine msleep (time)
  integer :: time ! millisecond
end subroutine msleep
```

<!-- ------------------------------------------------------------------------- -->

### `variance()`
Calculates [on-line variance](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance) for any real scalar or array, where `n` is data counter for variance aggregate.  
**NOTE:** True variance must be "corrected" after calculation as `var = merge( var/(n-1), -1.0, n>1 )`
```fortran
subroutine variance (value, mean, var, n)
  real or real*8 :: value(:), mean(:), var(:)
  integer        :: n
end subroutine variance
```
<!-- ------------------------------------------------------------------------- -->

### `pathName()`
Returns path name of file.  
*E.g. "./path/to/file.txt" &rarr; "./path/to/"*
```fortran
function pathName (name)
  character*(*)               :: name
  character*(:), allocatable  :: pathName
end function pathName
```

### `baseName()`
Returns base name of file.  
*E.g. "./path/to/file.txt" &rarr; "file"*
```fortran
function baseName (name)
  character*(*)               :: name
  character*(:), allocatable  :: baseName
end function baseName
```

### `extension()`
Returns file extension.    
*E.g. "./path/to/file.txt" &rarr; "txt"*
```fortran
function extension (name)
  character*(*)               :: name
  character*(:), allocatable  :: extension
end function extension
```

### `stripComment()`
Removes all characters trailing comment sign `cmt`.  
*E.g. "text #comment" &rarr; "text"*
```fortran
function stripComment (string, cmt)
  character*(*)               :: string, cmt
  character*(:), allocatable  :: stripComment
end function stripComment
```

### `backup()`
Renames file if it already exits.    
*E.g. "./path/to/file.txt" &rarr; "./path/to/#file.txt.n#" (n=1,2,..)*
```fortran
subroutine backup (file)
  character*(*) :: file
end subroutine backup
```

### `nextFreeName()`
Returns next free available variation of the file name.  
*E.g. "out.txt" &rarr; "out.n.txt" (n=1,2,..)*
```fortran
function nextFreeName (name)
  character*(*)              :: name
  character(:), allocatable  :: nextFreeName
function nextFreeName
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
end function isWord
```

### `replaceText()`
Replaces `text` with `rep` within `string`.  
*E.g. "This is bad." &rarr; "This is good."*
```fortran
function replaceText (string, text, rep) result (out)
  character(*)                :: string, text, rep
  character*(:), allocatable  :: out
end function replaceText
```

### `tab2space()`
Transforms *tab* to four *space* characters.
```fortran
function tab2space (string) result (out)
  character(*)                :: string
  character*(:), allocatable  :: out
end function tab2space
```

### `toLower()` and `toUpper()`
Transforms string to lower or all upper case, respectively.  
*E.g. "This IS foo BAR." &rarr; "this is foo bar." / "THIS IS FOO BAR."*
```fortran
function toLower (string)
  character*(*)           :: string
  character(len(string))  :: toLower
end function toLower

function toUpper (string)
  character*(*)           :: string
  character(len(string))  :: toUpper
end function toUpper
```

### `progressBar()`
Writes gradual progress bar to STDOUT (on the same output line). The size of the progress bar is determined by (optional) `size` variable. Write to STDOUT is "forbidden" until progress reaches 1.0 - use (optional) `message` instead.  
*E.g. 50.0% |############------------| [message]*
```fortran
subroutine progressBar (progress, size, message)
  real                   :: progress
  integer, optional      :: size
  character(*), optional :: message
end subroutine progressBar
```

----------------------------------------------

## Notes
Most of the routines in XsLib won't allow you to do stupid things and will either promptly stop you from hurting yourself by terminating the program or will politely protest with a warning (and most likely crash afterwards). If don't like being told what you can and can't do simply delete everything in `error` and `warning` subroutines located in *src/xslib_common.f90*.
