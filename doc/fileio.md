### xslib I/O modules
<!-- - [Molecular file types: General](#molecular-file-types-general)
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
  - [`csv_t` type definition](#csv_t-type-definition) -->

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
All objects (with exception of CUBE cub_t, but we will return to that latter) are structured in same way *i.e.* each object is composed of multiple frames (or just one) where the all data is stored. Each frame consists of number of atoms, their coordinates, simulation box size, and additional data that is specific to each file type. Detailed structure of each type is defined below. So for example to access the coordinates of the 2nd atoms in 1st frame:
```Fortran
obj%frame(1)%coor(:,2)
```
**NOTE** the Fortran language uses **row-major** indexing *i.e.* `array(row,column)` and that convention is retained here.

All I/O procedures in xslib are defined as functions with integer return value that contains error code:  
```Fortran
type(obj_t) :: obj
integer     :: error

error = obj%procedure(arg)
```
All **non zero** return values indicate an error has occurred. Detailed error message can be obtained with [`xslibErrMsg()`](#xslibErrMsg).

-----------------------------------------------

## Molecular file types: Interface
The most simple way to read molecular file is using read method. This opens a file, reads all of the data, and then closes it.
```Fortran
error = obj%read( "path/to/file.ext" )
```
The more typical way of reading file is to open it, read it frame-by-frame and then close it when you are done. To open a file use:
```Fortran
error = obj%open( "path/to/file.ext" )
```
Opening a file additionally preforms file check (for formatting and corrupted/missing data) and obtains some additional information such as:
number of frames, (lagest) number of atoms and (lagest) simulation box side:
```Fortran
integer :: natoms, nframes
real    :: box(3,3)
nframes  = obj%getFrames()
natoms   = obj%getNatoms()
box(:,:) = obj%getBox()
```

To read one (or more) frames use:
```Fortran
error = obj%read_next()
! or
error = obj%read_next( nframes )
```
where `nframes` is optional parameter that defines how many frames to read (default is 1). Note that when you call `read_next()` all previous data is deallocated.  
Similarly, in order to skip one (or more) frames use:
```Fortran
error = obj%skip_next()
! or
error = obj%skip_next(nframes)
```
This does not affect currently allocated data. **NOTE:** If you read/skip more frames that are present/left in file, only the remaining
number of frames will be allocated and read. It is up to the user to check if how many frames were actually read:
```Fortran
error = obj%read_next(10)
if ( obj%nframes /= 10 ) then
  ! ...
```

Alternatively, you can read frames out-of-order (regardless where you are in file). For example to read 2 frame in file use:
```Fortran
error = obj%read_frame( 2 )
```

You can set the current position (frame) in file using:
```Fortran
error = obj%fseek( 1, whence )
```
where `whence `refers to position from where offset is added and is one of the following:  
`0` -- Beginning of file (SEEK_SET),  
`1` -- Current position of the file pointer (SEEK_CUR),  
`2` -- End of file (SEEK_END)  
To inquire current position in file use:
```Fortran
integer :: pos
pos = obj%fseek()
```
Finally, when you are done you should close file with:
```Fortran
error = obj%close()
```
Closing the file does not effect any stored data.

-----------------------------------------------

All derived types have defined assignment(=) function, which allows you to copy data, either the whole object or frame-by-frame:
```Fortran
type(obj_t) :: obj, cpy
cpy = obj
! or
cpy%frame(1) = obj%frame(1)
```
If you want to create "new" object you first need to allocate the memory. This too can be done either whole object or frame-by-frame:
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
**NOTE:** The STDOUT and file UNIT methods do not work for `xtc_t`, `trr_t`, and `dcd_t` objects, as they are not human readable formats. Alternatively, you can use dump method to obtain human readable format (useful for debugging):
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
