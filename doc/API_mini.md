## Table of contents
- [Molecular file types](Molecular_file_types)
  - [`pdb_file`](#pdb_file)
  - [`gro_file`](#gro_file)
  - [`trj_file`](#trj_file)
  - [`xyz_file`](#xyz_file)
  - [`frame_file`](#frame_file-object)
- [Supporting file types](#supporting-file)
  - [`ndx_file`](#ndx_file)
  - [`tpl_file`](#tpl_file)
- [Data file types](#data-file-types)
  - [`pdh_file`](#pdh_file)
  - [`csv_file`](#csv_file)


## Molecular file types  

### `pdb_file`
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
  procedure :: read_next
  procedure :: read
  procedure :: box
  procedure :: natoms
  procedure :: write
end type pdb_file

! --------------------------

subroutine allocate (npoints, onlycoor, initialize)
  integer, intent(in) :: npoints
  class(*), optional  :: onlycoor, initialize
end subroutine allocate

subroutine deallocate ()
end subroutine deallocate

subroutine initialize ()
end subroutine initialize

! --------------------------  

subroutine open (file)
  character*(*) :: file
end subroutine open

subroutine close ()
end subroutine close

subroutine allocate (npoints)
  integer :: npoints
end subroutine allocate

function read_next (nframes, onlycoor) result (framesRead)
  integer, optional  :: nframes
  class(*), optional :: onlycoor
  integer            :: framesRead
end function read_next

subroutine read (file)
  character*(*) :: file
end subroutine read

function box () result (box)
	real :: box(3)
end function box

function natoms () result (natoms)
  integer :: natoms
end function natoms

subroutine write (unit, file)
  character*(*), optional :: file
  integer, optional       :: unit
end subroutine write
```

### `gro_file`
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
  procedure	:: allocate
  procedure	:: deallocate
  procedure	:: initialize

end type gro_frame

type gro_file
  integer                       :: nframes, framesLeft
  type(gro_frame), allocatable  :: frameArray(:)
contains
  procedure :: open
  procedure :: close
  procedure :: allocate
  procedure :: read_next
  procedure :: read
  procedure :: write
  procedure :: natoms
  procedure :: box
end type gro_file

! --------------------------

subroutine allocate (npoints, onlycoor, initialize)
  integer, intent(in)	:: npoints
  class(*), optional	:: onlycoor, initialize
end subroutine allocate

subroutine deallocate ()
end subroutine deallocate

subroutine initialize ()
end subroutine initialize

! --------------------------  

subroutine open (file)
  character*(*) :: file
end subroutine open

subroutine close ()
end subroutine close

subroutine allocate (npoints)
  integer :: npoints
end subroutine allocate

function read_next (nframes, onlycoor) result (framesRead)
  integer, optional  :: nframes
  class(*), optional :: onlycoor
end function read_next

subroutine read (file)
  character*(*) :: file
end subroutine read

function box () result (box)
	real :: box(3)
end function box

function natoms () result (natoms)
  integer :: natoms
end function natoms

subroutine write (unit, file)
  character*(*), optional :: file
  integer, optional       :: unit
end subroutine write

```

### `trj_file`
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
  procedure :: close
  procedure :: x
  procedure :: natoms
  procedure :: box
  procedure :: time
  procedure :: step
end type

! --------------------------  

subroutine open (file)
  character*(*) :: file
end subroutine open

subroutine read (file)
  character*(*) :: file
end subroutine read

function read_next (nframes) result (framesRead)
  integer, optional  :: nframes
  integer            :: framesRead
end function read_next

subroutine close ()
end subroutine close
```
**NOTE: Not completed.**

### `xyz_file`
```Fortran
type, provate :: xyz_frame
  integer 									:: natoms
  character*512							:: comment
  character*5, allocatable	:: name(:)
  real, allocatable					:: coor(:,:)
  real											:: box(3)
contains
  procedure	:: allocate
  procedure	:: deallocate
  procedure	:: initialize
end type

type xyz_file
  integer, private							:: unit
  integer 											:: nframes, framesLeft
  type(xyz_frame), allocatable	:: frameArray(:)
contains
  procedure	:: open
  procedure	:: close
  procedure	:: allocate
  procedure	:: read_next
  procedure	:: read
  procedure	:: box
  procedure	:: natoms
  procedure	:: write
end type

! --------------------------

subroutine allocate (npoints, onlycoor, initialize)
  integer, intent(in)	:: npoints
  class(*), optional	:: onlycoor, initialize
end subroutine allocate

subroutine deallocate ()
end subroutine deallocate

subroutine initialize ()
end subroutine initialize

! --------------------------  

subroutine open (file)
  character*(*) :: file
end subroutine open

subroutine close ()
end subroutine close

subroutine allocate (npoints)
  integer :: npoints
end subroutine allocate

function read_next (nframes, onlycoor) result (framesRead)
  integer, optional :: nframes
  integer           :: framesRead
end function read_next

subroutine read (file)
  character*(*) :: file
end subroutine read

function box () result (box)
	real :: box(3)
end function box

function natoms () result (natoms)
  integer :: natoms
end function natoms

subroutine write (unit, file)
  character*(*), optional :: file
  integer, optional       :: unit
end subroutine write
```

### `frame_file`  

```fortran
type frame_data
  integer            :: natoms
  real               :: box(3)
  real, allocatable  :: coor(:,:)
contains
  procedure :: open
  procedure :: read_next
  procedure :: close
  procedure :: nframes
  procedure :: get_natoms
  procedure :: get_box
end type frame_data

! --------------------------

subroutine open (file)
  character*(*) :: file
end subroutine open

function read_next () result (framesRead)
  integer :: framesRead
function read_next

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
## Supporting file types

### `ndx_file`

### `tpl_file`

## Data file types  

### `pdh_file`

### `csv_file`
