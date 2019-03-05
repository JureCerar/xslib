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

### `gro_file`

### `trj_file`

### `xyz_file`

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

subroutine open (name)
  character*(*) :: name

function read_next (n) result (nread)
  integer, optional :: n
  integer           :: nread

subroutine close ()

function nframes ()
  integer :: nframes

function get_natoms () result (natoms)
  integer :: natoms

function get_box () result (box)
  integer :: box(3)
```
## Supporting file types

### `ndx_file`

### `tpl_file`

## Data file types  

### `pdh_file`

### `csv_file`
