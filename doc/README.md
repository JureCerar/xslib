# xslib API
## Table of contents
### xslib I/O modules

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
- [xslib_xmalloc](#xslib_xmalloc)
  - [`xmalloc()`](#xmalloc)  
  - [`xcalloc()`](#xcalloc)  
  - [`xrealloc()`](#xrealloc)  
- [xslib_error](#xslib_error)
  - [`error()` and `error_()`](#error-and-error_)
  - [`warning()` and `warning_()`](#warning-and-warning_)
  - [`xslibErrMsg()`](#xsliberrmsg)
  - [`assert()` and `assert_()`](#assert-and-assert_)
- [Notes](#notes)


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
