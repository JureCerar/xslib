# xmalloc
Modules `xslib_xmalloc` contains functions for memory allocation.

---------------------------------------------------------------------
## `xmalloc`
Allocates a block of memory for an array of num. elements. It's just a nice wrapper for built-in allocation but with error handling.
```fortran
call xmalloc (object, spec, stat, errmsg)
```
#### *Parameters*
`class(*), allocatable, dimension(..), intent(INOUT) :: object`
> Object to be allocated.

`integer, dimension(:), intent(IN) :: spec`
>  Object shape specification. Must be same size as `rank(object)`.

`integer, intent(OUT), optional :: stat`
> Error status code. Returns zero if no error. 

`character(*), intent(OUT), optional :: errmsg`
> Error status message. Empty if no error. 

#### *Example*
```Fortran
> call xmalloc (array, [10, 10], STAT=status, ERRMSG=message)
```

---------------------------------------------------------------------
## `xcalloc`
Allocates and initializes a block of memory for an array of num. elements. It's just a nice wrapper for built-in allocation but with error handling.
```fortran
call xcalloc (object, spec, stat, errmsg)
```
#### *Parameters*
`class(*), allocatable, dimension(..), intent(INOUT) :: object`
> Object to be allocated.

`integer, dimension(:), intent(IN) :: spec`
>  Object shape specification. Must be same size as `rank(object)`.

`integer, intent(OUT), optional :: stat`
> Error status code. Returns zero if no error. 

`character(*), intent(OUT), optional :: errmsg`
> Error status message. Empty if no error. 

#### *Example*
```Fortran
> call xcalloc (array, [10, 10], STAT=status, ERRMSG=message)
```

---------------------------------------------------------------------
## `xrealloc`
Changes the size of memory block. Allocates a new memory block if not allocated. The content of the memory block is preserved up to the lesser of the new and old sizes, even if the block is moved to a new location. If the new size is larger, the value of the newly allocated portion is indeterminate.
```fortran
call xrealloc (object, spec, stat, errmsg)
```
#### *Parameters*
`class(*), allocatable, dimension(..), intent(INOUT) :: object`
> Object to be reallocated.

`integer, dimension(:), intent(IN) :: spec`
>  Object shape specification. Must be same size as `rank(object)`.

`integer, intent(OUT), optional :: stat`
> Error status code. Returns zero if no error. 

`character(*), intent(OUT), optional :: errmsg`
> Error status message. Empty if no error. 

#### *Example*
```Fortran
> call xrealloc (array, [10, 10], STAT=status, ERRMSG=message)
```
