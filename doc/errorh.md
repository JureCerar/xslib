# errorh
Module `xslib_errorh` contains functions of error and warning handling.

---------------------------------------------------------------------
## `error`
Write error message to STDERR and terminate the program.
```fortran
call error (message)
```
#### *Parameters*
`character(*), intent(IN) :: message`
> Error message to display.

#### *Example*
```Fortran
> call error ("Error message")
[Error]: Error message
```
#### *Note*
Use with `__FILE__` and `__LINE__` macros to get extended error message.
```Fortran
#define error(x) error_ (x, __FILE__, __LINE__)
```

---------------------------------------------------------------------
## `warning`
Write warning message to STDERR.
```fortran
call warning (message)
```
#### *Parameters*
`character(*), intent(IN) :: message`
> Warning message to display.

#### *Example*
```Fortran
> call warning ("Warning message")
[Warning]: Warning message
```
#### *Note*
Use with `__FILE__` and `__LINE__` macros to get extended warning message.
```Fortran
#define warning(x) warning_ (x, __FILE__, __LINE__)
```

---------------------------------------------------------------------
## `assert`
Assert logical expression. On fail write error message to STDERR and terminate the program.
```fortran
call assert (expression)
```
#### *Parameters*
`logical, dimension(..), intent(IN) :: expression`
> Logical expression to be evaluated.

#### *Example*
```Fortran
> call assert (array == 0)
```
#### *Note*
Use with `__FILE__` and `__LINE__` macros to get extended error message.
```Fortran
#define assert(x) assert_ (x, __FILE__, __LINE__)
```
