## time
Module `xslib_time` contains functions for timing and displaying time.

## `wtime`
Returns precise wall time in seconds since an unspecified time. The absolute value of `wtime` is meaningless, only differences between subsequent calls to this function should be used.  
```fortran
out = wtime()
```
#### *Parameters*
`real(REAL64) :: out`
> Time in seconds. 

#### *Example*
```Fortran
> wtime()
1234.567890000
```
---------------------------------------------------------------------

## `writeTime`
Transforms time from `wtime` or `OMP_get_wtime` to string in format `ddd-hh:mm:ss.sss`. Format changes depending on the lenght of time. 
```fortran
out = writeTime(time)
```
#### *Parameters*
`real(REAL64), intent(IN) :: time`
> Time in seconds. 

`character(64) :: out`
> Time string in format `ddd-hh:mm:ss.sss`. 

#### *Example*
```Fortran
> writeTime(126671.1111d0)
1-11:11:11.111
```
---------------------------------------------------------------------

## `msleep`
Susspends execution for specified millisecond interval.
```fortran
call msleep(time)
```
#### *Parameters*
`integer, intent(IN) :: time`
> Time in milliseconds. 

#### *Example*
```Fortran
> call msleep(1000)
```
---------------------------------------------------------------------
