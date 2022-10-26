# xslib - Extra-Small Library

## Table of content
- [cstring](cstring.md) 
  - [str](cstring.md#str)
  - [smerge](cstring.md#smerge)
  - [toUpper](cstring.md#toupper)
  - [toLower](cstring.md#tolower)
  - [toTitle](cstring.md#totitle)
  - [swapCase](cstring.md#swapcase)
  - [strip](cstring.md#strip)
  - [replace](cstring.md#replace)
  - [isAlpha](cstring.md#isalpha)
  - [isNumber](cstring.md#isnumber)
  - [isSpace](cstring.md#isspace)
  - [getColor](cstring.md#getcolor)
  - [setColor](cstring.md#setcolor)
  - [strtok](cstring.md#strtok)
  - [cnttok](cstring.md#cnttok)
  - [basename](cstring.md#basename)
  - [pathname](cstring.md#pathname)
  - [extension](cstring.md#extension)
- [xmalloc](xmalloc.md)
  - [xmalloc](xmalloc.md#xmalloc)
  - [xcalloc](xmalloc.md#xcalloc)
  - [xrealloc](xmalloc.md#xrealloc)
- [errorh](errorh.md)
  - [error](errorh.md#error)
  - [warning](errorh.md#warning)
  - [assert](errorh.md#assert)
- [time](time.md)  
  - [wtime](time.md#wtime)  
  - [writeTime](time.md#writetime)  
  - [msleep](time.md#msleep)  
- [vector](vector.md)  
  - [cross](vector.md#cross)  
  - [rotate](vector.md#rotate)  
  - [deg2rad and rad2deg](vector.md#deg2rad-and-rad2deg)  
  - [crt2sph and sph2crt](vector.md#crt2sph-and-sph2crt)  
  - [crt2cyl and cyl2crt](vector.md#crt2cyl-and-cyl2crt)  
  - [distance](vector.md#distance)  
  - [angle](vector.md#angle)  
  - [dihedral](vector.md#dihedral)  
- [math](math.md)  
  - [linspace](math.md#linspace)  
  - [logspace](math.md#logspace)  
  - [arange](math.md#arange)  
  - [mix](math.md#mix)  
  - [clip](math.md#clip)  
  - [isclose](math.md#isclose)  
  - [interp](math.md#interp)  
  - [trapz](math.md#trapz)  
  - [gradient](math.md#gradient)  
  - [gcd and lcm](math.md#gcd-and-lcm)  
- [sort](sort.md)  
  - [swap](sort.md#swap)  
  - [sort](sort.md#sort)  
  - [qsort](sort.md#qsort)  
  - [msort](sort.md#msort)  
  - [hsort](sort.md#hsort)  
- [stats](stats.md)  
  - [normal](stats.md#normal)  
  - [average](stats.md#average)  
  - [stddev](stats.md#stddev)  
  - [variance](stats.md#variance)  
  - [welford](stats.md#welford)  
  - [histogram](stats.md#histogram)  
- [fitting](fitting.md)  
  - [linfit](fitting.md#linfit)
  - [polyfit](fitting.md#polyfit)
  - [polyval](fitting.md#polyval)
- [example](#example)  
- [notes](#notes)  

## `example`
Short discription followed by function template.
```fortran
call example(arg)
```
#### *Parameters*
`character(*), intent(IN) :: arg`
> Parameter type and attributes folowed by short description.

#### *Example*
```fortran
> call example("This is example")
```
Example of function use and expected output. 

## Notes
- (bug) `xmalloc` has known issues with gcc-11 (older versions).
- (bug) `sort.test` has known issues with gcc-12. Function works correctly but test fails.