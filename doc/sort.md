# sort
Module `xslib_sort` contains function sorting arrays.

## `swap`
Swap values of `a` and `b`.
```fortran
call swap (a, b)
```
#### *Parameters*
`class(*), intent(INOUT) :: a, b`
> Values to be swapped. Must be same type.  

#### *Example*
```Fortran
> call swap (a, b)
```
---------------------------------------------------------------------

## `sort`
Sort input array. Sorting algorithm can be selected: `quicksort`, `mergesort`, or `heapsort`. Default is `quicksort`. Order contains argument sort order.
```fortran
call sort (array, kind, order)
```
#### *Parameters*
`class(*), dimmension(:), intent(INOUT) :: array`
> Input array to be sorted.

`character(*), intent(IN), optional :: kind`
> Sorting algorithm: `quicksort` (default), `mergesort`, or `heapsort`.

`integer, dimmension(:), intent(OUT), optional :: order`
> Value order in sorted array in respect to original. Same size as `array`.

#### *Example*
```Fortran
> call sort (array, KIND="quicksort", ORDER=order)
```
---------------------------------------------------------------------

## `qsort`
Sort input using [quicksort](https://en.wikipedia.org/wiki/Quicksort) algorithm. Order contains argument sort order.
```fortran
call qsort (array, order)
```
#### *Parameters*
`class(*), dimmension(:), intent(INOUT) :: array`
> Input array to be sorted.

`integer, dimmension(:), intent(OUT), optional :: order`
> Value order in sorted array in respect to original. Same size as `array`.

---------------------------------------------------------------------

## `msort`
Sort input using [merge sort](https://en.wikipedia.org/wiki/Merge_sort) algorithm. Order contains argument sort order.
```fortran
call msort (array, order)
```
#### *Parameters*
`class(*), dimmension(:), intent(INOUT) :: array`
> Input array to be sorted.

`integer, dimmension(:), intent(OUT), optional :: order`
> Value order in sorted array in respect to original. Same size as `array`.

---------------------------------------------------------------------

## `hsort`
Sort input using [heapsort](https://en.wikipedia.org/wiki/Heapsort) algorithm. Order contains argument sort order.
```fortran
call hsort (array, order)
```
#### *Parameters*
`class(*), dimmension(:), intent(INOUT) :: array`
> Input array to be sorted.

`integer, dimmension(:), intent(OUT), optional :: order`
> Value order in sorted array in respect to original. Same size as `array`.

---------------------------------------------------------------------
