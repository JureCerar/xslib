# stats
Module `xslib_stats` contains basic statistics functions. Supports both single and double precision (`DP`).  

## `normal`
Return random samples from a normal (Gaussian) distribution. Random number sequence can be initialized with [`srand`](https://gcc.gnu.org/onlinedocs/gfortran/SRAND.html#SRAND) function.
```fortran
out = normal(mu, sigma)
```
#### *Parameters*
`real(DP), intent(IN) :: mu, sigma`
> The mean `mu` and variance `sigma` values of random distribution.  

`real(DP) :: out`
> Random value on a normal distribution.  

#### *Example*
```Fortran
> normal(0., 1.)
0.123456
```
---------------------------------------------------------------------

## `average`
Return average value of an array.
```fortran
out = average(array)
```
#### *Parameters*
`integer(DP) or real(DP), dimesion(:), intent(IN) :: array`
> Input array.

`real(DP) :: out`
> Average value of array. 

#### *Example*
```Fortran
> average([1,2,3,4,5])
3.00000
```
---------------------------------------------------------------------

## `stddev`
Return standard deviation of an array.
```fortran
out = stddev(array)
```
#### *Parameters*
`integer(DP) or real(DP), dimesion(:), intent(IN) :: array`
> Input array.

`real(DP) :: out`
> Standard deviation of array values. 

#### *Example*
```Fortran
> stddev([1,2,3,4,5])
1.58114
```
---------------------------------------------------------------------

## `variance`
Return variance of an array.
```fortran
out = variance(array)
```
#### *Parameters*
`integer(DP) or real(DP), dimesion(:), intent(IN) :: array`
> Input array.

`real(DP) :: out`
> Variance of array values. 

#### *Example*
```Fortran
> variance([1,2,3,4,5])
2.50000
```
---------------------------------------------------------------------

## `welford`
Welford's online algorithm for calculating variance. Final variance must be "corrected" with `welford_finalize` call.
```fortran
call welford (array, mean, variance, n)
```
```fortran
call welford_finalize (mean, variance, n)
```

#### *Parameters*
`integer(DP) or real(DP), dimesion(:), intent(IN) :: array`
> Input array.

`real(DP), dimesion(:), intent(INOUT) :: mean, variance`
> Mean value and variance of array values. Must be same size as `array`.

`integer, intent(IN) :: n`
> Sequential number of an array. Must be non-zero.

#### *Example*
```Fortran
> do i = 1, NP
>   call random_number (array)
>   call welford (array, mean, variance, i)
> end do
> call welford_finalize (mean, variance, NP)
```
---------------------------------------------------------------------

## `histogram`
Return histogram of an array.
```fortran
out = histogram(array, nbins, min, max)
```

#### *Parameters*
`integer(DP) or real(DP), dimesion(:), intent(IN) :: array`
> Input array.

`integer, intent(IN) :: nbins`
> Number of equal-width bins in the given range.

`real(DP), intent(IN), optional :: min, max`
> The lower and upper range of the bins. If not provided, range is simply `min(array)` and `max(array)`.

`integer, dimension(nbins) :: out`
> The values of the histogram.

#### *Example*
```Fortran
> call random_number (array)
> histogram(array, 5, MIN=0., MAX=1.)
[9,10,8,11,11]
```
---------------------------------------------------------------------
