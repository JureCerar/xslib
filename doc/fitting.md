## fitting
Module `xslib_fitting` contains basic regression functions. Supports both single and double precision (`DP`). 

---------------------------------------------------------------------
## `linfit`
Least squares linear fit.
```fortran
out = linfit(x, y)
```
#### *Parameters*
`real(DP), dimension(:), intent(IN) :: x, y`
> x- and y-coordinates of the data points to be fitted. Must be same size. 

`real(DP), dimension(2) :: out`
> Linear coefficients, highest power first.

#### *Example*
```Fortran
> linfit([1.,2.,3.], [1.,2.,3.])
[1.00000,0.00000]
```

---------------------------------------------------------------------
## `polyfit`
Least squares polynomial fit. **NOTE:** For large arrays use [LAPACK](https://rosettacode.org/wiki/Polynomial_regression#Fortran).
```fortran
out = polyfit(x, y, deg)
```
#### *Parameters*
`real(DP), dimension(:), intent(IN) :: x, y`
> x- and y-coordinates of the data points to be fitted. Must be same size. 

`integer, intent(IN) :: deg`
> Degree of the fitting polynomial. 

`real(DP), dimension(deg+1) :: out`
> Polynomial coefficients, highest power first.

#### *Example*
```Fortran
> polyfit([1.,2.,3.], [6.,11.,18.], 2)
[1.00000,2.00000,3.00000]
```

---------------------------------------------------------------------
## `polyval`
Evaluate a polynomial at specific values.
```fortran
out = polyval(p, x)
```
#### *Parameters*
`real(DP), dimension(:), intent(IN) :: p`
> Polynomial coefficients, highest power first.

`real(DP), intent(IN) :: x`
> Value at which to evaluate the polynomial. 

`real(DP) :: out`
> Value of polynomial

#### *Example*
```Fortran
> polyval([1.00000,2.00000,3.00000], 3.0)
18.00000
```
