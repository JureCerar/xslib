# math
Module `xslib_math` contains basic mathematical functions. Supports both single and double precision (`DP`).

---------------------------------------------------------------------
## `linspace`
Return evenly spaced numbers over a specified interval.
```fortran
out = lispace(start, stop, num)
```
#### *Parameters*
`real(DP), intent(IN) :: start, stop`
> The starting and end value of the sequence. Start and end values are included in sequence.

`integer, intent(IN) :: num`
> Number of samples to generate. Must be non-negative.

`real(DP), dimension(num) :: out`
> Equally spaced numbers in the opened interval `(start,stop)`.

#### *Example*
```Fortran
> lispace(0., 1., 5)
[0.00000,0.25000,0.50000,0.75000,1.00000] 
```

---------------------------------------------------------------------
## `logspace`
Return evenly spaced numbers on a logarithmic scale over a specified interval.
```fortran
out = logspace(start, stop, num)
```
#### *Parameters*
`real(DP), intent(IN) :: start, stop`
> The starting and end value of the sequence. Start and end values are included in sequence. Muste be bigger than zero.

`integer, intent(IN) :: num`
> Number of samples to generate. Must be non-negative.

`real(DP), dimension(num) :: out`
> Logarithmically spaced numbers in the opened interval `(start,stop)`.

#### *Example*
```Fortran
> logspace(1., 10000., 5)
[1.00000,10.0000,100.000,1000.00,10000.0] 
```

---------------------------------------------------------------------
## `arange`
Return evenly spaced values within a given interval.
```fortran
out = arange(start, stop, step)
```
#### *Parameters*
`real(DP), intent(IN) :: start, stop`
> The starting and end value of the sequence. Start and end values are included in sequence.

`real(DP), intent(IN) :: step`
> Spacing between values. 

`real(DP), dimension(DIM) :: out`
> Equally spaced values. For floating point arguments, the length of the result is `int((stop - start) / step) + 1`

#### *Example*
```Fortran
> arange(0., 1., 0.25)
[0.00000,0.25000,0.50000,0.75000,1.00000]
```

---------------------------------------------------------------------
## `mix`
Retrun mix *i.e.* fractional linear interpolation between two points.
```fortran
out = mix(a, b, x)
```
#### *Parameters*
`real(DP), intent(IN) :: a, b`
> Input values to mix.

`real(DP), intent(IN) :: x`
> Fraction of the mixture. 

`real(DP) :: out`
> Output value.

#### *Example*
```Fortran
> mix(0., 1., 0.5)
0.50000
```

---------------------------------------------------------------------
## `clip`
Return clip (limit) of the value between lower and upper bound.
```fortran
out = clip(a, lower, upper)
```
#### *Parameters*
`real(DP), intent(IN) :: a`
> Input values.

`real(DP), intent(IN) :: lower, upper`
> Upper and lover bounds. 

`real(DP) :: out`
> Output value.

#### *Example*
```Fortran
> clip(0.9, 1., 2.)
1.00000
> clip(1.1, 1., 2.)
1.10000
> clip(2.1, 0., 1.)
2.00000
```

---------------------------------------------------------------------
## `isclose`
Check if two values are within a tolerance delta.
```fortran
out = isclose(a, b, delta)
```
#### *Parameters*
`real(DP), intent(IN) :: a, b`
> Input values to compare.

`real(DP), intent(IN) :: delta`
> Tolerance parameter.

`logical :: out`
> Output value.

#### *Example*
```Fortran
> isclose(1.1, 1.0, 0.2)
.True.
> isclose(1.5, 1.0, 0.2)
.False.
```

---------------------------------------------------------------------
## `interp`
Return linear interpolation for increasing array points.
```fortran
out = interp(x, xp, yp)
```
#### *Parameters*
`real(DP), dimension(:), intent(IN) :: x`
> x-coordinates at which to evaluate the interpolated values.

`real(DP), dimension(:), intent(IN) :: xp, yp`
> x- and y-coordinates of the data points to be interpolated. Must be same size. 

`real(DP), dimension(:) :: out`
> Interpolated values, same shape as `x`.

#### *Example*
```Fortran
> x = [0.,0.25,0.50,0.75,1.]
> out = interp(x, [0.,1.], [0.,1.])
[0.00000,0.25000,0.50000,0.75000,1.00000]
```

---------------------------------------------------------------------
## `trapz`
Return integral of given array using the composite trapezoidal rule.
```fortran
out = trapz(y, x)
```
```fortran
out = trapz(y, dx)
```
#### *Parameters*
`real(DP), dimension(:), intent(IN) :: y`
> Input array to integrate.

`real(DP), dimension(:), intent(IN) :: x`
> The sample points corresponding to the `y` values. Same size as `y`.

`real(DP), intent(IN) :: dx`
> The spacing between sample points.

`real(DP) :: out`
> Definite integral of `y`.

#### *Example*
```Fortran
> out = trapz([0.,1.], [0.,1.])
0.50000
> out = trapz([0.,1.], 1.)
0.50000
```

---------------------------------------------------------------------
## `gradient`
Return gradient (derivative) of an array using finite difference method.
```fortran
out = gradient(y, x)
```
```fortran
out = gradient(y, dx)
```
#### *Parameters*
`real(DP), dimension(:), intent(IN) :: y`
> Input array to derivate.

`real(DP), dimension(:), intent(IN) :: x`
> The sample points corresponding to the `y` values. 

`real(DP), intent(IN) :: dx`
> The spacing between sample points `y`.

`real(DP), dimension(:) :: out`
> Derivative at each value of `y`. Is same shape as `y`.

#### *Example*
```Fortran
> out = gradient([0.,1.], [0.,1.])
[1.00000,1.00000]
> out = gradient([0.,1.], 1.)
[1.00000,1.00000]
```

---------------------------------------------------------------------
## `gcd` and `lcm`
Return the greatest common divisor (GCM) or least common multiple (LCM) of the specified arguments.
```fortran
out = gcd (a, b)
```
```fortran
out = lcm(a, b)
```
#### *Parameters*
`integer, intent(IN) :: a, b`
> Input array to derivate.

`integer :: out`
> The greatest common divisor or least common multiple.

#### *Example*
```Fortran
> out = gcd(106, 901)
53
> out = lcm(12, 17)
204
```
