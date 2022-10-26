# vector
Module `xslib_vector` contains function for manipulation of 3D-vectors. Supports both single and double precision (`DP`). Default dimension of vectors is `DIM = 3`. 

## `cross`
Return the cross product of two vectors i.e. `a × b`. Note that cross product is anticommutative *i.e.* `(a × b) = -(b × a)`.
```fortran
out = cross(a, b)
```
#### *Parameters*
`real(DP), dimension(DIM), intent(IN) :: a, b`
> Input vectors.

`real(DP), dimension(DIM) :: out`
> Output vector.

#### *Example*
```Fortran
> cross([1.,0.,0.], [0.,1.,0.])
[0.0000000,0.0000000,1.0000000]
```
---------------------------------------------------------------------

## `rotate`
Rotate vector by specified angle around vector `vec` or axis.
```fortran
out = rotate(a, vec, angle)
```
```fortran
out = rotate(a, axis, angle)
```
#### *Parameters*
`real(DP), dimesnion(DIM), intent(IN) :: a`
> Input vector.

`real(DP), dimesnion(DIM), intent(IN) :: vec`
> Vector to rotate around.

`character, intent(IN) :: axis`
> Axis of rotation i.e. `x`, `y` or `z`

`real(DP), intent(IN) :: angle`
> Angle of rotation in radians.

`real(DP), dimesnion(DIM) :: out`
> Output vector.

#### *Example*
```Fortran
> rotate([1.,0.,0.], [0.,1.,0.], PI/2)
[0.0000000,0.0000000,-1.0000000]
> rotate([1.,0.,0.], "y", PI/2)
[0.0000000,0.0000000,-1.0000000]
```
---------------------------------------------------------------------

## `deg2rad` and `rad2deg`
Convert angles from degrees to radians and back.
```fortran
out = deg2rad(angle)
```
```fortran
out = rad2deg(angle)
```
#### *Parameters*
`real(DP), intent(IN) :: angle`
> Input angle in degrees or radians.

`real(DP) :: out`
> Input angle in radians or degrees.

#### *Example*
```Fortran
> deg2rad(180.)
3.14159274
> rad2deg(3.14159274)
180.0000000
```
---------------------------------------------------------------------

## `crt2sph` and `sph2crt`
Convert cartesian to spherical coordinate system: `x,y,z` -> `r,theta,phi` and back.
<!-- Add image of spherical coordinate system -->
```fortran
out = crt2sph(a)
```
```fortran
out = sph2crt(a)
```
#### *Parameters*
`real(DP), dimension(DIM), intent(IN) :: a`
> Input vector in cartesian or spherical coordinate system.

`out` : *real or double, out*
> Output vector in spherical or cartesian coordinate system.

#### *Example*
```Fortran
> crt2sph([1.,0.,0.])
[1.0000000,1.5707964,0.0000000]
> sph2crt([1.,PI/2,0.])
[1.0000000,0.0000000,0.0000000]
```
---------------------------------------------------------------------

## `crt2cyl` and `cyl2crt`
Convert from cartesian to cylindrical coordinate system: `x,y,z` -> `r,theta,z` and back.
<!-- Add image of cylindrical coordinate system -->
```fortran
out = crt2cyl(a)
```
```fortran
out = cyl2crt(a)
```
#### *Parameters*
`real(DP), dimension(DIM), intent(IN) :: a`
> Input vector in cartesian or cylindrical coordinate system.

`out` : *real or double, out*
> Output vector in cylindrical or cartesian coordinate system.

#### *Example*
```Fortran
> crt2cyl([0.,1.,0.])
[1.0000000,1.5707964,0.0000000]
> cyl2crt([1.,PI/2,0.])
[0.0000000,1.0000000,0.0000000]
```
---------------------------------------------------------------------

## `distance`
Returns distance between two points (vectors).
```fortran
out = distance(a, b)
```
#### *Parameters*
`real(DP), dimension(DIM), intent(IN) :: a, b`
> Input vector.

`real(DP) :: out`
> Output distance.

#### *Example*
```Fortran
> distance([0.,0.,0.],[1.,1.,1.])
1.73205
```
---------------------------------------------------------------------

## `angle`
Calculates angle between three points (vectors).
```fortran
out = angle(a, b, c)
```
#### *Parameters*
`real(DP), dimension(DIM), intent(IN) :: a, b, c`
> Input vector.

`real(DP) :: out`
> Output angle in radians.

#### *Example*
```Fortran
> angle([1.,0.,0.],[0.,0.,0.],[0.,1.,0.])
1.57079637
```
---------------------------------------------------------------------

## `dihedral`
Returns dihedral angle (theta) between four points.
```fortran
out = dihedral(a, b, c, d)
```
#### *Parameters*
`real(DP), dimension(DIM), intent(IN) :: a, b, c, d`
> Input vectors.

`real(DP) :: out`
> Output angle in radians.

#### *Example*
```Fortran
> dihedral([0., 0., 1.],[0., 0., 0.],[1., 0., 0.],[1., 1., 0.])
1.57079637
```
---------------------------------------------------------------------
