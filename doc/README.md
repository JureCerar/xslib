# Table of Contents
* [`ARRAY` - Array operations](#array---array-operations)
  * [`LINSPACE` - Generate evenly spaced numbers](#linspace---generate-evenly-spaced-numbers)
  * [`LOGSPACE` - Generate logarithmically spaced numbers](#logspace---generate-logarithmically-spaced-numbers)
  * [`ARANGE` - Generate equally spaced values](#arange---generate-equally-spaced-values)
  * [`INTERP` - Descritpion](#interp---descritpion)
  * [`TRAPZ` - Trapezoidal rule integration](#trapz---trapezoidal-rule-integration)
  * [`GRADIENT` - Calculate gradient of an array](#gradient---calculate-gradient-of-an-array)
* [`CSTRING` - Functions for string manipulation.](#cstring---functions-for-string-manipulation)
  * [`STR` - Converts value to string](#str---converts-value-to-string)
  * [`SMERGE` - Merge stings](#smerge---merge-stings)
  * [`TOLOWER` - Return lower case](#tolower---return-lower-case)
  * [`TOUPPER` - Return upper case](#toupper---return-upper-case)
  * [`TOTITLE` - Capitalize letters](#totitle---capitalize-letters)
  * [`SWAPCASE` - Swap case](#swapcase---swap-case)
  * [`STRIP` - Removes trailing characters](#strip---removes-trailing-characters)
  * [`REPLACE` - Replace string](#replace---replace-string)
  * [`ISALPHA` - Is alphanumeric character](#isalpha---is-alphanumeric-character)
  * [`ISDIGIT` - Is digit character](#isdigit---is-digit-character)
  * [`ISSPACE` - Is whitespace character](#isspace---is-whitespace-character)
  * [`GETCOLOR` - Get ANSI escape color code](#getcolor---get-ansi-escape-color-code)
  * [`SETCOLOR` - Set string ANSI color](#setcolor---set-string-ansi-color)
  * [`STRTOK` - Split string into tokens](#strtok---split-string-into-tokens)
  * [`CNTTOK` - Count tokens in a string](#cnttok---count-tokens-in-a-string)
* [`ERRORH` - Error and warning handling](#errorh---error-and-warning-handling)
  * [`ERROR` - Display error and exit](#error---display-error-and-exit)
  * [`WARNING` - Display warning and continue](#warning---display-warning-and-continue)
  * [`ASSERT` - Assert logical expression](#assert---assert-logical-expression)
* [`FITTING` - Function fitting](#fitting---function-fitting)
  * [`LINFIT` - Least squares linear fit](#linfit---least-squares-linear-fit)
  * [`POLYFIT` - Least squares polynomial fit](#polyfit---least-squares-polynomial-fit)
  * [`POLYVAL` - Evaluate a polynomial](#polyval---evaluate-a-polynomial)
* [`LIST` - Linked list functions](#list---linked-list-functions)
  * [`LIST_T` - Polymorphic linked list](#listt---polymorphic-linked-list)
  * [`LIST%APPEND` - Append element to the list](#listappend---append-element-to-the-list)
  * [`LIST%CLEAR` - Removes all elements from the list](#listclear---removes-all-elements-from-the-list)
  * [`LIST%COUNT` - Count elements on the list](#listcount---count-elements-on-the-list)
  * [`LIST%EXTEND` - Append array of elements to the list](#listextend---append-array-of-elements-to-the-list)
  * [`LIST%INDEX` - Return index of element on the list](#listindex---return-index-of-element-on-the-list)
  * [`LIST%INSERT` - Append element to the list at specified position](#listinsert---append-element-to-the-list-at-specified-position)
  * [`LIST%LEN` - Count number of elements on the list](#listlen---count-number-of-elements-on-the-list)
  * [`LIST%GET` - Get an element from the list](#listget---get-an-element-from-the-list)
  * [`LIST%POP` - Removes element at the specified position from the list](#listpop---removes-element-at-the-specified-position-from-the-list)
  * [`LIST%REMOVE` - Remove element from the list](#listremove---remove-element-from-the-list)
  * [`LIST%REVERSE` - Reverse element order on the list](#listreverse---reverse-element-order-on-the-list)
  * [`LIST%SAME_TYPE_AS` - Check if element on list is same type as reference](#listsametypeas---check-if-element-on-list-is-same-type-as-reference)
  * [`LIST%SET` - Change element on the list](#listset---change-element-on-the-list)
  * [`LIST%SORT` - Sort elements on the list](#listsort---sort-elements-on-the-list)
  * [`LIST%WRITE` - Formatted and unformatted write](#listwrite---formatted-and-unformatted-write)
* [`MATH` - Basic mathematical functions](#math---basic-mathematical-functions)
  * [`FACTORIAL` - Return factorial of an integer](#factorial---return-factorial-of-an-integer)
  * [`PERM` - Permutation of two numbers](#perm---permutation-of-two-numbers)
  * [`COMB` - Combination of two numbers](#comb---combination-of-two-numbers)
  * [`MIX` - Linear interpolation of two numbers](#mix---linear-interpolation-of-two-numbers)
  * [`CLIP` - Limit the values in an array.](#clip---limit-the-values-in-an-array)
  * [`ISCLOSE` - Checks if two values are within a tolerance](#isclose---checks-if-two-values-are-within-a-tolerance)
  * [`GCD` - Return greatest common divisor](#gcd---return-greatest-common-divisor)
  * [`LCM` - Return least common multiple](#lcm---return-least-common-multiple)
* [`PATHLIB` - Functions for path and file manipulation.](#pathlib---functions-for-path-and-file-manipulation)
  * [`FILENAME` - Get file name of pathname](#filename---get-file-name-of-pathname)
  * [`BASENAME` - Get base name of pathname](#basename---get-base-name-of-pathname)
  * [`DIRNAME` - Get base name of pathname](#dirname---get-base-name-of-pathname)
  * [`EXTNAME` - Get extension name of pathname](#extname---get-extension-name-of-pathname)
  * [`BACKUP` - Backup existing file](#backup---backup-existing-file)
* [`SORT` - Sorting functions](#sort---sorting-functions)
  * [`SWAP` - Swap input values](#swap---swap-input-values)
  * [`SORT` - Sort input array](#sort---sort-input-array)
  * [`QSORT` - Sort input array using Quicksort](#qsort---sort-input-array-using-quicksort)
  * [`MSORT` - Sort input array using Merge sort](#msort---sort-input-array-using-merge-sort)
  * [`HSORT` - Sort input array using Merge sort](#hsort---sort-input-array-using-merge-sort)
* [`STAT` - Basic statistics functions](#stat---basic-statistics-functions)
  * [`NORMAL` - Random number on a normal distribution](#normal---random-number-on-a-normal-distribution)
  * [`MEAN` - Arithmetic mean](#mean---arithmetic-mean)
  * [`STDEV` - Get standard deviation](#stdev---get-standard-deviation)
  * [`VARIANCE` - Get variance of an array](#variance---get-variance-of-an-array)
  * [`WELFORD` - Welford's online variance](#welford---welfords-online-variance)
  * [`WELFORD_FINALIZE` - Correct Welford's online variance](#welfordfinalize---correct-welfords-online-variance)
  * [`HISTOGRAM` - Get histogram of an array.](#histogram---get-histogram-of-an-array)
* [`TIME` - Time functions](#time---time-functions)
  * [`WTIME` - Precise time](#wtime---precise-time)
  * [`WRITETIME` - Write precise time](#writetime---write-precise-time)
  * [`MSLEEP` - Suspend execution for time interval](#msleep---suspend-execution-for-time-interval)
* [`VECTOR` - Vector functions](#vector---vector-functions)
  * [`CROSS` - Vector cross product](#cross---vector-cross-product)
  * [`ROTATE` - Vector rotation](#rotate---vector-rotation)
  * [`DEG2RAD` - Degrees to radians](#deg2rad---degrees-to-radians)
  * [`RAD2DEG` - Radians to degrees](#rad2deg---radians-to-degrees)
  * [`CRT2SPH` - Cartesian to spherical](#crt2sph---cartesian-to-spherical)
  * [`SPH2CRT` - Spherical to cartesian](#sph2crt---spherical-to-cartesian)
  * [`CRT2CYL` - Cartesian to cylindrical](#crt2cyl---cartesian-to-cylindrical)
  * [`CYL2CRT` - Cylindrical to cartesian](#cyl2crt---cylindrical-to-cartesian)
  * [`DISTANCE` - Vector distance](#distance---vector-distance)
  * [`ANGLE` - Vector angle](#angle---vector-angle)
  * [`DIHEDRAL` - Vector dihedral angle](#dihedral---vector-dihedral-angle)
* [`XMALLOC` - Memory allocation](#xmalloc---memory-allocation)
  * [`XMALLOC` - Allocate memory](#xmalloc---allocate-memory)
  * [`XCALLOC` - Allocate and initialize memory](#xcalloc---allocate-and-initialize-memory)
  * [`XREALLOC` - Reallocate memory](#xrealloc---reallocate-memory)
# `ARRAY` - Array operations
Module `xslib_array` contains function for array operations. Supports both single and double precision (`DP`) functions.
## `LINSPACE` - Generate evenly spaced numbers
#### DESCRIPTION
Return evenly spaced numbers over a specified interval.
#### USAGE
```Fortran
out = linspace(start, stop, num)
```
#### PARAMETERS
* `real(ANY), intent(IN) :: start, stop`</br>
The starting and end value of the sequence. `start` and `stop` values are included in sequence.
* `integer, intent(IN) :: num`</br>
Number of samples to generate. Must be non-negative.
* `real(ANY), dimension(num) :: out`</br>
Equally spaced numbers in the opened interval `(start, stop)`.
#### EXAMPLE
```Fortran
> linspace(0.0, 1.0, 5)
[0.00, 0.25, 0.50, 0.75, 1.00]
```
## `LOGSPACE` - Generate logarithmically spaced numbers
#### DESCRIPTION
Return evenly spaced numbers on a logarithmic scale over a specified interval.
#### USAGE
```Fortran
out = logspace(start, stop, num)
```
#### PARAMETERS
* `real(ANY), intent(IN) :: start, stop`</br>
The starting and end value of the sequence. `start` and `stop` values are included in sequence. Must be bigger than zero.
* `integer, intent(IN) :: num`</br>
Number of samples to generate. Must be non-negative.
* `real(ANY), dimension(num) :: out`</br>
Logarithmically spaced numbers in the opened interval `(start, stop)`.
#### EXAMPLE
```Fortran
> logspace(1.0, 10000.0, 5)
[1.0, 10.0, 100.0, 1000.0, 10000.0]
```
## `ARANGE` - Generate equally spaced values
#### DESCRIPTION
Return equally spaced values within a given interval.
#### USAGE
```Fortran
out = arange(start, stop, step)
```
#### PARAMETERS
* `real(ANY), intent(IN) :: start, stop`</br>
The starting and end value of the sequence. `stop` value is not necessarily included in sequence.
* `real(ANY), intent(IN) :: step`</br>
Spacing between values.
* `real(ANY), dimension(LENGTH) :: out`</br>
Equally spaced values. The `LENGTH` of the result is equal to `int((stop - start) / step) + 1`.
#### EXAMPLE
```Fortran
> arange(0.0, 1.0, 0.25)
[0.00, 0.25, 0.50, 0.75, 1.00]
```
## `INTERP` - Descritpion
#### DESCRIPTION
One-dimensional linear interpolation for monotonically increasing sample points.

Returns the one-dimensional piecewise linear interpolant to a function with given
discrete data points `(xp, fp)`, evaluated at `x`.
#### USAGE
```Fortran
out = interp(x, xp, yp)
```
#### PARAMETERS
* `real(ANY), dimension(:), intent(IN) :: x`</br>
x-coordinates at which to evaluate the interpolated values.
* `real(ANY), dimension(:), intent(IN) :: xp, yp`</br>
x- and y-coordinates of the data points to be interpolated. Must be same size.
* `real(DP), dimension(:) :: out`</br>
Interpolated values, same shape as `x`.
#### EXAMPLE
```Fortran
> x = [0.00, 1.00, 1.50, 2.50, 3.50]
> out = interp(x, [1.0, 2.0, 3.0], [3.0, 2.0, 0.0])
[4.00, 3.00, 2.50, 1.00, -1.00]
```
## `TRAPZ` - Trapezoidal rule integration
#### DESCRIPTION
Integrate along the given axis using the composite trapezoidal rule.

If `x` is provided, the integration happens in sequence along its
elements - they are not sorted. If points are equidistant use `dx` option.
#### USAGE
```Fortran
out = trapz(y, x)
out = trapz(y, dx)
```
#### PARAMETERS
* `real(ANY), dimension(:), intent(IN) :: y`</br>
Input array to integrate.
* `real(ANY), dimension(:), intent(IN) :: x`</br>
The sample points corresponding to the `y` values. Must be same size as `y`.
* `real(ANY), intent(IN), OPTIONAL :: dx`</br>
The spacing between sample points. Default: 1.0.
* `real(ANY) :: out`</br>
Definite integral of `y`.
#### EXAMPLE
```Fortran
> out = trapz([0.0, 1.0], [0.0, 1.0])
0.50000
> out = trapz([0.0, 1.0], dx=1.0)
0.50000
> out = trapz([0.0, 1.0])
0.50000
```
## `GRADIENT` - Calculate gradient of an array
#### DESCRIPTION
Return the gradient (derivative) of an array using finite difference method.

The gradient is computed using second order accurate central differences in
the interior points and either first or second order accurate one-sides
(forward or backwards) differences at the boundaries. The returned gradient
hence has the same shape as the input array.
#### USAGE
```Fortran
out = gradient(y, x)
out = gradient(y, dx=dx)
```
#### PARAMETERS
* `real(ANY), dimension(:), intent(IN) :: y`</br>
Input array to derivate.
* `real(ANY), dimension(:), intent(IN) :: x`</br>
The sample points corresponding to the `y` values. Must be same size as `y`.
* `real(ANY), intent(IN), OPTIONAL :: dx`</br>
The spacing between sample points `y`. Default: 1.0.
* `real(ANY), dimension(:) :: out`</br>
Derivative at each value of `y`. Is same shape as `y`.
#### EXAMPLE
```Fortran
> out = gradient([0.0, 1.0], [0.0, 1.0])
[1.00000,1.00000]
> out = gradient([0.0, 1.0], dx=1.0)
[1.00000,1.00000]
> out = gradient([0.0, 1.0])
[1.00000,1.00000]
```
--------------------------------------------------------------------------------
# `CSTRING` - Functions for string manipulation.
Module `xslib_cstring` contains function for manipulating character strings.
## `STR` - Converts value to string
#### DESCRIPTION
Converts the specified SCALAR or ARRAY (of any kind) into a string. Optionally,
output format can be defined with `fmt` argument. In case input values is an ARRAY
a custom delimiter can be defined (default is whitespace).
#### USAGE
```fortran
out = str(value, fmt=fmt, delim=delim)
```
#### PARAMETERS
* `class(*), dimension(..), intent(IN) :: value`</br>
Value of any kind or shape to be transformed into character.
* `character(*), intent(IN), OPTIONAL :: fmt`</br>
Valid fortran format specifier. Default is compiler representation.
* `character(*), intent(IN), OPTIONAL :: delim`</br>
Separator to use when joining the string. Only applies for ARRAYS. Default is whitespace.
* `character(:), allocatable :: out`</br>
Output string.
#### EXAMPLE
```Fortran
> str(1)
"1"
> str(1.0, "(f5.3)")
"1.000"
> str([1,2], DELIM=",")
"1,2"
```
## `SMERGE` - Merge stings
#### DESCRIPTION
Select values two arbitrary length stings according to a logical mask. The result
is equal to `tsource` if `mask` is `.TRUE.`, or equal to `fsource` if it is `.FALSE.`.
#### USAGE
```fortran
out = smerge(tsource, fsource, mask)
```
#### PARAMETERS
* `character(*), intent(IN) :: fsource`</br>
Return string if mask is `.TRUE.`.
* `character(*), intent(IN) :: fsource`</br>
Return string if mask is `.FALSE.`.
`logical, intent(IN) :: mask`
Selection logical mask.
* `character(:), allocatable :: out`</br>
Return string of variable length.
#### EXAMPLE
```fortran
> smerge("Top", "Bottom", .True.)
"Top"
> smerge("Top", "Bottom", .False.)
"Bottom"
```
## `TOLOWER` - Return lower case
#### DESCRIPTION
Return the string with all the cased characters are converted to lowercase.
#### USAGE
```fortran
out = toLower(string)
```
#### PARAMETERS
* `character(*), intent(IN) :: string`</br>
Input string.
* `character(LEN) :: out`</br>
Output string. Same length as `string`.
###### *Example*
```fortran
> toLower("Hello, WORLD!")
"hello world!"
```
## `TOUPPER` - Return upper case
#### DESCRIPTION
Return the string with all the cased characters are converted to uppercase.
#### USAGE
```fortran
out = toUpper(string)
```
#### PARAMETERS
* `character(*), intent(IN) :: string`</br>
Input string.
* `character(LEN) :: out`</br>
Output string. Same length as `string`.
###### *Example*
```fortran
> toLower("Hello, WORLD!")
"HELLO WORLD!"
```
## `TOTITLE` - Capitalize letters
#### DESCRIPTION
Return a titlecased version of the string where words start with an
uppercase character and the remaining characters are lowercase.
#### USAGE
```fortran
out = toTitle(string)
```
#### PARAMETERS
* `character(*), intent(IN) :: string`</br>
Input string.
* `character(LEN) :: out`</br>
Output string. Same length as `string`.
#### EXAMPLE
```fortran
> toTitle("hello, world!")
"Hello, World!"
```
## `SWAPCASE` - Swap case
#### DESCRIPTION
Return a string with uppercase characters converted to lowercase and vice versa.
Note that it is not necessarily true that `swapCase(swapCase(s)) == s`.
#### USAGE
```fortran
out = swapCase(string)
```
#### PARAMETERS
* `character(*), intent(IN) :: string`</br>
Input string.
* `character(LEN) :: out`</br>
Output string. Same length as `string`.
#### EXAMPLE
```fortran
> swapCase("Hello, WORLD!")
"hELLO, world!"
```
## `STRIP` - Removes trailing characters
#### DESCRIPTION
Return a string with the leading and trailing characters removed. The `delim` argument is
a string specifying the characters after which characters to be removed. If omitted,
the argument defaults to removing whitespace. Useful for removing "comments" from a string.
#### USAGE
```fortran
out = strip(string, delim)
```
#### PARAMETERS
* `character(*), intent(IN) :: string`</br>
Input string.
* `character(*), intent(IN) :: delim`</br>
Separator use for delimiting a string.
* `character(LEN) :: out `</br>
Output string. Same length as `string`.
#### EXAMPLE
```fortran
> strip("Hello, WORLD!", ",")
"Hello"
```
## `REPLACE` - Replace string
#### DESCRIPTION
Return a string with all occurrences of substring `old` replaced by `new`.
#### USAGE
```fortran
out = replace(string, old, new)
```
#### PARAMETERS
* `character(*), intent(IN) :: string`</br>
Input string.
* `character(*), intent(IN) :: old`</br>
String to be replaced.
* `character(*), intent(IN) :: new`</br>
String to be replaced with.
* `character(:), allocatable :: out`</br>
Output string.
#### EXAMPLE
```fortran
> "Hello, World!", "World", "Universe")
"Hello, Universe!"
```
## `ISALPHA` - Is alphanumeric character
#### DESCRIPTION
Return `.True.` if all characters in the string are alphabetic and
there is at least one character, `.False.` otherwise.
#### USAGE
```Fortran
out = isAlpha(string)
```
#### PARAMETERS
* `character(*), intent(IN) :: string`</br>
Input string.
* `logical :: out`</br>
Is input string an ASCII alphabetical character.
#### EXAMPLE
```Fortran
> isAlpha("ABC")
.True.
> isAlpha("123")
.False.
```
## `ISDIGIT` - Is digit character
#### DESCRIPTION
Return `.True.` if all characters in the string are digits and
there is at least one character, `.False.` otherwise.
#### USAGE
```Fortran
out = isDigit(string)
```
#### PARAMETERS
* `character(*), intent(IN) :: string`</br>
Input string.
* `logical :: out`</br>
Is input a digit?
#### EXAMPLE
```Fortran
> isDigit("ABC")
.False.
> isDigit("123")
.True.
```
## `ISSPACE` - Is whitespace character
#### DESCRIPTION
Return `.True.` if there are only whitespace characters in the string
and there is at least one character, `.False.` otherwise.
#### USAGE
```Fortran
out = isSpace(string)
```
#### PARAMETERS
* `character(*), intent(IN) :: string`</br>
Input string.
* `logical :: out`</br>
Is input a whitespace?
#### EXAMPLE
```Fortran
> isSpace(" ")
.True.
> isSpace("A")
.False.
```
## `GETCOLOR` - Get ANSI escape color code
#### DESCRIPTION
Add some colors in your life! Get terminal ANSI escape sequences to set
terminal text background, foreground, and attribute. Available colors are:
`black`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`, `white`, and their `light`
variants (e.g. `lightblue`). Available attributes are: `bold`, `dim`, `underline`,
`blink`, `reverse`, and `hidden`. If no input is provided function returns "reset"
escape code. Your experience may vary depending on the terminal used. For more
information see [ANSI escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code).
#### USAGE
```fortran
out = getColor(fg=fg, bg=bg, attr=attr)
```
#### PARAMETERS
* `character(*), intent(IN), OPTIONAL :: fg`</br>
Foreground text color. Default is None.
* `character(*), intent(IN), OPTIONAL :: bg`</br>
Background text color. Default is None.
* `character(*), intent(IN), OPTIONAL :: attr`</br>
Text color attribute. Default is None.
* `character(:), allocatable :: out`</br>
Terminal color ANSI escape code sequence.
#### EXAMPLE
```Fortran
> getColor("white", "red", "bold")
ESC[1;37;041m
```
#### NOTE
Max length of output string is 11 i.e. `E[*;**;***m`.
## `SETCOLOR` - Set string ANSI color
#### DESCRIPTION
Add some colors in your life! Set text background, foreground color, and attribute
using ANSI escape sequences. Available colors are: `black`, `red`, `green`,
`yellow`, `blue`, `magenta`, `cyan`, `white`, and their `light` variants (e.g. `lightblue`).
Available attributes are: `bold`, `dim`, `underline`, `blink`, `reverse`, and `hidden`.
Your experience may vary depending on the terminal used. For more information
see [ANSI escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code).
#### USAGE
```fortran
out = setColor(string, fg=fg, bg=gb, attr=attr)
```
#### PARAMETERS
* `character(*), intent(IN) :: string`</br>
Input string.
* `character(*), intent(IN), OPTIONAL :: fg`</br>
Foreground text color. Default is None.
* `character(*), intent(IN), OPTIONAL :: bg`</br>
Background text color. Default is None.
* `character(*), intent(IN), OPTIONAL :: attr`</br>
Text color attribute. Default is None.
* `character(:), allocatable :: out`</br>
Terminal color ANSI escape code sequence.
#### EXAMPLE
```Fortran
> setColor("This string is bold and red", "red", "bold")
"This string is bold and red" ! Use your imagination
```
## `STRTOK` - Split string into tokens
#### DESCRIPTION
A sequence of calls to this function split string into tokens, which are sequences
of contiguous characters separated by the delimiter `delim`.

On a first call, the function a string as argument for str, whose first character
is used as the starting location to scan for tokens. In subsequent calls, the function
expects a null character `char(0)` and uses the position right after the end of the last
token as the new starting location for scanning.

Once the end character of string is found in a call to `strtok`, all subsequent calls to
this function (with a null character as the first argument) return a null character.

See [`strtok`](https://cplusplus.com/reference/cstring/strtok) for full reference.
#### USAGE
```Fortran
out = strtok(string, delim)
```
#### PARAMETERS
* `character(*), intent(IN) :: string`</br>
Input string. Feed null character `char(0)` to get next token on precious string.
* `character(*), intent(IN) :: delim`</br>
Separator use for delimiting a string.
* `character(:), allocatable :: out`</br>
Next output character token.
#### EXAMPLE
```Fortran
> strtok("Hello, World!", " ")
"Hello,"
> strtok(char(0), " ")
"World!"
> strtok(char(0), " ")
NULL
```
#### NOTES
Should be thread-safe with OpenMP.
## `CNTTOK` - Count tokens in a string
#### DESCRIPTION
Count number of tokens in a string separated by a delimiter `delim`.
#### USAGE
```fortran
out = cnttok(string, delim)
```
#### PARAMETERS
* `character(*), intent(IN) :: string`</br>
Input string.
* `character(*), intent(IN) :: delim`</br>
Separator used for delimiting a string.
* `integer :: out`</br>
Number of tokens on the string.
#### EXAMPLE
```Fortran
> cnttok("Hello, World!", " ")
2
```
--------------------------------------------------------------------------------
# `ERRORH` - Error and warning handling
Module `xslib_errorh` contains functions of error and warning handling.
## `ERROR` - Display error and exit
#### DESCRIPTION
Write error message to STDERR and terminate the program.
#### USAGE
```Fortran
call error(message)
```
#### PARAMETERS
* `character(*), intent(IN) :: message`</br>
Error message to display.
#### EXAMPLE
```Fortran
> call error("Error message")
"[ERROR]: Error message"
```
#### NOTES
Use with `__FILE__` and `__LINE__` macros to get extended error message.
```Fortran
#define error(x) error_(x, __FILE__, __LINE__)
```
## `WARNING` - Display warning and continue
#### DESCRIPTION
Write warning message to STDERR and continue the program.
#### USAGE
```Fortran
call warning(message)
```
#### PARAMETERS
* `character(*), intent(IN) :: message`</br>
Warning message to display.
#### EXAMPLE
```Fortran
> call warning("Warning message")
"[WARNING]: Warning message"
```
#### NOTES
Use with `__FILE__` and `__LINE__` macros to get extended warning message.
```Fortran
#define warning(x) warning_(x, __FILE__, __LINE__)
```
## `ASSERT` - Assert logical expression
#### DESCRIPTION
Assert logical expression. On fail write error message to STDERR and terminate the program.
#### USAGE
```Fortran
call assert(expression)
```
#### PARAMETERS
* `logical, dimension(..), intent(IN) :: expression`</br>
Logical expression to be evaluated.
#### EXAMPLE
```Fortran
> call assert (array == 0)
"[ERROR]: Assertion failed at: [1,1]"
```
#### NOTES
Use with `__FILE__` and `__LINE__` macros to get extended assertion message.
```Fortran
#define assert(x) assert_ (x, __FILE__, __LINE__)
```
--------------------------------------------------------------------------------
# `FITTING` - Function fitting
Module `xslib_fitting` contains basic regression functions. Supports both single and double precision (`DP`).
## `LINFIT` - Least squares linear fit
#### DESCRIPTION
Calculate a linear least-squares regression for two sets of data.
#### USAGE
```fortran
out = linfit(x, y)
```
#### PARAMETERS
* `real(ANY), dimension(:), intent(IN) :: x, y`</br>
Two sets of data points to be fitted. Both arrays must have the same length.
* `real(ANY), dimension(2) :: out`</br>
Linear coefficients: slope, and intercept (highest power first).
#### EXAMPLE
```Fortran
> linfit([1.0, 2.0, 3.0], [2.0, 3.0, 4.0])
[1.0, 2.0]
```
#### NOTE
For large array sizes use [LAPACK](https://www.netlib.org/lapack/).
## `POLYFIT` - Least squares polynomial fit
#### DESCRIPTION
Calculate a polynomial least-squares regression for two sets of data.
#### USAGE
```fortran
out = polyfit(x, y, deg)
```
#### PARAMETERS
* `real(ANY), dimension(:), intent(IN) :: x, y`</br>
Two sets of data points to be fitted. Both arrays must have the same length.
* `integer, intent(IN) :: deg`</br>
Degree of the fitting polynomial.
* `real(ANY), dimension(DEG) :: out`</br>
Polynomial coefficients: highest powers first.
#### EXAMPLE
```Fortran
> polyfit([1.0, 2.0, 3.0], [6.0, 11.0, 18.0], 2)
[1.0, 2.0, 3.0]
```
#### NOTE
For large arrays use [LAPACK](https://rosettacode.org/wiki/Polynomial_regression#Fortran).
## `POLYVAL` - Evaluate a polynomial
#### DESCRIPTION
Evaluate a polynomial at specific values.
#### USAGE
```fortran
out = polyval(p, x)
```
#### PARAMETERS
* `real(ANY), dimension(:), intent(IN) :: p`</br>
Polynomial coefficients: highest powers first.
* `real(ANY), dimension(..), intent(IN) :: x`</br>
Value or array at which to evaluate the polynomial.
* `real(ANY), dimension(..) :: out`</br>
Value or array of polynomial.
#### EXAMPLE
```Fortran
> polyval([1.0, 2.0, 3.0], 1.0)
6.0
> polyval([1.0, 2.0, 3.0], [1.0, 2.0, 3.0])
[6.0, 11.0, 18.0]
```
--------------------------------------------------------------------------------
#  `LIST` - Linked list functions
Module `xslib_list` contains primitive implementation of unlimited polymorphic linked list.
List currently supports only `INT32`, `INT64`, `REAL32`, `REAL64`, `LOGICAL`, and `CHARACTER(*)`
variable types. To add new derived TYPE support you only have write extension to `equal`, `copy`,
and (optional) `write` functions.
## `LIST_T` - Polymorphic linked list
#### DESCRIPTION
Implementation of unlimited polymorphic linked list derived type variable. Supports `INT32`, `INT64`,
`REAL32`, `REAL64`, `LOGICAL`, and `CHARACTER(*)` variable types. Variables on list cannot be directly
accessed and can be set via `append`, `extend`, and `set` functionality or retrieved via `get` functionality.
#### USAGE
```Fortran
> type(list_t) :: list
```
## `LIST%APPEND` - Append element to the list
#### DESCRIPTION
Append new element to the end of the list.
#### USAGE
```Fortran
call list%append(elem)
```
#### PARAMETERS
* `class(*), intent(IN) :: elem`</br>
Element to be added end of the list.
#### EXAMPLE
```Fortran
> type(list_t) :: list
> list = list_t([1, 2, 3])
> call list%append(4])
[1, 2, 3, 4]
```
## `LIST%CLEAR` - Removes all elements from the list
#### DESCRIPTION
Remove ALL elements from the list.
#### USAGE
```Fortran
call list%clear()
```
#### EXAMPLE
```Fortran
> type(lit_t) :: list
> list = list_t([1, 2, 3])
> call list%clear()
[]
```
## `LIST%COUNT` - Count elements on the list
#### DESCRIPTION
Returns the number of `elem` elements (with the specified value) on the list.
#### USAGE
```Fortran
out = list%count(elem)
```
#### PARAMETERS
* `class(*), intent(IN) :: elem`</br>
Value of elements to search on the list.
* `integer :: out`</br>
Number of elements with specified value on the list.
#### EXAMPLE
```Fortran
> type(list_t) :: list
> list = list_t([1, 2, 1])
> list%count(1)
2
```
## `LIST%EXTEND` - Append array of elements to the list
#### DESCRIPTION
Append array of elements to the end of the list.
#### USAGE
```Fortran
call list%append(array)
```
#### PARAMETERS
* `class(*), dimension(:), intent(IN) :: array`</br>
Array of elements to be added to the list.
#### EXAMPLE
```Fortran
> type(list_t) :: list
> list = list_t([1, 2])
> call list%extend([3, 4, 5])
[1, 2, 3, 4, 5]
```
## `LIST%INDEX` - Return index of element on the list
#### DESCRIPTION
Returns the index of the first element on list with `elem` value.
#### USAGE
```Fortran
out = list%index(elem)
```
#### PARAMETERS
* `class(*), intent(IN) :: elem`</br>
Value of element to index.
* `integer :: out`</br>
Index of element on the list. Returns zero if element is not present.
#### EXAMPLE
```Fortran
> type(list_t) :: list
> list = list_t([1, 2, 3])
> list%index(1)
1
> list%index(4)
0
```
## `LIST%INSERT` - Append element to the list at specified position
#### DESCRIPTION
Adds an element `elem` at the specified `pos` position on the list. If position index is outside the list range
it is either appended to the list if the index is larger than the list or prepended in index is smaller than one.
#### USAGE
```Fortran
call list%insert(pos, elem)
```
#### PARAMETERS
* `integer, intent(IN) :: pos`</br>
A number specifying in which position to insert the element.
* `class(*), intent(IN) :: elem`</br>
Element to be added to the list.
#### EXAMPLE
```Fortran
> type(list_t) :: list
> list = list_t([1, 2, 3])
> list%insert(2, 1.5)
[1, 1.5, 2, 3]
```
## `LIST%LEN` - Count number of elements on the list
#### DESCRIPTION
Get number of ALL elements on the list.
#### USAGE
```Fortran
out = list%len()
```
#### PARAMETERS
* `integer :: out`</br>
Number of all elements on the list.
#### EXAMPLE
```Fortran
> type(list_t) :: list
> list = list_t([1, 2, 3])
> list%len()
3
```
## `LIST%GET` - Get an element from the list
#### DESCRIPTION
Get element at `pos` index from the list. Raises error if `pos` index
is out of range.
#### USAGE
```Fortran
call list%get(pos, elem)
```
#### PARAMETERS
* `integer, intent(IN) :: pos`</br>
A number specifying at which position to get element.
* `class(*), intent(IN) :: elem`</br>
Corresponding element from to the list.
#### EXAMPLE
```Fortran
> type(list_t) :: list
> list = list_t([1, 2, 3])
> call list%get(1, val)
> print *, val
1
```
## `LIST%POP` - Removes element at the specified position from the list
#### DESCRIPTION
Removes the element at the specified position `pos`. Last element is removed if
`pos` is not specified.
#### USAGE
```Fortran
call list%pop(pos=pos)
```
#### PARAMETERS
* `integer, intent(IN), OPTIONAL :: pos`</br>
A number specifying the position of the element you want to remove.
Last element is removed if not specified.
#### EXAMPLE
```Fortran
> type(list_t) :: list
> list = list_t([1, 2, 3, 4])
> call list%pop()
[1, 2, 3]
> call list%pop(1)
[2, 3]
```
## `LIST%REMOVE` - Remove element from the list
#### DESCRIPTION
Removes the first occurrence of `elem` from the list.
#### USAGE
```Fortran
call list%remove(value)
```
#### PARAMETERS
* `class(*), intent(IN) :: elem`</br>
Element to be removed from the list.
#### EXAMPLE
```Fortran
> type(list_t) :: list
> list = list_t([1, 2, 3])
> call list%remove(2)
[1, 3]
```
## `LIST%REVERSE` - Reverse element order on the list
#### DESCRIPTION
Reverse element order on the list.
#### USAGE
```Fortran
call list%reverse()
```
#### EXAMPLE
```Fortran
> type(list_t) :: list
> list = list_t([1, 2, 3])
> call list%reverse()
[3, 2, 1]
```
## `LIST%SAME_TYPE_AS` - Check if element on list is same type as reference
#### DESCRIPTION
Returns `.True.` if the dynamic type of element at index `pos` is the same as the dynamic type of `elem`.
#### USAGE
```Fortran
out = list%same_type_as(pos, elem)
```
#### PARAMETERS
* `integer, intent(IN) :: pos`</br>
A number specifying at which position to check the element.
* `class(*), intent(IN) :: elem`</br>
Element against which to compare the type.
* `logical :: out`</br>
Returns `.True.` if evaluated elements are of same type.
#### EXAMPLE
```Fortran
> type(list_t) :: list
> call list%extend([1, 2])
> list%same_type_as(1, 0)
.True.
> list%same_type_as(1, "one")
.False.
```
## `LIST%SET` - Change element on the list
#### DESCRIPTION
Change value of element at index `pos` on the list. Raises error if `pos` index
is out of list range.
#### USAGE
```Fortran
call list%set(pos, elem)
```
#### PARAMETERS
* `integer, intent(IN) :: pos`</br>
A number specifying at which position to set element.
* `class(*), intent(IN) :: elem`</br>
Element to be replaced on the list.
#### EXAMPLE
```Fortran
> type(list_t) :: list
> list = list_t([1, 2, 3])
> call list%set(1, 0)
[0, 2, 3]
```
## `LIST%SORT` - Sort elements on the list
#### DESCRIPTION
Sort elements on the list in ascending order.
__WARING:__ Implementation pending!
#### USAGE
```Fortran
call list%sort()
```
#### EXAMPLE
```Fortran
> type(list_t) :: list
> list = list_t([3, 1, 2, 4])
> call list%sort()
[1, 2, 3, 4]
```
## `LIST%WRITE` - Formatted and unformatted write
#### DESCRIPTION
Allows for formatted and unformatted write of `list_t`.
#### USAGE
```Fortran
print *, list_t
write (*, *) list_t
```
#### EXAMPLE
```Fortran
> type(list_t) :: list
> list = list_T([1, 2, 3])
> print *, list
[1, 2, 3]
> write (*, *) list_t
[1, 2, 3]
```
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
# `MATH` - Basic mathematical functions
Module `xslib_math` contains basic mathematical functions. Supports both single and double precision (`DP`).
## `FACTORIAL` - Return factorial of an integer
#### DESCRIPTION
Return factorial of an integer `n`. Returns error if `n` is not integral or is negative.
#### USAGE
```Fortran
out = factorial(n)
```
#### PARAMETERS
* `integer(ANY), intent(IN) :: n`</br>
Input value.
* `integer(ANY) :: out`</br>
Factorial of an input value.
#### EXAMPLE
```Fortran
> factorial(5)
120
```
## `PERM` - Permutation of two numbers
#### DESCRIPTION
Return the number of ways to choose `k` items from `n` items without repetition and with order.
#### USAGE
```Fortran
out = perm(n, k)
```
#### PARAMETERS
* `integer(ANY), intent(IN) :: k, n`</br>
...
* `integer(ANY) :: out`</br>
...
#### EXAMPLE
```Fortran
> perm(6, 4)
360
```
## `COMB` - Combination of two numbers
#### DESCRIPTION
Return the number of ways to choose `k` items from `n` items without repetition and without order.
#### USAGE
```Fortran
out = comb(n, k)
```
#### PARAMETERS
* `integer(ANY), intent(IN) :: k, n`</br>
...
* `integer(ANY) :: out`</br>
...
#### EXAMPLE
```Fortran
> comb(6, 4)
15
```
## `MIX` - Linear interpolation of two numbers
#### DESCRIPTION
Return mix *i.e.* fractional linear interpolation between two values. If `x = 0.0` then
return `a` if `x = 1.0` return `b` otherwise return linear interpolation of `a` and `b`.
#### USAGE
```Fortran
out = mix(a, b, x)
```
#### PARAMETERS
* `real(ANY), intent(IN) :: a, b`</br>
Two input values to mix.
* `real(ANY), intent(IN) :: x`</br>
Fraction of the mixture: form 0.0 to 1.0.
* `real(ANY) :: out`</br>
Output value.
#### EXAMPLE
```Fortran
> mix(0.0, 5.0, 0.25)
1.25
```
## `CLIP` - Limit the values in an array.
#### DESCRIPTION
Clip (limit) the values in an array.

Given an interval, values outside the interval are clipped to the interval edges.
For example, if an interval of `[0, 1]` is specified, values smaller than 0 become `0`,
and values larger than 1 become `1`.
#### USAGE
```Fortran
out = clip(a, lower, upper)
```
#### PARAMETERS
* `real(ANY), intent(IN) :: a`</br>
Input value
* `real(ANY), intent(IN) :: lower, upper`</br>
Upper and lower bounds.
* `real(ANY) :: out`</br>
Output value.
#### EXAMPLE
```Fortran
> clip(0.9, 1., 2.)
1.00000
> clip(1.1, 1., 2.)
1.10000
> clip(2.1, 1., 2.)
2.00000
```
## `ISCLOSE` - Checks if two values are within a tolerance
#### DESCRIPTION
Checks if two values are within a tolerance. The tolerance values are positive,
typically very small numbers. The relative difference `(rtol * abs(b))` and the
absolute difference `atol` are added together to compare against the absolute
difference between `a` and `b`.
#### USAGE
```Fortran
out = isClose(a, b, rtol=rtol, atol=atol)
```
#### PARAMETERS
* `real(ANY), intent(IN) :: a, b`</br>
Input values to compare.
* `real(ANY), intent(IN), OPTIONAL :: rtol`</br>
The relative tolerance parameter. Default: 1.0e-05
* `real(ANY), intent(IN), OPTIONAL :: rtol`</br>
The absolute tolerance parameter. Default: 1.0e-08
* `logical :: out`</br>
Output value.
#### EXAMPLE
```Fortran
> isclose(1.1, 1.0, atol=1.0)
.True.
> isclose(1.5, 1.0, RTOL=1.0)
.False.
```
## `GCD` - Return greatest common divisor
#### DESCRIPTION
Return the greatest common divisor (GCD) of the specified integer arguments.
If any of the arguments is nonzero, then the returned value is the largest
positive integer that is a divisor of all arguments. If all arguments are
zero, then the returned value is 0.
#### USAGE
```Fortran
out = gcd (a, b)
```
#### PARAMETERS
* `integer(ANY), intent(IN) :: a, b`</br>
Input values.
* `integer(ANY) :: out`</br>
Output value.
#### EXAMPLE
```Fortran
> gcd(106, 901)
53
```
## `LCM` - Return least common multiple
#### DESCRIPTION
Return the least common multiple (LCM) of the specified integer arguments.
If all arguments are nonzero, then the returned value is the smallest
positive integer that is a multiple of all arguments. If any of the arguments
is zero, then the returned value is 0.
#### USAGE
```Fortran
out = lcm(a, b)
```
#### PARAMETERS
* `integer(ANY), intent(IN) :: a, b`</br>
Input values.
* `integer(ANY) :: out`</br>
Output value.
#### EXAMPLE
```Fortran
> out = lcm(12, 17)
204
```
--------------------------------------------------------------------------------
# `PATHLIB` - Functions for path and file manipulation.
Module `xslib_pathlib` contains function for path and file manipulation.
## `FILENAME` - Get file name of pathname
#### DESCRIPTION
Return the file name of pathname `path`. If pathname is a folder it will return an empty string.
#### USAGE
```Fortran
out = filename(path)
```
#### PARAMETERS
* `character(*), intent(IN) :: path`</br>
String containing pathname.
* `character(:), allocatable :: out`</br>
String containing pathname file name.
#### EXAMPLE
```Fortran
> filename("/path/to/file.txt")
"file.txt"
> filename("/path/to/")
""
```
## `BASENAME` - Get base name of pathname
#### DESCRIPTION
Return the base name of pathname `path`. If pathname is folder it will return an empty string.
#### USAGE
```Fortran
out = basename(path)
```
#### PARAMETERS
* `character(*), intent(IN) :: path`</br>
String containing pathname.
* `character(:), allocatable :: out`</br>
String containing pathname base name.
#### EXAMPLE
```Fortran
> basename("/path/to/file.txt")
"file"
> basename("/path/to/")
""
```
## `DIRNAME` - Get base name of pathname
#### DESCRIPTION
Return the directory name of pathname `path`. If pathname is file it will return an empty string.
#### USAGE
```Fortran
out = dirname(path)
```
#### PARAMETERS
* `character(*), intent(IN) :: path`</br>
String containing pathname.
* `character(:), allocatable :: out`</br>
String containing pathname directory name.
#### EXAMPLE
```Fortran
> dirname("/path/to/file.txt")
"/path/to/"
> dirname("file.txt")
""
```
## `EXTNAME` - Get extension name of pathname
#### DESCRIPTION
Return the extension name of pathname `path`. If pathname is not a file it will return an empty string.
#### USAGE
```Fortran
out = extname(path)
```
#### PARAMETERS
* `character(*), intent(IN) :: path`</br>
String containing pathname.
* `character(:), allocatable :: out`</br>
String containing pathname extension name.
#### EXAMPLE
```Fortran
> extname("/path/to/file.txt")
"txt"
> extname("/path/to/")
""
```
## `BACKUP` - Backup existing file
#### DESCRIPTION
Backup a existing file i.e. checks if `file` already exists and renames it to `#file.{n}#` where.
#### USAGE
```Fortran
call backup(file, status=status)
```
#### PARAMETERS
* `character(*), intent(IN) :: file`</br>
File name to backup.
* `integer, intent(OUT), OPTIONAL :: status`</br>
Return status returns 0 on success and nonzero otherwise.
#### EXAMPLE
```Fortran
call backup("file.txt", status)
if (status != 0) error stop "Backup failed."
```
--------------------------------------------------------------------------------
# `SORT` - Sorting functions
Module `xslib_sort` contains function sorting arrays.
Supports `INT32`, `INT64`, `REAL32`, `REAL64`, and `CHARACTER` input arrays.
## `SWAP` - Swap input values
#### DESCRIPTION
Swap values of `a` and `b`.
#### USAGE
```Fortran
call swap (a, b)
```
#### PARAMETERS
* `class(*), intent(INOUT) :: a, b`</br>
Values to be swapped. Must be same KIND.
#### EXAMPLE
```Fortran
> print *, a, b
1.0, 2.0
> call swap(a, b)
> print *, a, b
2.0, 1.0
```
## `SORT` - Sort input array
#### DESCRIPTION
Sort input array in ascending order. Different sorting algorithm can be
selected: `quicksort`, `mergesort`, or `heapsort`. Default is `quicksort`.
Order contains argument sort order from original array.
#### USAGE
```Fortran
call sort(array, kind=kind, order=order)
```
#### PARAMETERS
* `class(*), dimension(:), intent(INOUT) :: array`</br>
Input array to be sorted. Supports array of any KIND.
* `character(*), intent(IN), OPTIONAL :: kind`</br>
Sorting algorithm: `quicksort` (default), `mergesort`, or `heapsort`.
* `integer, dimension(:), intent(OUT), OPTIONAL :: order`</br>
Value order in sorted array in respect to original. Same size as `array`.
#### EXAMPLE
```Fortran
> array = [1.0, 4.0, 3.0, 2.0]
> call sort (array, kind="quicksort", kind=order)
> print *, array
1.0, 2.0, 3.0, 4.0
> print *, order
1, 4, 3, 2
```
## `QSORT` - Sort input array using Quicksort
#### DESCRIPTION
Sort input array in ascending order using using [Quicksort](https://en.wikipedia.org/wiki/Quicksort)
algorithm. Order contains argument sort order from original array.
#### USAGE
```Fortran
call qsort(array, order=order)
```
#### PARAMETERS
* `class(*), dimension(:), intent(INOUT) :: array`</br>
Input array to be sorted. Supports array of any KIND.
* `integer, dimension(:), intent(OUT), OPTIONAL :: order`</br>
Value order in sorted array in respect to original. Same size as `array`.
#### EXAMPLE
```Fortran
> array = [1.0, 4.0, 3.0, 2.0]
> call qsort(array, kind=order)
> print *, array
1.0, 2.0, 3.0, 4.0
> print *, order
1, 4, 3, 2
```
## `MSORT` - Sort input array using Merge sort
#### DESCRIPTION
Sort input array in ascending order using using [Merge sort](https://en.wikipedia.org/wiki/Merge_sort)
algorithm. Order contains argument sort order from original array.
#### USAGE
```Fortran
call msort(array, order=order)
```
#### PARAMETERS
* `class(*), dimension(:), intent(INOUT) :: array`</br>
Input array to be sorted. Supports array of any KIND.
* `integer, dimension(:), intent(OUT), OPTIONAL :: order`</br>
Value order in sorted array in respect to original. Same size as `array`.
#### EXAMPLE
```Fortran
> array = [1.0, 4.0, 3.0, 2.0]
> call msort(array, kind=order)
> print *, array
1.0, 2.0, 3.0, 4.0
> print *, order
1, 4, 3, 2
```
## `HSORT` - Sort input array using Merge sort
#### DESCRIPTION
Sort input array in ascending order using using [Heapsort](https://en.wikipedia.org/wiki/Heapsort)
algorithm. Order contains argument sort order from original array.
#### USAGE
```Fortran
call hsort(array, order=order)
```
#### PARAMETERS
* `class(*), dimension(:), intent(INOUT) :: array`</br>
Input array to be sorted. Supports array of any KIND.
* `integer, dimension(:), intent(OUT), OPTIONAL :: order`</br>
Value order in sorted array in respect to original. Same size as `array`.
#### EXAMPLE
```Fortran
> array = [1.0, 4.0, 3.0, 2.0]
> call hsort(array, kind=order)
> print *, array
1.0, 2.0, 3.0, 4.0
> print *, order
1, 4, 3, 2
```
--------------------------------------------------------------------------------
# `STAT` - Basic statistics functions
Module `xslib_stats` contains basic statistics functions. Supports both single and double precision (`DP`).
## `NORMAL` - Random number on a normal distribution
#### DESCRIPTION
Return random samples from a normal (Gaussian) distribution. Random number sequence
can be initialized with [`srand`](https://gcc.gnu.org/onlinedocs/gfortran/SRAND.html) function.
#### USAGE
```Fortran
out = normal(mu, sigma)
```
#### PARAMETERS
* `real(ANY), intent(IN) :: mu, sigma`</br>
The mean `mu` and variance `sigma` values of normal distribution.
* `real(ANY) :: out`</br>
Random value on a normal distribution.
#### EXAMPLE
```Fortran
> normal(0.0, 1.0)
0.123456
```
## `MEAN` - Arithmetic mean
#### DESCRIPTION
Return mean (average) value of an array.
#### USAGE
```Fortran
out = mean(array)
```
#### PARAMETERS
* `class(*) dimension(:), intent(IN) :: array`</br>
Input array of `REAL` or `INT` kind.
* `real(ANY) :: out`</br>
Average value of array.
#### EXAMPLE
```Fortran
> mean([1, 2, 3, 4, 5])
3.0
```
## `STDEV` - Get standard deviation
#### DESCRIPTION
Return standard deviation of an array.
#### USAGE
```Fortran
out = stdev(array)
```
#### PARAMETERS
* `class(*), dimension(:), intent(IN) :: array`</br>
Input array of `REAL` or `INT` kind.
* `real(ANY) :: out`</br>
Standard deviation of array values.
#### EXAMPLE
```Fortran
> stdev([1, 2, 3, 4, 5])
1.58114
```
## `VARIANCE` - Get variance of an array
#### DESCRIPTION
Calculate variance of an array.
#### USAGE
```Fortran
out = variance(array)
```
#### PARAMETERS
* `class(*), dimension(:), intent(IN) :: array`</br>
Input array of kind `INT` or `REAL`.
* `real(ANY) :: out`</br>
Variance of array values.
#### EXAMPLE
```Fortran
> variance([1, 2, 3, 4, 5])
2.5
```
## `WELFORD` - Welford's online variance
#### DESCRIPTION
[Welford's online algorithm](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm)
for calculating variance. Final variance must be "corrected" with `welford_finalize` call.
#### USAGE
```Fortran
call welford(array, mean, variance, n)
```
#### PARAMETERS
* `class(*), dimension(:), intent(IN) :: array`</br>
Input array of kind `INT` or `REAL`.
* `real(ANY), dimension(:), intent(INOUT) :: mean, variance`</br>
Mean value and variance of array values. Must be same size as `array`.
* `integer, intent(IN) :: n`</br>
Sequential number of an array. Must be non-zero.
#### EXAMPLE
```Fortran
> do i = 1, NP
>   call random_number(array)
>   call welford(array, mean, variance, i)
> end do
> call welford_finalize(mean, variance, NP)
```
## `WELFORD_FINALIZE` - Correct Welford's online variance
#### DESCRIPTION
Correct final variance of Welford's online algorithm.
#### USAGE
```Fortran
call welford_finalize(mean, variance, n)
```
#### PARAMETERS
* `real(ANY), dimension(:), intent(INOUT) :: mean, variance`</br>
Mean value and variance of arrays. Must be same size.
* `integer, intent(IN) :: n`</br>
Total number of averaged arrays. Must be non-zero.
#### EXAMPLE
```Fortran
> do i = 1, NP
>   call random_number(array)
>   call welford(array, mean, variance, i)
> end do
> call welford_finalize(mean, variance, NP)
```
## `HISTOGRAM` - Get histogram of an array.
#### DESCRIPTION
Calculate histogram distribution of an array.
#### USAGE
```Fortran
out = histogram(array, nbins, min=min, max=max)
```
#### PARAMETERS
* `class(*), dimension(:), intent(IN) :: array`</br>
Input array of kind `INT` or `REAL`.
* `integer, intent(IN) :: nbins`</br>
Number of equal-width bins in the given range.
* `real, intent(IN), OPTIONAL :: min, max`</br>
The lower and upper range of the bins. If not provided, ranges are
simply: `min(array)` and `max(array)`, respectively.
* `integer, dimension(nbins) :: out`</br>
The values of the histogram.
#### EXAMPLE
```Fortran
> call random_number(array)
> histogram(array, 5, MIN=0.0, MAX=1.0)
[9, 10, 8, 11, 11]
```
--------------------------------------------------------------------------------
# `TIME` - Time functions
Module `xslib_time` contains functions for timing and displaying time.
## `WTIME` - Precise time
#### DESCRIPTION
Returns precise wall time in seconds since an unspecified time. The absolute
value of `wtime` is meaningless, only differences between subsequent calls to
this function should be used.
#### USAGE
```Fortran
out = wtime()
```
#### PARAMETERS
* `real(REAL64) :: out`</br>
Precise time in seconds.
#### EXAMPLE
```Fortran
> wtime()
1234.567890000
```
## `WRITETIME` - Write precise time
#### DESCRIPTION
Transforms time in seconds from `wtime` or `OMP_get_wtime` to string
in format `ddd-hh:mm:ss.sss`. Format can be shorter depending on the length of time.
#### USAGE
```Fortran
out = writeTime(time)
```
#### PARAMETERS
* `real(REAL64), intent(IN) :: time`</br>
Precise time in seconds.
* `character(64) :: out`</br>
Time string in format `ddd-hh:mm:ss.sss`.
#### EXAMPLE
```Fortran
> writeTime(151501.992d0)
"1-18:05:01.992"
```
## `MSLEEP` - Suspend execution for time interval
#### DESCRIPTION
Suspends execution for specified millisecond interval.
#### USAGE
```Fortran
call msleep(time)
```
#### PARAMETERS
* `integer, intent(IN) :: time`</br>
Time interval in milliseconds.
#### EXAMPLE
```Fortran
> call msleep(1000)
```
--------------------------------------------------------------------------------
# `VECTOR` - Vector functions
Module `xslib_vector` contains function for vector operations. Default dimension of vectors is `DIM = 3`.
Supports both single and double precision (`DP`).
## `CROSS` - Vector cross product
#### DESCRIPTION
Return the cross product of two vectors i.e. `u × v`. Note that
cross product is anti-commutative *i.e.* `(u × v) = -(v × u)`.
#### USAGE
```Fortran
out = cross(u, v)
```
#### PARAMETERS
* `real(ANY), dimension(DIM), intent(IN) :: u, v`</br>
Input vectors.
* `real(ANY), dimension(DIM) :: out`</br>
Output vector.
#### EXAMPLE
```Fortran
> cross([1.0, 0.0, 0.0], [0.0, 1.0, 0.0])
[0.0, 0.0, 1.0]
```
## `ROTATE` - Vector rotation
#### DESCRIPTION
Rotate vector by specified angle `angle` around vector `vec` or axis.
#### USAGE
```Fortran
out = rotate(v, vector, angle)
out = rotate(v, axis, angle)
```
#### PARAMETERS
* `real(ANY), dimension(DIM), intent(IN) :: v`</br>
Input vector.
* `real(ANY), dimension(DIM), intent(IN) :: vector`</br>
Vector of rotation.
* `character, intent(IN) :: axis`</br>
Axis of rotation: `x`, `y`, or `z`.
* `real(ANY), intent(IN) :: angle`</br>
Angle of rotation in radians.
* `real(ANY), dimension(DIM) :: out`</br>
Output vector.
#### EXAMPLE
```Fortran
> rotate([1.0, 0.0, 0.0], [0.0, 1.0, 0.0], PI/2)
[0.0, 0.0, -1.0]
> rotate([1.0, 0.0, 0.0], "y", PI/2)
[0.0, 0.0, -1.0]
```
## `DEG2RAD` - Degrees to radians
#### DESCRIPTION
Convert angles from degrees to radians.
#### USAGE
```Fortran
out = deg2rad(angle)
```
#### PARAMETERS
* `real(ANY), intent(IN) :: angle`</br>
Input angle in degrees.
* `real(ANY) :: out`</br>
Output angle in radians.
#### EXAMPLE
```Fortran
> deg2rad(180.0)
DIM.14159274
```
## `RAD2DEG` - Radians to degrees
#### DESCRIPTION
Convert angles from radians to degrees.
#### USAGE
```Fortran
out = rad2deg(angle)
```
#### PARAMETERS
* `real(ANY), intent(IN) :: angle`</br>
Input angle in radians.
* `real(ANY) :: out`</br>
Output angle in degrees.
#### EXAMPLE
```Fortran
> rad2deg(PI)
180.0
```
## `CRT2SPH` - Cartesian to spherical
#### DESCRIPTION
Convert vector from cartesian to spherical coordinate system: `[x, y, z]` → `[r, theta, phi]`.
#### USAGE
```Fortran
out = crt2sph(v)
```
#### PARAMETERS
* `real(ANY), dimension(DIM), intent(IN) :: v`</br>
Input vector in cartesian coordinate system.
* `real(ANY) :: out`</br>
Output vector in spherical coordinate system.
#### EXAMPLE
```Fortran
> crt2sph([1.0, 0.0, 0.0])
[1.0, 1.5707964, 0.0]
```
## `SPH2CRT` - Spherical to cartesian
#### DESCRIPTION
Convert vector from spherical to cartesian coordinate system: `[r, theta, phi]` → `[x, y, z]`.
#### USAGE
```Fortran
out = sph2crt(v)
```
#### PARAMETERS
* `real(ANY), dimension(DIM), intent(IN) :: v`</br>
Input vector in spherical coordinate system.
* `real(ANY) :: out`</br>
Output vector in cartesian coordinate system.
#### EXAMPLE
```Fortran
> sph2crt([1.0, PI/2, 0.0])
[1.0, 0.0, 0.0]
```
## `CRT2CYL` - Cartesian to cylindrical
#### DESCRIPTION
Convert vector from cartesian to cylindrical coordinate system: `[x, y, z]` → `[r, theta, z]`.
<!-- TODO: Add image of cylindrical coordinate system -->
#### USAGE
```Fortran
out = crt2cyl(v)
```
#### PARAMETERS
* `real(ANY), dimension(DIM), intent(IN) :: v`</br>
Input vector in cartesian coordinate system.
* `real(ANY) :: out`</br>
Output vector in cylindrical coordinate system.
#### EXAMPLE
```Fortran
> crt2cyl([0.0, 1.0, 0.0])
[1.0, 1.5707964, 0.0]
```
## `CYL2CRT` - Cylindrical to cartesian
#### DESCRIPTION
Convert vector from cylindrical to cartesian coordinate system: `[r, theta, z]` → `[x, y, z]`.
#### USAGE
```Fortran
out = cyl2crt(v)
```
#### PARAMETERS
* `real(ANY), dimension(DIM), intent(IN) :: v`</br>
Input vector in cylindrical coordinate system.
* `real(ANY) :: out`</br>
Output vector in cartesian coordinate system.
#### EXAMPLE
```Fortran
> cyl2crt([1.0, PI/2, 0.0])
[0.0, 1.0, 0.0]
```
## `DISTANCE` - Vector distance
#### DESCRIPTION
Calculates distance (norm) between two points (vectors).
#### USAGE
```Fortran
out = distance(a, b)
```
#### PARAMETERS
* `real(ANY), dimension(DIM), intent(IN) :: a, b`</br>
Input vector points.
* `real(ANY) :: out`</br>
Output distance.
#### EXAMPLE
```Fortran
> distance([0.0, 0.0, 0.0], [1.0, 1.0, 1.0])
1.73205
```
## `ANGLE` - Vector angle
#### DESCRIPTION
Calculates angle between three points (vectors).
#### USAGE
```Fortran
out = angle(a, b, c)
```
#### PARAMETERS
* `real(ANY), dimension(DIM), intent(IN) :: a, b, c`</br>
Input vector points.
* `real(ANY) :: out`</br>
Output angle in radians.
#### EXAMPLE
```Fortran
> angle([1.0, 0.0, 0.0], [0.0, 0.0, 0.0], [0.0, 1.0, 0.0])
1.57079637
```
## `DIHEDRAL` - Vector dihedral angle
#### DESCRIPTION
Calculates dihedral angle (theta) between four points (vectors).
#### USAGE
```Fortran
out = dihedral(a, b, c, d)
```
#### PARAMETERS
* `real(ANY), dimension(DIM), intent(IN) :: a, b, c, d`</br>
Input vector points.
* `real(ANY) :: out`</br>
Output dihedral angle in radians.
#### EXAMPLE
```Fortran
> dihedral([0, 0, 1], [0, 0, 0], [1, 0, 0], [1, 1, 0])
1.57079637
```
--------------------------------------------------------------------------------
# `XMALLOC` - Memory allocation
Modules `xslib_xmalloc` contains functions for memory allocation.
Currently supported types are: `INT32`, `INT64`, `REAL32`, `REAL64`, and `CHARACTER`
## `XMALLOC` - Allocate memory
#### DESCRIPTION
Allocates a block of memory for an array of num. elements. It's just
a nice wrapper for built-in allocation but with error handling.
#### USAGE
```Fortran
call xmalloc(object, spec, stat=stat, errmsg=errmsg)
```
#### PARAMETERS
* `class(*), ALLOCATABLE, dimension(..), intent(INOUT) :: object`</br>
Object to be allocated.
`integer, dimension(:), intent(IN) :: spec`
Object shape specification. Must be same size as `rank(object)`.
* `integer, intent(OUT), OPTIONAL :: stat`</br>
Error status code. Returns zero if no error.
* `character(*), intent(OUT), OPTIONAL :: errmsg`</br>
Error status message. Empty if no error.
#### EXAMPLE
```Fortran
> call xmalloc(array, [10, 10], STAT=status, ERRMSG=message)
```
## `XCALLOC` - Allocate and initialize memory
#### DESCRIPTION
Allocates and initializes a block of memory for an array of num. elements.
It's just a nice wrapper for built-in allocation but with error handling.
#### USAGE
```Fortran
call xcalloc(object, spec, stat=stat, errmsg=errmsg)
```
#### PARAMETERS
* `class(*), ALLOCATABLE, dimension(..), intent(INOUT) :: object`</br>
Object to be allocated.
`integer, dimension(:), intent(IN) :: spec`
Object shape specification. Must be same size as `rank(object)`.
* `integer, intent(OUT), OPTIONAL :: stat`</br>
Error status code. Returns zero if no error.
* `character(*), intent(OUT), OPTIONAL :: errmsg`</br>
Error status message. Empty if no error.
#### EXAMPLE
```Fortran
> call xcalloc(array, [10, 10], STAT=status, ERRMSG=message)
```
## `XREALLOC` - Reallocate memory
#### DESCRIPTION
Changes the size of memory block. Allocates a new memory block if
not allocated. The content of the memory block is preserved up to
the lesser of the new and old sizes, even if the block is moved to
a new location. If the new size is larger, the value of the newly
allocated portion is indeterminate.
#### USAGE
```Fortran
call xrealloc(object, spec, stat=stat, errmsg=errmsg)
```
#### PARAMETERS
* `class(*), ALLOCATABLE, dimension(..), intent(INOUT) :: object`</br>
Object to be allocated.
* `integer, dimension(:), intent(IN) :: spec`</br>
Object shape specification. Must be same size as `rank(object)`.
* `integer, intent(OUT), OPTIONAL :: stat`</br>
Error status code. Returns zero if no error.
* `character(*), intent(OUT), OPTIONAL :: errmsg`</br>
Error status message. Empty if no error.
#### EXAMPLE
```Fortran
> call xrealloc(array, [10, 10], STAT=status, ERRMSG=message)
```
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
