# cstring
Module `xslib_cstring` contains function for manupulating character strings.

---------------------------------------------------------------------
## `str`
Converts the specified SCALAR or ARRAY (of any kind) into a string. Optionally, output format can be defined with `fmt` argument. In case input values is an ARRAY a custom delimiter can be defined (default is whitespace). 
```fortran
out = str(value, fmt)
```
```fortran
out = str(value, fmt, delim)
```
#### *Parameters*
`class(*), dimmension(..), intent(IN) :: value`
> Value of any kind or shape to be transformed into character. 

`character(*), intent(IN), optional :: fmt`
> Format specifier. See [Developer Guide and Reference](https://www.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/a-to-z-reference/e-to-f/format.html) for more info.  

`character(*), intent(IN), optional :: delim`
> Separator to use when joining the string. Only applies for ARRAYS. Default is whitespace. 

`character(:), allocatable :: out`
> Output string. 

#### *Examples*
```Fortran
> str(1)
"1"
> str(1.0, "(f5.3)")
"1.000"
> str([1,2], DELIM=",")
"1,2"
```

---------------------------------------------------------------------
## `smerge`
Merge two stings of arbitrary length based on logical mask.
```fortran
out = smerge(tsource, fsource, mask)
```
#### *Parameters*
`character(*), intent(IN) :: tsource, fsource`
> Return string if mask is TRUE or FALSE, respectivly.

`logical, intent(IN) :: mask`
> Selection logical mask. 

`character(:), allocatable :: out`
> Return string. 

#### *Example*
```fortran
> smerge("Foo", "Bar", .True.)
"Foo"
> smerge("Foo", "Bar", .False.)
"Bar"
```

---------------------------------------------------------------------
## `toUpper` 
Return string in all upper case.
```fortran
out = toUpper(string)
```
#### *Parameters*
`character(*), intent(IN) :: string`
> Input string.

`character(LEN) :: out`
> Output string. Same length as `string`.

#### *Example*
```fortran
> toUpper("Foo Bar")
"FOO BAR"
```

---------------------------------------------------------------------
## `toLower` 
Return string in all lower case.
```fortran
out = toLower(string)
```
#### *Parameters*
`character(*), intent(IN) :: string`
> Input string.

`character(LEN) :: out`
> Output string. Same length as `string`.

#### *Example*
```fortran
> toLower("Foo Bar")
"foo bar"
```

---------------------------------------------------------------------
## `toTitle` 
Return string where all words are capitalized.
```fortran
out = toTitle(string)
```
#### *Parameters*
`character(*), intent(IN) :: string`
> Input string.

`character(LEN) :: out`
> Output string. Same length as `string`.

#### *Example*
```fortran
> toTitle("foo bar")
"Foo Bar"
```

---------------------------------------------------------------------
## `swapcase` 
Return string where all cases have been fliped (lower to upper, and vice versa).
```fortran
out = swapcase(string)
```
#### *Parameters*
`character(*), intent(IN) :: string`
> Input string.

`character(LEN) :: out`
> Output string. Same length as `string`.

#### *Example*
```fortran
> swapcase("Foo Bar")
"fOO bAR"
```

---------------------------------------------------------------------
## `strip` 
Removes all characters after delimiter. Useful for removing "comments" from a string.
```fortran
out = strip(string, delim)
```
#### *Parameters*
`character(*), intent(IN) :: string`
> Input string.

`character(*), intent(IN) :: delim`
> Separator use for delimiting a string. 

`character(LEN) :: out `
> Output string. Same length as `string`.

#### *Example*
```fortran
> strip("Foo,Bar", ",")
"Foo"
```

---------------------------------------------------------------------
## `replace` 
Replaces a specified phrase with another specified phrase.
```fortran
out = replace(string, old, new)
```
#### *Parameters*
`character(*), intent(IN) :: string`
> Input string.

`character(*), intent(IN) :: old, new`
> String to replace and string to replace with. 

`character(:), allocatable :: out`
> Output string.

#### *Example*
```fortran
> replace("This is Foo", "Foo", "Bar")
"This is Bar"
```

---------------------------------------------------------------------
## `isAlpha` 
Check if string is alphabetical character element wise.
```fortran
out = isAlpha(string)
```
#### *Parameters*
`character(*), intent(IN) :: string`
> Input string.

`logical :: out`
> Is input an ASCII alphabetical character.

#### *Example*
```fortran
> isAlpha("A")
.True.
> isAlpha("1")
.False.
```

---------------------------------------------------------------------
## `isNumber` 
Check if character is an ASCII number element wise.
```fortran
out = isNumber(string)
```
#### *Parameters*
`character(*), intent(IN) :: string`
> Input string.

`logical :: out`
> Is input an ASCII number character?

#### *Example*
```fortran
> isNumber("1")
.True.
> isNumber("A")
.False.
```

---------------------------------------------------------------------
## `isSpace` 
Check if character is an ASCII whitespace element wise.
```fortran
out = isSpace(string)
```
#### *Parameters*
`character(*), intent(IN) :: string`
> Input string.

`logical :: out`
> Is input an ASCII whitespace?

#### *Example*
```fortran
> isSpace(" ")
.True.
> isSpace("1")
.False.
```

---------------------------------------------------------------------
## `getColor`
Get terminal  ANSI escape sequences to set terminal text background, foreground, and attribute. Available colors are: `black`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`, `white`, and light variants (e.g. `lightblue`), where as available attributes are: `bold`, `dim`, `underline`, `blink`, `reverse`, and `hidden`. If no input is provided function returns "reset" escape code. Your experience may vary depending on the terminal used. For more informations see [ANSI escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code).
```fortran
out = setColor(fg, bg, attr)
```
#### *Parameters*
`character(*), intent(IN), optional :: fg`
> Foreground text color.  

`character(*), intent(IN), optional :: bg`
> Background text color.  

`character(*), intent(IN), optional :: attr`
> Text color attribute.  

`character(:), allocatable :: out`
> ANSI escape code sequence.  

#### *Example*
```Fortran
> getColor(white, red, bold)
ESC[1;37;041m
```

---------------------------------------------------------------------
## `setColor`
Set text background, foreground color, and attribute using ANSI escape sequences. Available colors are: `black`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`, `white`, and light variants (e.g. `lightblue`), where as available attributes are: `bold`, `dim`, `underline`, `blink`, `reverse`, and `hidden`. 
```fortran
out = setColor(string, fg, bg, attr)
```
#### *Parameters*
`character(*), intent(IN) :: string`
> Input string. 

`character(*), intent(IN), optional :: fg`
> Foreground text color.  

`character(*), intent(IN), optional :: bg`
> Background text color.  

`character(*), intent(IN), optional :: attr`
> Text color attribute.  

`character(:), allocatable :: out`
> Colored output string.  

#### *Example*
```Fortran
> setColor("This is bold and red", FG=white, BG=red, ATTR=bold)
"This is bold and red" (use your imagination)
```

---------------------------------------------------------------------
## `strtok`
Breaks string into a series of tokens seperated by delimiter. Works in same way as [`strtok`](https://cplusplus.com/reference/cstring/strtok) in C, except instead of `NULL` feed `char(0)`.
```fortran
out = strtok(string, delim)
```
#### *Parameters*
`character(*), intent(IN) :: string`
> Input string. Feed `char(0)` to get next token on precious string.  

`character(*), intent(IN) :: delim`
> Separator use for delimiting a string.  

`character(:), allocatable :: out`
> Next output character token.  

#### *Example*
```Fortran
> strtok("Foo,Bar", ",")
"Foo"
> strtok(char(0), ",")
"Bar"
> strtok(char(0), ",")
NULL
```
#### *Note*
Should be threadsafe with OpenMP.

---------------------------------------------------------------------
## `cnttok`
Return number of tokens in a string seperated by a delimiter.
```fortran
out = cnttok(string, delim)
```
#### *Parameters*
`character(*), intent(IN) :: string`
> Input string.  

`character(*), intent(IN) :: delim`
> Separator used for delimiting a string.  

`integer :: out`
> Number of tokens on the string.  

#### *Example*
```Fortran
> cnttok("Foo,Bar", ",")
2 
```

---------------------------------------------------------------------
## `basename`
Returns basename of file name.
```fortran
out = basename(string)
```
#### *Parameters*
`haracter(*), intent(IN) :: string`
> File name string.  

`character(:), allocatable :: out`
> File base name.  

#### *Example*
```Fortran
> basename("/path/to/file.txt")
"file" 
```

---------------------------------------------------------------------
## `pathname`
Returns pathname of file. **WARNING:** Currently works only for UNIX systems.
```fortran
out = pathname(string)
```
#### *Parameters*
`character(*), intent(IN) :: string`
> File name string.  

`character(:), allocatable :: out`
> File path name.  

#### *Example*
```Fortran
> pathname("/path/to/file.txt")
"/path/to"
```

---------------------------------------------------------------------
## `extension`
Returns extension of file.
```fortran
out = extension(value)
```
#### *Parameters*
`character(*), intent(IN) :: string`
> File name string.  

`character(:), allocatable :: out`
> File extension name.  

#### *Example*
```Fortran
> pathname("/path/to/file.txt")
"txt"
```
