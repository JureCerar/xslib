# Changelog

#### To-do (someday):
- There is always something to do...

#### v2.6.0 - 27.11.2020
- Added `xslib_iostream` utilities.  
- Added `character(*)` support to `xslib_xmalloc`.  

#### v2.5.2
- Fixed .pdb read bug(s).  

#### v2.5.1
- Improvements to CMake.
- Bug fixes and code clean-up in `tpl_t`.
- Added `tpl2ndx` function.
- `tpl%makeNdx` is no longer part of `tpl_t`. See `tpl2ndx`.

#### v2.5.0 - 05.08.2020
- Better and more flexible implementation of `file_t` (can now handle frames).
- All file I/O now support out-of-order file reading.
- Added `xslibCheck()` routine
- Code clean-up
- Fixed bug in `pdh%copy`

#### v2.4.0 - 22.06.2020
- Changed CMake flags.
- Internal procedures were renamed to be more consistent.
- Added `xslib_plot` object for gnuplot interface.
- Added custom string merge - `smerge`.
- Bug fix in `gro_t` title output.
- Improved `ftoa`, `f8toa` to handle leading "0".
- Fixed bug in `xrealloc`.
- More clean include module output.

#### v2.3.0 - 15.04.2020
- Each file type now has type independent functions (if you are feeling adventurous).
- Added frame check to `dcd_t` when counting frames.
- Corrected c_sifeof and fixed print in `trr_t`.
- Added file dump to `file_t`.
- Improved `file_t` class.
- Added better implementation of `ctoa`.
- Fixed getColor not being public?
- Added dummy initialize to backup & progress.
- Added `isnumber`, `isspace`, `isnum`, and `ischar`
- Added xslib_cpuInfo; NOTE: It is not part of xslib.
- Assertion -- only logical input (REAL comparison is defined by user)
- Removed `file_t%open( ..., first, last, stride )` -- should be user defined.

#### v2.2.1 - 24.03.2020
- Added `obj%frame%copy` utility for `gro_t`, `pdb_t`, and `xyz_t`.
- Improved documentation.
- Bug fixes in `cub_t` and added documentation.

#### v2.2.0 - 04.02.2020
- Fixed documentation typos.  
- Added `xslib_xmalloc` module.  
- Added .trr file handling; new type `trr_t`.
- Fixed `replaceText`.
- Add `file_t%open( file, first, last, stride )`.
- Bugfix in `crt2sph` and `rotate`.
- Added `assert()` function.  
- `file_t` is now always in [nm] units.
- Added `Makefile` as backup.
- Added more strict testing.

#### v2.1.1 - 21.01.2020
- Minor bug fixes.
- Added both shared and static library.

#### v2.1.0 - 17.01.2020
- Improved and restructured most routines.   
- Better internal error handling.
- Added my implementation of Double Linked List - DLL.  
- Implemented proper `xtc%skip`.
- Added support for `.dcd` and `.cub` files.  
- Added new timing routines.
- Changed type naming from `<obj>_file` to `<obj>_t`; Less confusion with cxslib.
- Implemented box matrix `box(3,3)` and associated `obj%get_box()` and `obj%getCubicBox()`
- Fixed bug for .pdb charge.  
- Bugfixes.
- CMake changes and Improved compiler flags.

#### v1.x - v2.0
- [?] Internal releases.

#### v1.0.1 - 30.05.2019
- API improvement.
- Improved CMake.
- Switched to soft tabs.
- Fixed `write_time()` function.

#### v1.0.0 - 28.03.2019
- Added frame skip to all conf. objects - `obj%next()`.
- Added `obj%set` routine that allows to simply set first/last frame and stride.
- Removed `linest` (now in xslib_array.f90).
- Removed `elapsedTime`, `getTime` and `timeStamp()`.
- Added NEW timing routines `get_wtime()`, `write_time()`, `msleep()`.
- Added `ndx%display()` to write present groups.
- Improved write function for all conf. objects to compensate for missing data.
- Added vector `rotate` function.
- Esthetical fixes to CMAKE.
- Bunch of bugfixes.

#### v0.3.1 - 14.03.2019
- Added (somewhat decent) API documentation.
- Updated CMake.
- Fixed bugs in 'gro%write()'. #FFS
- `str()` is no longer interface, but polymorphic function.  
- `str()` can now handle arrays (very crudely).  
- `ndx%tpl2ndx()` is now part of standard procedures.
- Added `pathname()` function.  
- Fixed bug where `backup()` function would not work correctly if full path was passed (basically it did the opposite of backup).  
- `frame_file` is now a polymorphic.
- Bug-fixes in `str_ARRAY()`
- Added alternative (more stable) `getAngle2()`.  
- **IMPORTANT:** Fixed xdrfile dependency in CMake.
- Changed file names to *xslib_<name\>.f90* and *xdrlib_<name\>.c* .  

#### v0.3.0 - 03.02.2019:
- Added to GitHub.  
- Bug fixes in `pdb_file` and `xyz_file`.  
- Renamed some variables in `trj_file` for consistency.
- Added `write_ndx` function.  
- Added tests (as examples) - `make test`
- `errorHandling` (kept for legacy reasons) now renamed to `error` and added similar `warning` directive.
- Included license - GNU GPL v3.0
- `%allocate()`, `%deallocate()` and `%initialize()` procedures for `frameArray` are now subroutines. (Whose idea it was to make them functions in first place?).
- All derived types `<obj>_data` renamed to `<obj>_file`.
- `frame_file` can now handle only a single frame. Now consists of `%open()`, `%read_next()`, and `%close()` procedures.

#### v0.2.7 - 15.01.2019
- Included partially written API documentation in `doc/`.
- Renamed and added some `frame_data` derived type procedures for consistency: `%add()`, `%read()`, and `%reset()`. Added procedures `%getNframes()`, `%getNfiles()`, `%getNatoms()`, and `%getBox()`.

#### v0.2.6 - 19.12.2018
- Added `backup()` function.
- Various bug fixes.

#### v0.2.5
- Debugged `gro%write` for velocities.
- Hardcoded flags for CMAKE.  
- Defined `type(ndx_data)` and added new `ndx%read()` procedure.

#### v0.2.4
- Bug fixes for PGI compiler. #FML #FFS  

#### v0.2.3 - 28.11.2018
- Various bug fixes.

#### v0.2.2 - 09.11.2018
- Fixed "acos" instability of `getDihedral()`.  
- Added `variance()` procedure that supports real scalars & replaces `ave_and_std()` (kept for Legacy code).  
- Added `isWord()`, `linest()`, `polyfit()`, and `findNearest()` functions.  
- Added **.xyz** format.  
- Split `smearing` routine into *Width* & *Length* and added a wrapper for both + debugging.
- Added "jobfarm" routine `frame%get()`, `frame%add()`  
- Use (learn) CMAKE instead of MAKE.  
- Defined **.tpl** format. (not anymore "between" formats)
- Standardized operations for all file formats: `read, write, allocate, deallocate, initialize` + ...  
- Renamed some routines (for consistency): `stripComment()`, `nextUnit()`, `errorHandling()`, `replaceText()`, `countLines()`, `nextOccurrance()`, `progressBar()`, `minImg()`, `getDihedral()`, `getAngle()`, `getDistance()`
- Debuged `xyz%read()`
- Added tests and removed them the next second.
- Debugged pointers in `frame_data`.
- Directly included [libxdrfile](https://github.com/wesbarnett/libxdrfile) package by [wesbarnett](https://github.com/wesbarnett).  
- `tpl_data` now works through pointer.
- Promptly removed `COM()` and `AssignMass()` functions.

#### v0.2.1 - 16.05.2018
- Added `COM()` and `AssignMass()` functions.
- Added `append_gro()` procedure.

#### v0.2.0 - 23.04.2018
- Imported [libxdrfile](https://github.com/wesbarnett/libxdrfile) for reading **.trr** and **.xtc** trajectory files.

#### v0.1.2 - 20.04.2018
- Added `getTime()` function.  

#### v0.1.1
- Added debug flags to `Makefile`.  
- Debugged `pdh_data` procedures.  
- Debugged `ftoa()` function.  

#### v0.1.0
- First stable(-ish) release.  
