# Changelog

#### To-do (someday):
- [ ] Expand `csv_file` to handle any kind of variables (make polymorphic).
- [ ] Add `frame%write` (trajectory write).

#### v0.3.2 - TBA (currently v0.3.2c)
- [x] Added frame skip to all conf. objects - `obj%next()`.
- [x] Added `obj%set` routine that allows to simply set first/last frame and stride.
- [x] Removed `linest` (now in xslib_array.f90).
- [x] Removed `elapsedTime`, `getTime` and `timeStamp()`.
- [x] Added NEW timing routines `get_wtime()`, `write_time()`, `msleep()`.
- [x] Added `ndx%display()` to write present groups.
- [x] Improved write function for all conf. objects to compensate for missing data.
- [x] Added vector `rotate` function.
- [x] Aesthetic fixes to CMAKE.
- [x] Bunch of bugfixes.

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
- **IMPORTANT:** Fixed xdrfile dependency flaw in CMake.
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
