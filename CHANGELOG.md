# Changelog

#### To-do:
- [ ] Finish writing API documentation.
- [ ] Return multi-file functionality to `frame_file` in more "graceful" manner.

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
