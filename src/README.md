## Source
- **xslib.F90**   
  Master file containing all other modules. Additionally contains xslib info.

### Utility
- **xslib_error.f90**  
  Error/Warning functions, error codes/messages, and assert.
- **xslib_cstring.f90**  
  String manipulation and transformation functions.
- **xslib_iostream.f90**  
  Wrappers for working with files as streams.
- **xslib_quickio.f90**  
  Functions for quickly reading and writing data files.
- **xslib_vector.f90**  
  Some basic vector operations.
- **xslib_time.f90**  
  Time and timing functions.

### Headers
- **fileio.h**  
  Common header for all I/O functions, containing error code definitions.

### Molecular files I/O
- **xslib_fileio.f90:**    
  Object and functions for reading ALL molecular coordinates files.
- **xslib_xyzio.f90:**  
  Object and functions for reading/writing .xyz files.  
- **xslib_groio.f90:**    
  Object and functions for reading/writing GROMACS .gro files.  
- **xslib_pdbio.f90:**  
  Object and functions for reading/writing .pdb files.  
- **xslib_xtcio.f90:**    
  Object and functions for reading/writing GROMACS .xtc files.  
- **xslib_trrio.f90:**    
  Object and functions for reading/writing GROMACS .trr files.  
- **xslib_dcdio.f90:**  
  Object and functions for reading/writing .dcd trajectory files.
- **xslib_cubio.f90:**  
  Object and functions for reading/writing Gaussian CUBE (.cub) files.
- **xdrfor.f90:**  
  Fortran wrapper for xdrfile.c
- **xdrfile.c:**  
  Functions for reading/writing portable data based on the XDR standard.

### Support files I/O  
- **xslib_ndxio.f90:**    
  Object and functions for reading/writing .ndx files.
- **xslib_tplio.f90:**   
  Object and functions for reading/writing proprietary .tpl template files.  

### Data files I/O  
- **xslib_pdhio.f90:**  
  Object and functions for reading/writing .pdh data files.

### Notes
There is also `Makefile` for xslib as a backup.   
