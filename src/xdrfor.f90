! This file is part of xslib
! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2020  Jure Cerar
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

module xdrfor
  use iso_c_binding
  implicit none

  ! Handle for portable binary files
  ! * The data type definition is located in xdrfile.h
  type, public, bind(C) :: xdrfile
    type(C_PTR)       :: fp, xdr
    character(C_CHAR) :: mode
    integer(C_INT)    :: buf1, buf1size, buf2, buf2size
  end type xdrfile

  ! xdrfile error code enumerator
  ! * Use exdr_message to get appropriate error message
  enum, bind(C)
    enumerator :: exdrOK, exdrHEADER, exdrSTRING, exdrDOUBLE, &
    exdrINT, exdrFLOAT, exdrUINT, exdr3DX, exdrCLOSE, exdrMAGIC, &
    exdrNOMEM, exdrENDOFFILE, exdrFILENOTFOUND, exdrNR
  end enum

  ! XDRFILE interface
  ! * Not entire interface is here. Just the ones I need.
  ! * NOTE: xrdfile read is same as write. He he he.

  ! Read/write one or more integer type variable(s)
  ! * Returns number of integers read/written. If this is negative, an error occured.
  interface xdrfile_read_int
    integer(C_INT) function xdrfile_read_int( ptr, ndata, xfp ) bind( C, NAME="xdrfile_read_int" )
      import
      integer(C_INT), intent(inout)     :: ptr    ! Pointer to memory where data should be read/written
      integer(C_INT), value, intent(in) :: ndata  ! Number of integers to write/read.
      type(xdrfile), intent(in)         :: xfp    ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_read_int

    integer(C_INT) function xdrfile_read_int_1d( ptr, ndata, xfp ) bind( C, NAME="xdrfile_read_int" )
      import
      integer(C_INT), intent(inout)     :: ptr(*) ! Pointer to memory where data should be read/written
      integer(C_INT), value, intent(in) :: ndata  ! Number of integers to write/read.
      type(xdrfile), intent(in)         :: xfp    ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_read_int_1d

    integer(C_INT) function xdrfile_read_int_2d( ptr, ndata, xfp ) bind( C, NAME="xdrfile_read_int" )
      import
      integer(C_INT), intent(inout)     :: ptr(1,*) ! Pointer to memory where data should be read/written
      integer(C_INT), value, intent(in) :: ndata    ! Number of integers to write/read.
      type(xdrfile), intent(in)         :: xfp      ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_read_int_2d
  end interface xdrfile_read_int


  ! Read/write one or more float type variable(s)
  ! * Returns number of floats read/written. If this is negative, an error occured.
  interface xdrfile_read_float
    integer(C_INT) function xdrfile_read_float( ptr, ndata, xfp ) bind( C, NAME="xdrfile_read_float" )
      import
      real(C_FLOAT), intent(inout)      :: ptr    ! Pointer to memory where data should be read/written
      integer(C_INT), value, intent(in) :: ndata  ! Number of floats to write/read.
      type(xdrfile), intent(in)         :: xfp    ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_read_float

    integer(C_INT) function xdrfile_read_float_1d( ptr, ndata, xfp ) bind( C, NAME="xdrfile_read_float" )
      import
      real(C_FLOAT), intent(inout)      :: ptr(*) ! Pointer to memory where data should be read/written
      integer(C_INT), value, intent(in) :: ndata  ! Number of floats to write/read.
      type(xdrfile), intent(in)         :: xfp    ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_read_float_1d

    integer(C_INT) function xdrfile_read_float_2d( ptr, ndata, xfp ) bind( C, NAME="xdrfile_read_float" )
      import
      real(C_FLOAT), intent(inout)      :: ptr(1,*) ! Pointer to memory where data should be read/written
      integer(C_INT), value, intent(in) :: ndata    ! Number of floats to write/read.
      type(xdrfile), intent(in)         :: xfp      ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_read_float_2d
  end interface xdrfile_read_float


  ! Read/write one or more double type variable(s)
  ! * Returns number of doubles read/written. If this is negative, an error occured.
  interface xdrfile_read_double
    integer(C_INT) function xdrfile_read_double( ptr, ndata, xfp ) bind( C, NAME="xdrfile_read_double" )
      import
      real(C_DOUBLE), intent(inout)     :: ptr    ! Pointer to memory where data should be read/written
      integer(C_INT), value, intent(in) :: ndata  ! Number of doubles to write/read.
      type(xdrfile), intent(in)         :: xfp    ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_read_double

    integer(C_INT) function xdrfile_read_double_1d( ptr, ndata, xfp ) bind( C, NAME="xdrfile_read_double" )
      import
      real(C_DOUBLE), intent(inout)     :: ptr(*) ! Pointer to memory where data should be read/written
      integer(C_INT), value, intent(in) :: ndata  ! Number of doubles to write/read.
      type(xdrfile), intent(in)         :: xfp    ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_read_double_1d

    integer(C_INT) function xdrfile_read_double_2d( ptr, ndata, xfp ) bind( C, NAME="xdrfile_read_double" )
      import
      real(C_DOUBLE), intent(inout)     :: ptr(1,*) ! Pointer to memory where data should be read/written
      integer(C_INT), value, intent(in) :: ndata    ! Number of doubles to write/read.
      type(xdrfile), intent(in)         :: xfp      ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_read_double_2d
  end interface xdrfile_read_double


  ! Decompress coordiates from XDR file to array of floats
  ! * Returns number of coordinate triplets read/written. If this is negative, an error occured.
  interface xdrfile_decompress_coord_float
    integer(C_INT) function xdrfile_decompress_coord_float( ptr, ncoord, precision, xfp ) &
      & bind( C, NAME="xdrfile_decompress_coord_float" )
      import
      real(C_FLOAT), intent(out)  :: ptr        ! Pointer to coordinates to compress ( length >= 3*ncoord )
      integer(C_INT), intent(in)  :: ncoord     ! Max number of coordinate triplets to read on input.
      real(C_FLOAT), intent(out)  :: precision  ! The precision used in the compression.
      type(xdrfile), intent(in)   :: xfp        ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_decompress_coord_float

    integer(C_INT) function xdrfile_decompress_coord_float_1d( ptr, ncoord, precision, xfp ) &
      & bind( C, NAME="xdrfile_decompress_coord_float" )
      import
      real(C_FLOAT), intent(out)  :: ptr(*)     ! Pointer to coordinates to compress ( length >= 3*ncoord )
      integer(C_INT), intent(in)  :: ncoord     ! Max number of coordinate triplets to read on input.
      real(C_FLOAT), intent(out)  :: precision  ! The precision used in the compression.
      type(xdrfile), intent(in)   :: xfp        ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_decompress_coord_float_1d

    integer(C_INT) function xdrfile_decompress_coord_float_2d( ptr, ncoord, precision, xfp ) &
      & bind( C, NAME="xdrfile_decompress_coord_float" )
      import
      real(C_FLOAT), intent(out)  :: ptr(1,*)   ! Pointer to coordinates to compress ( length >= 3*ncoord )
      integer(C_INT), intent(in)  :: ncoord     ! Max number of coordinate triplets to read on input.
      real(C_FLOAT), intent(out)  :: precision  ! The precision used in the compression.
      type(xdrfile), intent(in)   :: xfp        ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_decompress_coord_float_2d
  end interface xdrfile_decompress_coord_float


  ! Compresses coordiates to XDRFILE to array of floats
  ! * Returns number of coordinate triplets written. If this is negative, an error occured.
  interface xdrfile_compress_coord_float
    integer(C_INT) function xdrfile_compress_coord_float( ptr, ncoord, precision, xfp ) &
    & bind( C, NAME="xdrfile_compress_coord_float" )
      import
      real(C_FLOAT), intent(in)         :: ptr        ! Pointer to coordinates to decompress ( length >= 3*ncoord )
      integer(C_INT), value, intent(in) :: ncoord     ! Max number of coordinate triplets to written on output.
      real(C_FLOAT), value, intent(in)  :: precision  ! The precision used in the compression.
      type(xdrfile), intent(in)         :: xfp        ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_compress_coord_float

    integer(C_INT) function xdrfile_compress_coord_float_1d( ptr, ncoord, precision, xfp ) &
    & bind( C, NAME="xdrfile_compress_coord_float" )
      import
      real(C_FLOAT), intent(in)         :: ptr(*)     ! Pointer to coordinates to decompress ( length >= 3*ncoord )
      integer(C_INT), value, intent(in) :: ncoord     ! Max number of coordinate triplets to written on output.
      real(C_FLOAT), value, intent(in)  :: precision  ! The precision used in the compression.
      type(xdrfile), intent(in)         :: xfp        ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_compress_coord_float_1d

    integer(C_INT) function xdrfile_compress_coord_float_2d( ptr, ncoord, precision, xfp ) &
    & bind( C, NAME="xdrfile_compress_coord_float" )
      import
      real(C_FLOAT), intent(in)         :: ptr(1,*)   ! Pointer to coordinates to decompress ( length >= 3*ncoord )
      integer(C_INT), value, intent(in) :: ncoord     ! Max number of coordinate triplets to written on output.
      real(C_FLOAT), value, intent(in)  :: precision  ! The precision used in the compression.
      type(xdrfile), intent(in)         :: xfp        ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_compress_coord_float_2d
  end interface xdrfile_compress_coord_float


  interface

    ! Read string type (array of characters) variable
    ! * If no end-of-string is encountered, one byte less than this is read and end-of-string appended.
    ! * Returns number of characters read, including end-of-string.
    integer(C_INT) function xdrfile_read_string( ptr, maxlen, xfp ) bind( C, NAME="xdrfile_read_string" )
      import
      character(C_CHAR), intent(out)    :: ptr(*)  ! Pointer to memory where data should be written.
      integer(C_INT), value, intent(in) :: maxlen       ! Maximum length of string.
      type(xdrfile), intent(in)         :: xfp          ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_read_string

    ! Write string type (array of characters) variable
    ! * Returns number of characters written, including end-of-string.
    integer(C_INT) function xdrfile_write_string( ptr, xfp ) bind( C, NAME="xdrfile_write_string" )
      import
      character(C_CHAR), intent(in) :: ptr(*)  ! Pointer to memory where data should be written.
      type(xdrfile), intent(in)     :: xfp    ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_write_string

    ! Returns absolute current position in XDRFILE.
    integer(C_INT64_T) function xdr_tell( xd ) bind( C, NAME="xdr_tell" )
      import
      type(xdrfile), intent(in) :: xd ! Pointer to an abstract XDRFILE datatype.
    end function xdr_tell

    ! Moves XDRFILE position to the specified offset from whence.
    ! NOTE: This uses C based indexing (0, 1, 2, ...)
    integer(C_INT) function xdr_seek( xd, offset, whence ) bind( C, NAME="xdr_seek" )
      import
      type(xdrfile), intent(in)             :: xd     ! Pointer to an abstract XDRFILE datatype.
      integer(C_INT64_T), value, intent(in) :: offset ! Position offset
      integer(C_INT), value, intent(in)     :: whence ! Origin of offset: 0 = SEEL_BEG, 1 = SEEK_CUR, 2 = SEEK_END
    end function xdr_seek

    ! Open a portable binary file, just like fopen()
    ! * Returns pointer to abstract xdr file datatype, or NULL if an error occurs.
    type(C_PTR) function xdrfile_open( file, mode ) bind( C, NAME="xdrfile_open" )
      import
      character(C_CHAR), intent(in) :: file(*) ! Full or relative path (including name) of the file
      character(C_CHAR), intent(in) :: mode(*) ! "r" for reading, "w" for writing, "a" for append.
    end function xdrfile_open

    ! Close a previously opened portable binary file, just like fclose()
    ! * Returns 0 on success, non-zero on error.
    integer(C_INT) function xdrfile_close( xd ) bind( C, NAME="xdrfile_close" )
      import
      type(xdrfile), intent(in) :: xd ! Pointer to an abstract XDRFILE datatype.
    end function xdrfile_close
  end interface

contains

! xdrfile code to error message
function exdr_message( errnum )
  implicit none
  character(:), allocatable :: exdr_message
  integer, intent(in)       :: errnum

  select case( errnum )
  case( exdrOK )
    exdr_message = "OK"
  case( exdrHEADER )
    exdr_message = "Header"
  case( exdrSTRING )
    exdr_message = "String"
  case( exdrDOUBLE )
    exdr_message = "Double"
  case( exdrINT )
    exdr_message = "Integer"
  case( exdrFLOAT )
    exdr_message = "Float"
  case( exdrUINT )
    exdr_message = "Unsigned integer"
  case( exdr3DX )
    exdr_message = "Compressed 3D coordinate"
  case( exdrCLOSE )
    exdr_message = "Closing file"
  case( exdrMAGIC )
    exdr_message = "Magic number"
  case( exdrNOMEM )
    exdr_message = "Not enough memory"
  case( exdrENDOFFILE )
    exdr_message = "End of file"
  case( exdrFILENOTFOUND )
    exdr_message = "File not found"
  case default
    exdr_message = "???"
  end select

  return
end function exdr_message

end module xdrfor
