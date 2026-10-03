! This file is part of xslib
! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2026 Jure Cerar
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

module xslib_fileio
    use iso_fortran_env, only: IOSTAT_END, INT32, INT64, REAL32, REAL64
    implicit none
    private
    public :: fromtxt

    integer, parameter :: BUFLEN = 4096
    !! Max length of buffer
    
    character(*), parameter :: DEFAULT_DELIM = ","
    !! Default delimiters

    character(*), parameter :: DEFAULT_COMMENTS = "#"
    !! Default comment symbols

    interface fromtxt
        !! Load data from a text file.
        !!
        !! Example:
        !! ```Fortran
        !! print *, fromtxt("file.csv")
        !! >>> [[0, 1, 2, 3, 4], [5, 6, 7, 8, 9]]
        !! print *, fromtxt("heterogenus.csv", skiprows=1, usecol=[1,2,3])
        !! >>> [[0, 1, 2, 3, 4], [5, 6, 7, 8, 9], [0, 0, 0, 0, 0]]
        !! array = fromtxt("file.csv", mold=array, STAT=status, ERRMSG=message)
        !! if (status /= 0) error stop trim(message)
        !! ```
        module procedure :: fromtxt_i32, fromtxt_r32, fromtxt_c
    end interface fromtxt
    
contains


function fromtxt_i32 (file, delimiters, comments, skiprows, usecols, mold, stat, errmsg) result (out)
    implicit none
    integer(INT32), allocatable :: out(:,:)
    !! Output array with data read from file.
    character(*), intent(in) :: file
    !! Path to file.
    character(*), intent(in), optional :: delimiters
    !! The character (or multiple) to separate the values. The 
    !! default is `,`.
    character(*), intent(in), optional :: comments
    !! The character (or multiple) used to indicate the start of
    !! a comment. Default is `#`.
    integer, intent(in), optional :: skiprows
    !! Skip the first lines (NOT including comments). Default is zero.
    integer, intent(in), optional :: usecols(:)
    !! Which columns to read, with 1 being the first. For example, `usecols=[1,3,4]`
    !! will extract the 1st, 3rd and 4th columns. The default, results in all columns being read.
    integer(INT32), intent(in) :: mold
    !! Specify type (mold) of output array. This is very important for character arrays, 
    !! as it specifies the character length and can lead not being read correctly.   
    integer, intent(out), optional :: stat
    !! Error status code. Returns zero if no error.
    character(*), intent(out), optional :: errmsg
    !! Error message.
    ! ---------------------------
    integer, allocatable :: usecols_(:)
    character(:), allocatable :: delimiters_, comments_
    character(BUFLEN), allocatable :: lines(:)
    character(BUFLEN) :: buffer
    character(128) :: message
    integer :: num_rows, num_cols, actual_cols, skiprows_
    integer :: status, unit, i, j, jj
    
    ! Value defaults
    delimiters_ = DEFAULT_DELIM
    if (present(delimiters)) delimiters_ = delimiters
    comments_ = DEFAULT_COMMENTS
    if (present(comments)) comments_ = comments
    skiprows_ = 0
    if (present(skiprows)) skiprows_ = skiprows
    
    catch: block
        open (FILE=file, NEWUNIT=unit, STATUS="old", ACTION="read", IOSTAT=status, IOMSG=message)
        if (status /= 0) exit catch
        
        lines = readlines(unit, comments_, skiprows_, STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
       
        close (UNIT=unit, IOSTAT=status, IOMSG=message)
        if (status /= 0) exit catch
        
        ! Check number of rows and columns
        ! NOTE: Use first line in storage buffer to count number of columns
        num_rows = size(lines)
        num_cols = cnttok(lines(1), delimiters_)

        ! How many of those columns will we be storing?
        if (present(usecols)) then
            usecols_ = usecols
        else 
            usecols_ = [(i, i = 1, num_cols)]
        end if 
        actual_cols = count(0 < usecols_ .and. usecols_ <= num_cols)
        
        ! Allocate data
        if (allocated(out)) deallocate (out, STAT=status, ERRMSG=message)
        allocate (out(actual_cols, num_rows), mold=mold, STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        ! Parse lines on delimiter
        do i = 1, num_rows
            jj = 0
            do j = 1, num_cols
                if (findloc(usecols_, j, DIM=1) == 0) cycle
                jj = jj + 1 
                buffer = strtok(lines(i), j, delimiters_)
                read (buffer, *, IOSTAT=status, IOMSG=message) out(jj, i)
                if (status == IOSTAT_END) then
                    message = "Unexpected number of columns" 
                    status = 1
                    exit catch
                end if
                if (status /= 0) exit catch
            end do
        end do

    end block catch

    ! Error handling
    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end function fromtxt_i32


function fromtxt_r32 (file, delimiters, comments, skiprows, usecols, mold, stat, errmsg) result (out)
    implicit none
    real(REAL32), allocatable :: out(:,:)
    character(*), intent(in) :: file
    character(*), intent(in), optional :: delimiters
    character(*), intent(in), optional :: comments
    integer, intent(in), optional :: skiprows
    integer, intent(in), optional :: usecols(:)
    real(REAL32), intent(in), optional :: mold
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    ! ---------------------------
    integer, allocatable :: usecols_(:)
    character(:), allocatable :: delimiters_, comments_
    character(BUFLEN), allocatable :: lines(:)
    character(BUFLEN) :: buffer
    character(128) :: message
    integer :: num_rows, num_cols, actual_cols, skiprows_
    integer :: status, unit, i, j, jj
    
    delimiters_ = DEFAULT_DELIM
    if (present(delimiters)) delimiters_ = delimiters
    comments_ = DEFAULT_COMMENTS
    if (present(comments)) comments_ = comments
    skiprows_ = 0
    if (present(skiprows)) skiprows_ = skiprows
    
    catch: block
        open (FILE=file, NEWUNIT=unit, STATUS="old", ACTION="read", IOSTAT=status, IOMSG=message)
        if (status /= 0) exit catch
        
        lines = readlines(unit, comments_, skiprows_, STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
       
        close (UNIT=unit, IOSTAT=status, IOMSG=message)
        if (status /= 0) exit catch
        
        num_rows = size(lines)
        num_cols = cnttok(lines(1), delimiters_)

        if (present(usecols)) then
            usecols_ = usecols
        else 
            usecols_ = [(i, i = 1, num_cols)]
        end if 
        actual_cols = count(0 < usecols_ .and. usecols_ <= num_cols)
        
        if (allocated(out)) deallocate (out, STAT=status, ERRMSG=message)
        allocate (out(actual_cols, num_rows), mold=mold, STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        do i = 1, num_rows
            jj = 0
            do j = 1, num_cols
                if (findloc(usecols_, j, DIM=1) == 0) cycle
                jj = jj + 1 
                buffer = strtok(lines(i), j, delimiters_)
                read (buffer, *, IOSTAT=status, IOMSG=message) out(jj, i)
                if (status == IOSTAT_END) then
                    message = "Unexpected number of columns" 
                    status = 1
                    exit catch
                end if
                if (status /= 0) exit catch
            end do
        end do

    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end function fromtxt_r32


function fromtxt_c (file, delimiters, comments, skiprows, usecols, mold, stat, errmsg) result (out)
    implicit none
    character(:), allocatable :: out(:,:)
    character(*), intent(in) :: file
    character(*), intent(in), optional :: delimiters
    character(*), intent(in), optional :: comments
    integer, intent(in), optional :: skiprows
    integer, intent(in), optional :: usecols(:)
    character(*), intent(in) :: mold
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    ! ---------------------------
    integer, allocatable :: usecols_(:)
    character(:), allocatable :: delimiters_, comments_
    character(BUFLEN), allocatable :: lines(:)
    character(128) :: message
    integer :: num_rows, num_cols, actual_cols, skiprows_
    integer :: status, unit, i, j, jj
    
    delimiters_ = DEFAULT_DELIM
    if (present(delimiters)) delimiters_ = delimiters
    comments_ = DEFAULT_COMMENTS
    if (present(comments)) comments_ = comments
    skiprows_ = 0
    if (present(skiprows)) skiprows_ = skiprows
    
    catch: block
        open (FILE=file, NEWUNIT=unit, STATUS="old", ACTION="read", IOSTAT=status, IOMSG=message)
        if (status /= 0) exit catch
        
        lines = readlines(unit, comments_, skiprows_, STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
       
        close (UNIT=unit, IOSTAT=status, IOMSG=message)
        if (status /= 0) exit catch
        
        num_rows = size(lines)
        num_cols = cnttok(lines(1), delimiters_)

        if (present(usecols)) then
            usecols_ = usecols
        else 
            usecols_ = [(i, i = 1, num_cols)]
        end if 
        actual_cols = count(0 < usecols_ .and. usecols_ <= num_cols)
        
        if (allocated(out)) deallocate (out, STAT=status, ERRMSG=message)
        allocate (out(actual_cols, num_rows), mold=mold, STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        do i = 1, num_rows
            jj = 0
            do j = 1, num_cols
                if (findloc(usecols_, j, DIM=1) == 0) cycle
                jj = jj + 1
                out(jj, i) = trim(strtok(lines(i), j, delimiters_))
            end do
        end do

    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end function fromtxt_c


function readlines(unit, comments, skiprows, stat, errmsg) result (lines)
    implicit none
    character(BUFLEN), allocatable :: lines(:)
    integer, intent(in) :: unit
    character(*), intent(in) :: comments
    integer, intent(in) :: skiprows
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg
    character(BUFLEN), allocatable :: temp(:)
    character(BUFLEN) :: buffer
    logical :: opened
    character(128) :: action, message
    integer :: status, num_rows, rows_skipped, loc, i

    catch: block
        ! Check if file unit is opened for reading.
        inquire (UNIT=unit, OPENED=opened, ACTION=action, IOSTAT=status, IOMSG=message)
        if (.not. opened .or. index(action, "READ") == 0) then
            message = "File unit not opened for reading"
            status = 1
        end if
        if (status /= 0) exit catch
        
        ! Start at buffer of size 128, we will expand it as we go along
        if (allocated(lines)) deallocate (lines, STAT=status, ERRMSG=message)
        allocate (lines(128), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch

        num_rows = 0
        rows_skipped = 0

        while: do
            read (unit, "(a)", IOSTAT=status, IOMSG=message) buffer
            if (status == IOSTAT_END) exit while
            if (status /= 0) exit catch
            
            ! Remove comments
            do i = 1, len(comments)
                loc = index(trim(buffer), comments(i:i))
                if (loc == 0) then
                    buffer = trim(buffer)
                else if (loc == 1) then
                    buffer = ""
                else
                    buffer = buffer(:loc-1)
                end if
            end do

            ! Continue if empty
            if (len_trim(buffer) == 0) cycle

            ! Skip first specified number of rows
            if (rows_skipped < skiprows) then
                rows_skipped = rows_skipped + 1
                cycle
            end if
            
            ! Expand storage buffer if needed
            if (num_rows > size(lines)) then
                allocate (temp(2 * size(lines)), STAT=status, ERRMSG=message)
                if (status /= 0) exit catch
                temp(1:num_rows) = lines(1:num_rows)
                call move_alloc (temp, lines)
            end if

            num_rows = num_rows + 1
            lines(num_rows) = trim(buffer)

        end do while

        ! How many did we read
        if (num_rows == 0) then
            message = "No valid line found"
            status = 1
            exit catch
        end if

        ! Correct the size (for simple size lookup) 
        allocate (temp(num_rows), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        temp(1:num_rows) = lines(1:num_rows)
        call move_alloc (temp, lines)

    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end function readlines


function cnttok(str, delimiters) result (cnt)
    !! Count number of tokens in a string separated by a delimiter(s).
    implicit none
    integer :: cnt
    character(*), intent(in) :: str
    character(*), intent(in) :: delimiters
    integer :: i

    cnt = 1
    do i = 1, len_trim(str)
        if (index(delimiters, str(i:i)) /= 0) cnt = cnt + 1
    end do

end function cnttok


function strtok(str, n, delimiters) result (token)
    !! Get i-th token in a string separated by delimiter(s).
    implicit none
    character(*), intent(in) :: str
    character(*), intent(in) :: delimiters
    integer, intent(in) :: n
    character(:), allocatable :: token
    integer :: i, start, finish, token_num
    logical :: is_delimiter

    token = ""

    if (n <= 0) return

    token_num = 0
    start = 1

    do i = 1, len_trim(str) + 1
        if (i > len_trim(str)) then
            is_delimiter = .True.
        else
            is_delimiter = index(delimiters, str(i:i)) > 0
        end if
        finish = i - 1
        if (is_delimiter) then
            if (finish >= start) then
                token_num = token_num + 1
                if (token_num == n) then
                    token = str(start:finish)
                    return
                end if
            end if
            start = i + 1
        end if
    end do

end function strtok

end module xslib_fileio