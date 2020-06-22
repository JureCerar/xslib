module xslib_plot
  implicit none
  private
  public :: plot_t


  ! TODO:
  ! * - Define default precission
  ! * - Clean-up routines
  ! * - Better UI

  type plot_t
    private
    integer         :: unit      = -100
    character(128)  :: file      = "out.gp"
    integer         :: cnt       = 1 ! Number of data sets
    logical, public :: animation = .false.
    real, public    :: delay     = 0.100 ! [sec]
    character(:), allocatable :: buffer
  contains
    procedure, private :: check_file ! Check if file is initialized
    procedure, private :: finalize   ! Close file and plot data
    procedure, public  :: init    => initialize   ! Start new file
    procedure, public  :: title   => set_title    ! Set title
    procedure, public  :: style   => set_style    ! Set custom style
    procedure, public  :: palette => set_palette  ! Set color pallete
    procedure, public  :: grid    => set_grid     ! Set plot grid
    procedure, public  :: options => set_options  ! Set custom option
    procedure, private :: set_label ! Generic set axis label
    procedure, public  :: xlabel  => set_xlabel
    procedure, public  :: ylabel  => set_ylabel
    procedure, public  :: zlabel  => set_zlabel
    procedure, private :: set_range ! Generic set axis range
    procedure, public  :: xrange  => set_xrange
    procedure, public  :: yrange  => set_yrange
    procedure, public  :: zrange  => set_zrange
    procedure, private :: add_array    ! Add array data to plot
    procedure, private :: add_function ! Add function data to plot
    procedure, private :: add_matrix   ! Add matrix data to plot
    generic, public    :: add => add_array, add_function, add_matrix ! Add data to plot
    procedure, public  :: plot   ! Plot data
    procedure, public  :: splot  ! Surface plot
    procedure, public  :: cplot  ! Contour plot
    procedure, public  :: animation_start ! Start animation
    procedure, public  :: animation_end   ! End animation & plot
    procedure, nopass  :: linspace  ! Construct linear space in range
    procedure, nopass  :: logspace  ! Construct log space in range
    procedure, nopass  :: gridspace ! Construct grid space in range
  end type plot_t

contains

! ---------------------------------------------------

! Create new plot instance if it does not exist
subroutine check_file( this )
  implicit none
  class(plot_t) :: this
  integer       :: stat
  logical       :: opened
  character(128)  :: msg

  inquire ( UNIT=this%unit, OPENED=opened )
  if ( .not. opened ) then
    open ( NEWUNIT=this%unit, FILE=this%file, STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) call error( "Cannot open file: '"//trim(this%file)//"'" )

    ! Set up default terminal and size
    write (this%unit,"(a)") "set terminal wxt size 800,600"

    ! Add line for generating picture
    write (this%unit,"(a)") "# Uncomment to generate PNG picture"
    write (this%unit,"(a)") "# set terminal png size 800,600"
    write (this%unit,"(a)") "# set output '"// basename(this%file)//".png'"

  end if

  return
end subroutine check_file

! Close file and system call gnuplot
subroutine finalize( this )
  implicit none
  class(plot_t)   :: this
  logical         :: opened
  integer         :: stat
  character(128)  :: msg

  ! Close unit
  inquire ( UNIT=this%unit, OPENED=opened )
  if ( opened ) close ( this%unit )

  ! Call gnuplot
  call execute_command_line( "gnuplot -p "//this%file, WAIT=.true., CMDSTAT=stat, CMDMSG=msg )
  if ( stat /= 0 ) call error( msg )

  ! Clear buffer and reset counter
  this%cnt    = 1
  this%buffer = ""

  return
end subroutine finalize

! ---------------------------------------------------

! Initialize new plot instance and load default values
subroutine initialize( this, file )
  implicit none
  class(plot_t)                       :: this
  character(*), intent(in), optional  :: file
  logical                             :: opened

  ! Close current unit if opened
  inquire ( UNIT=this%unit, OPENED=opened )
  if ( opened ) close( this%unit )

  ! Set initaal values
  this%file      = smerge(trim(file),"out.gp",present(file))
  this%animation = .false.
  this%delay     = 0.1

  ! Create and open new file
  call this%check_file()

  return
end subroutine initialize

! ---------------------------------------------------

! Set plot title
subroutine set_title( this, title )
  implicit none
  class(plot_t)             :: this
  character(*), intent(in)  :: title

  ! Check output unit
  call this%check_file()

  ! Set plot title
  write (this%unit,"(a)") "set title '" // trim(title) // "' enhanced"

  return
end subroutine set_title

! Set custom style for index
! * lt - linetype
! * lc - linecolor
! * lw - linewidth
! * pt - pointtype
! * ps - pointssize
subroutine set_style( this, index, lt, lc, lw, pt, ps )
  implicit none
  class(plot_t)                       :: this
  integer, intent(in)                 :: index
  integer, intent(in), optional       :: lt, pt
  real, intent(in), optional          :: lw, ps
  character(*), intent(in), optional  :: lc
  character(:), allocatable           :: buffer

  ! Check output file
  call this%check_file()

  ! Construct style for index
  buffer = "set style line "//str(index)

  ! Line styles
  if ( present(lt) ) buffer = buffer//" lt "//str(lt)
  if ( present(lc) ) buffer = buffer//" lc '"//trim(lc)//"'"
  if ( present(lw) ) buffer = buffer//" lw "//str(lw,"(f8.1)")

  ! Point styles
  if ( present(pt) ) buffer = buffer//" pt "//str(pt)
  if ( present(ps) ) buffer = buffer//" ps "//str(ps,"(f8.1)")

  ! Write buffer to file
  write (this%unit,"(a)") buffer

  return
end subroutine set_style

! Set color pallete presets.
! * Options: set1, set2, set3, palette1, palette2, paired, dark2, accent and temp
subroutine set_palette( this, name, pm3d )
  implicit none
  class(plot_t)                 :: this
  character(*), intent(in)      :: name
  logical, intent(in), optional :: pm3d ! Enable 3D palette mode
  character(7)                  :: palette(12)
  integer                       :: i, np

  ! For more palette options see:
  ! https://github.com/aschn/gnuplot-colorbrewer

  ! Select palette
  select case ( toLower(trim(adjustl(name))) )
  case ( "set1" )
    np = 8
    palette(1:np) = [&
    & "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", &
    & "#FF7F00", "#FFFF33", "#A65628", "#F781BF" ]

  case ( "set2" )
    np = 8
    palette(1:np) = [&
    & "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", &
    & "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3" ]

  case ( "set3" )
    np = 8
    palette(1:np) = [&
    & "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", &
    & "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5" ]

  case ( "palette1" )
    np = 8
    palette(1:np) = [&
    & "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", &
    & "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC" ]

  case ( "palette2" )
    np = 8
    palette(1:np) = [&
    & "#B3E2CD", "#FDCDAC", "#CDB5E8", "#F4CAE4", &
    & "#D6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC" ]

  case ( "paired" )
    np = 8
    palette(1:np) = [&
    & "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", &
    & "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00" ]

  case ( "dark2" )
    np = 8
    palette(1:np) = [&
    & "#1B9E77", "#D95F02", "#7570B3", "#E7298A", &
    & "#66A61E", "#E6AB02", "#A6761D", "#666666" ]

  case ( "accent" )
    np = 8
    palette(1:np) = [&
    & "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", &
    & "#386CB0", "#F0027F", "#BF5B17", "#666666" ]

  case ( "temp" )
    np = 8
    palette(1:np) = [&
    & "#D73027", "#F46D43", "#FDAE61", "#FEE090", &
    & "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4" ]

  case ( "jet" )
    np = 9
    palette(1:np) = [&
    & "#000090", "#000fff", "#0090ff", "#0fffee", &
    & "#90ff70", "#ffee00", "#ff7000", "#ee0000", &
    & "#7f0000" ]

  case default
    call error( "Invalid palette name" )
    write (*,*) "gnuplot default palette will be used!"
    return

  end select

  ! Check ouput file
  call this%check_file()

  ! Set line styles
  write (this%unit,"(a)") "# Palette styles"
  do i = 1, np
    write (this%unit,"(a)") "set style line " // str(i) // " lc rgb '" // palette(i) // "'"
  end do

  ! Set pallete definition
  write (this%unit,"(a)") "# Pallete defion"
  write (this%unit,"(a)") "set palette defined ( \"
  do i = 1, np
    write (this%unit,"(a)") str(i) // " '" // palette(i) // "'" // smerge( ")", ", \", i==np )
  end do

  ! Enable surface color palette
  if ( merge(pm3d,.false.,present(pm3d)) ) then
    write (this%unit,"(a)") "# 3D color palette"
    write (this%unit,"(a)") "set pm3d"
  end if

  return
end subroutine set_palette

! Set plot grid
subroutine set_grid( this, options )
  implicit none
  class(plot_t)                       :: this
  character(*), intent(in), optional  :: options

  ! Check output file
  call this%check_file()

  ! set generic gray grid
  write (this%unit,"(a)") "# Display plot grid"
  if ( present(options) ) then
    write (this%unit,"(a)") "set grid "//trim(options)
  else
    write (this%unit,"(a)") "set grid linestyle 1 linecolor 'gray' linewidth 1"
  end if

  return
end subroutine set_grid

! Set used defined options
subroutine set_options( this, options )
  implicit none
  class(plot_t)             :: this
  character(*), intent(in)  :: options

  ! Check output file
  call this%check_file()

  ! Write options
  write (this%unit,"(a)") trim(options)

  return
end subroutine set_options

! ---------------------------------------------------

! Generic set axis label
subroutine set_label( this, label, title )
  class(plot_t)                       :: this
  character(*), intent(in)            :: label, title

  ! Check input file
  call this%check_file()

  ! Write generic label
  write (this%unit,"(a)") "set "//trim(label)//" '"//trim(title)//"' enhanced"

  return
end subroutine set_label

! Set x-axis label
subroutine set_xlabel( this, title )
  implicit none
  class(plot_t)            :: this
  character(*), intent(in) :: title
  call this%set_label( "xlabel", title )
  return
end subroutine set_xlabel

! Set y-axis label
subroutine set_ylabel( this, title )
  implicit none
  class(plot_t)            :: this
  character(*), intent(in) :: title
  call this%set_label( "ylabel", title )
  return
end subroutine set_ylabel

! Set z-axis label
subroutine set_zlabel( this, title )
  implicit none
  class(plot_t)            :: this
  character(*), intent(in) :: title
  call this%set_label( "zlabel", title )
  return
end subroutine set_zlabel

! ---------------------------------------------------

! Generic set axis range
subroutine set_range( this, label, min, max )
  implicit none
  class(plot_t)               :: this
  character(*), intent(in)    :: label
  real, intent(in), optional  :: min, max
  character(:), allocatable   :: buffer

  ! Check input file
  call this%check_file()

  ! Set generic min and max range
  buffer = "set "// trim(label) // " ["
  if ( present(min) ) buffer = buffer // str(min)
  buffer = buffer // ":"
  if ( present(max) ) buffer = buffer // str(max)
  buffer = buffer // "]"

  ! Write to file
  write (this%unit,"(a)") trim(buffer)

  return
end subroutine set_range

! Set x-axis range
subroutine set_xrange( this, min, max )
  implicit none
  class(plot_t)               :: this
  real, intent(in), optional  :: min, max
  call this%set_range( "xrange", min, max )
  return
end subroutine set_xrange

! Set y-axis range
subroutine set_yrange( this, min, max )
  implicit none
  class(plot_t)               :: this
  real, intent(in), optional  :: min, max
  call this%set_range( "yrange", min, max )
  return
end subroutine set_yrange

! Set z-axis range
subroutine set_zrange( this, min, max )
  implicit none
  class(plot_t)               :: this
  real, intent(in), optional  :: min, max
  call this%set_range( "zrange", min, max )
  return
end subroutine set_zrange

! ---------------------------------------------------

! Add array data
subroutine add_array( this, x, y, options )
  implicit none
  class(plot_t)                       :: this
  real, intent(in)                    :: x(:), y(:)
  character(*), intent(in), optional  :: options
  character(16)                       :: keyword
  integer                             :: i

  ! Check output file
  call this%check_file()

  ! Check x and y size
  if ( size(x) /= size(y) ) then
    call error( "Different sized arrays!" )
    return
  end if

  ! Generate data placeholder name
  write (keyword,"('$DATA',i0.3)") this%cnt

  ! Write header
  write (this%unit,"(a)") trim(keyword)//" << END"

  ! Write data
  do i = 1, size(x)
    write (this%unit,*) x(i), y(i)
  end do

  ! End data
  write (this%unit,"(a)") "END"

  ! -------------------------------------------
  ! Add new enty to plot buffer

  ! Go to new line if not first entry
  if ( this%cnt /= 1 ) this%buffer = this%buffer//", \"//new_line("a")

  ! Plot <data> using 1:2
  this%buffer = this%buffer//trim(keyword)//" u 1:2"

  ! Append options
  if ( present(options) ) this%buffer = this%buffer//" "//trim(options)

  ! Increment data counter
  this%cnt = this%cnt+1

  return
end subroutine add_array

! Add function data
subroutine add_function( this, x, func, options )
  implicit none
  class(plot_t)                       :: this
  real, intent(in)                    :: x(:)
  character(*), intent(in), optional  :: options
  character(16)                       :: keyword
  integer                             :: i
  interface
    real function func( x )
      real, intent(in) :: x
    end function func
  end interface

  ! Check output file
  call this%check_file()

  ! Generate data placeholder name
  write (keyword,"('$DATA',i0.3)") this%cnt

  ! Write header
  write (this%unit,"(a)") trim(keyword)//" << END"

  ! Write data
  do i = 1, size(x)
      write (this%unit,*) x(i), func(x(i))
  end do ! for i

  ! End data
  write (this%unit,"(a)") "END"

  ! -------------------------------------------
  ! Add new enty to plot buffer

  ! Go to new line if not first entry
  if ( this%cnt /= 1 ) this%buffer = this%buffer//", \"//new_line("a")

  ! Plot <data> using 1:2
  this%buffer = this%buffer//trim(keyword)//" u 1:2"

  ! Append options
  if ( present(options) ) this%buffer = this%buffer//" "//trim(options)

  ! Increment data counter
  this%cnt = this%cnt+1

  return
end subroutine add_function

! Add matrix data
subroutine add_matrix( this, x, y, z, options )
  implicit none
  class(plot_t)                       :: this
  real, intent(in)                    :: x(:,:), y(:,:), z(:,:)
  character(*), intent(in), optional  :: options
  character(16)                       :: keyword
  integer                             :: i, j

  ! Check output file
  call this%check_file()

  ! Check x, y and z size
  if ( size(x,DIM=1) /= size(y,DIM=1) .and. size(x,DIM=1) /= size(z,DIM=1) ) then
    call error( "Different sized matices!" )
    return
  end if

  if ( size(x,DIM=2) /= size(y,DIM=2) .and. size(x,DIM=2) /= size(z,DIM=2) ) then
    call error( "Different sized matices!" )
    return
  end if

  ! Generate data placeholder name
  write (keyword,"('$DATA',i0.3)") this%cnt

  ! Write header
  write (this%unit,"(a)") trim(keyword)//" << END"

  ! Write data
  do i = 1, size(x,DIM=2)
    do j = 1, size(x,DIM=1)
      write (this%unit,*) x(j,i), y(j,i), z(j,i)

    end do ! for j
    write (this%unit,"(a)")  ! put an empty line

  end do ! for i

  ! End data
  write (this%unit,"(a)") "END"

  ! -------------------------------------------
  ! Add new enty to plot buffer

  ! Go to new line if not first entry
  if ( this%cnt /= 1 ) this%buffer = this%buffer//", \"//new_line("a")

  ! Plot <data> using 1:2
  this%buffer = this%buffer//trim(keyword)//" u 1:2:3"

  ! Append options
  if ( present(options) ) this%buffer = this%buffer//" "//trim(options)

  ! Increment data counter
  this%cnt = this%cnt+1

  return
end subroutine add_matrix

! ---------------------------------------------------

! Plot data
subroutine plot( this, x, y, options)
  implicit none
  class(plot_t) :: this
  real, intent(in), optional          :: x(:), y(:)
  character(*), intent(in), optional  :: options

  ! Check input file
  call this%check_file()

  ! Add data if present
  if ( present(x) .and. present(y) ) call this%add_array( x, y, options )

  ! Add plot directive
  write (this%unit,"(a)") "plot "//trim(this%buffer)

  ! Finalize plot
  if ( .not. this%animation ) then
    call this%finalize()
  else
    write (this%unit,"(a)") "pause "//str(this%delay,FMT="(f0.3)")
  end if

  ! Clear buffer and reset counter
  this%cnt    = 1
  this%buffer = ""

  return
end subroutine plot

! Plot surface data
subroutine splot( this, x, y, z, options  )
  implicit none
  class(plot_t)                       :: this
  real, intent(in), optional          :: x(:,:), y(:,:), z(:,:)
  character(*), intent(in), optional  :: options

  ! Check output file
  call this%check_file()

  ! Add data if present
  if ( present(x) .and. present(y) .and. present(z) ) then
    call this%add( x, y, z, options )
  end if

  ! Write splot directive
  write (this%unit,"(a)") "splot "//trim( this%buffer )

  ! Finalize plot
  if ( .not. this%animation ) then
    call this%finalize()
  else
    write (this%unit,"(a)") "pause "//str(this%delay,FMT="(f0.3)")
  end if

  ! Clear buffer and reset counter
  this%cnt    = 1
  this%buffer = ""

  return
end subroutine splot

! Plot contour data
subroutine cplot( this, x, y, z,  options )
  implicit none
  class(plot_t)                       :: this
  real, intent(in), optional          :: x(:,:), y(:,:), z(:,:)
  character(*), intent(in), optional  :: options

  ! Check output file
  call this%check_file()

  ! create the contour lines
  write (this%unit,"(a)") ! empty line
  write (this%unit,"(a)") "# Create the contour"
  write (this%unit,"(a)") "set contour base"
  write (this%unit,"(a)") "set cntrparam levels 16"
  write (this%unit,"(a)") "unset surface"
  write (this%unit,"(a)") "set view map"

  ! All other is same as splot
  call this%splot( x, y, z, options )

  return
end subroutine cplot

! ! ---------------------------------------------------

! Start animation
subroutine animation_start( this, delay )
  implicit none
  class(plot_t) :: this
  real, intent(in), optional :: delay

  ! Check output file
  call this%check_file()

  ! Set animation to TRUE
  this%animation = .true.

  ! Set delay time
  if ( present(delay) ) this%delay = delay

  ! Gif export comment
  write (this%unit,"(a)") "# Uncomment this to create GIF image"
  write (this%unit,"(a)") "# set terminal gif animate size 800,600"
  write (this%unit,"(a)") "# set output '"//basename(this%file)//".gif'"

  return
end subroutine animation_start

! Plot the animation
subroutine animation_end( this )
  implicit none
  class(plot_t) :: this

  ! Check output and finalize
  call this%check_file()
  call this%finalize()

  return
end subroutine animation_end

! ---------------------------------------------------

! Construct linear space in range
subroutine linspace( x, min, max )
  implicit none
  real, intent(inout) :: x(:)
  real, intent(in)    :: min, max
  integer             :: i, nx
  real                :: step
  nx = size(x)
  step = (max-min)/(nx-1)
  x(:) = [( min+(i-1)*step, i=1,nx )]
  return
end subroutine linspace

! Construct log space in range
subroutine logspace( x, min, max )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real, intent(inout) :: x(:)
  real, intent(in)    :: min, max
  integer             :: i, nx
  real(REAL64)        :: step
  if ( min < 0.0 .or. max < 0.0) call error( "On logscale range cannot be less than 0." )
  nx = size(x)
  step = (max/min)**(1.0d0/(nx-1))
  x(:) = real( [( min*step**(i-1), i=1,nx )] )
  return
end subroutine logspace

! Construct grid space in range
subroutine gridspace( x, xmin, xmax, y, ymin, ymax )
  implicit none
  real, intent(inout) :: x(:,:), y(:,:)
  real, intent(in)    :: xmin, xmax, ymin, ymax
  real                :: step
  integer             :: i, j, nx, ny
  nx = size(x,DIM=1)
  ny = size(x,DIM=2)
  if ( nx /= size(y,DIM=1) .or. ny /= size(y,DIM=2) ) call error( "Different sized matrices." )

  step = (xmax-xmin)/(nx-1)
  x(:,1) = [( xmin+(i-1)*step, i=1,nx )]
  x(:,:) = spread(x(:,1),DIM=2,NCOPIES=nx)

  step = (ymax-ymin)/(ny-1)
  y(1,:) = [( ymin+(i-1)*step, i=1,ny )]
  y(:,:) = spread(y(1,:),DIM=1,NCOPIES=ny)

  return
end subroutine gridspace

! ---------------------------------------------------
! Utility functions

! Write error message to errout (does not terminate)
subroutine error( message )
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  implicit none
  character(*), intent(in) :: message
  character, parameter     :: ESC = char(27)
  write (ERROR_UNIT,*) ESC//"[1;91m", "Error:", ESC//"[m", " ", trim(message)
  return
end subroutine error

! Returns basename of file.
character(:) function basename( file )
  implicit none
  allocatable               :: basename
  character(*), intent(in)  :: file
  integer                   :: i, j
  i = 1+index( file, "/", BACK=.true. )
  j = index( file, ".", BACK=.true. )
  j = merge( j-1, len_trim(file), j /= 0 )
  basename = file(i:j)
  return
end function basename

function smerge( tsource, fsource, mask )
  implicit none
  character(:), allocatable :: smerge
  character(*), intent(in)  :: tsource, fsource
  logical, intent(in)       :: mask
  if ( mask ) then
    smerge = tsource
  else
    smerge = fsource
  end if
  return
end function smerge

! Trnasforms scalar of any kind to character.
character(:) function str( x, fmt )
  use iso_fortran_env, only: REAL64, INT64
  implicit none
  allocatable             :: str
  class(*), intent(in)    :: x
  character(*), optional  :: fmt
  character(64)           :: tmp

  ! Act according to the type of variable
  select type ( x )
  type is ( integer )
    if ( present(fmt) ) then
      write (tmp,fmt) x
    else
      write (tmp,"(i0)") x
    end if
    str = trim(adjustl(tmp))

  type is ( integer(INT64) )
    if ( present(fmt) ) then
      write (tmp,fmt) x
    else
      write (tmp,"(i0)") x
    end if
    str = trim(adjustl(tmp))

  type is ( real )
    if ( present(fmt) ) then
      write (tmp,fmt) x
    else
      write (tmp,*) x
    end if
    str = trim(adjustl(tmp))
    if ( str(1:1) == "." ) then
      str = "0"//trim(str)
    else if ( str(1:2) == "-." ) then
      str = "-0"//trim(str(3:))
    else if ( str(1:2) == "+." ) then
      str = "+0"//trim(str(3:))
    end if

  type is ( real(REAL64) )
    if ( present(fmt) ) then
      write (tmp,fmt) x
    else
      write (tmp,*) x
    end if
    str = trim(adjustl(tmp))
    if ( str(1:1) == "." ) then
      str = "0"//trim(str)
    else if ( str(1:2) == "-." ) then
      str = "-0"//trim(str(3:))
    else if ( str(1:2) == "+." ) then
      str = "+0"//trim(str(3:))
    end if

  type is ( complex )
    if ( present(fmt) ) then
      write (tmp,fmt) x
    else
      write (tmp,*) x
    endif
    str = trim(adjustl(tmp))

  type is ( complex(REAL64) )
    if ( present(fmt) ) then
      write (tmp,fmt) x
    else
      write (tmp,*) x
    endif
    str = trim(adjustl(tmp))

  type is ( logical )
    if ( present(fmt) ) then
      write (tmp,fmt) x
    else
      write (tmp,*) merge( "TRUE ", "FALSE", x )
    end if
    str = trim(adjustl(tmp))

  type is ( character(*) )
    str = trim(adjustl(x))

  class default
    ! Can't have everything
    str = "???"

  end select

  return
end function str

! Return string in all lower case.
character(:) function toLower( string )
  implicit none
  allocatable               :: toLower
  character(*), intent(in)  :: string
  integer, parameter        :: offset=32 ! ASCII offset
  integer                   :: i
  toLower = trim(string)
  do i = 1, len_trim(toLower)
    select case ( toLower(i:i) )
    case ( "A":"Z" )
      toLower(i:i) = char(ichar(toLower(i:i))+offset)
    end select
  end do
  return
end function toLower

end module xslib_plot

! program name
!   use xslib_plot
!   implicit none
!   integer, parameter :: NP = 55
!   real               :: x(NP), y(NP)
!   real               :: a = 0.5, b = 2.0
!   integer            :: i
!   type(plot_t)       :: this
!
!   call this%linspace( x, -5., 5.)
!   y(:) = a*x+b
!
!   call this%palette( "temp" )
!   call this%options( "unset key" )
!
!   do i = 1, 5
!     call this%style( i, lw=3.0 )
!   end do
!
!   do i = 1, 5
!     call this%add( x, y*i, "w lines ls "//str(i) )
!   end do
!
!   call this%plot()
!
!   call exit( 0 )
! end program

! program name
!   use xslib_plot
!   implicit none
!   integer, parameter :: NP = 5
!   real               :: x(NP), y(NP)
!   integer            :: i
!   type(plot_t)       :: this
!
!   call this%linspace( x, -5., 5.)
!   y(:) = x
!
!   call this%animation_start( 1.0 )
!   call this%options( "unset key" )
!
!   do i = 1, 5
!     call this%add( x(:), y(:), "w lines" )
!     call this%plot( [x(i)], [y(i)], "w points")
!   end do
!
!   call this%animation_end()
!
!   call exit( 0 )
! end program

! program name
!   use xslib_plot
!   implicit none
!   real, parameter    :: pi = acos(-1.)
!   integer, parameter :: NP = 50
!   real               :: x(NP), y(NP)
!   integer            :: i
!   type(plot_t)       :: this
!   interface
!     real function func( x )
!       implicit none
!       real, intent(in) :: x
!     end function func
!   end interface
!   procedure(func), pointer :: f
!
!   f => sinx
!
!   call this%title( "sin(x)" )
!   call this%xlabel( "x" )
!   call this%ylabel( "y" )
!   call this%options( "unset key" )
!   call this%grid()
!   call this%xrange( 0., 2*pi )
!   call this%yrange( -1.1, 1.1 )
!   call this%linspace( x, 0., 2*pi)
!   this%animation = .true.
!   this%delay     = 0.02
!   do i = 1, NP
!     call this%add( x(:), f, "w l lw 3.0 lc 'red' " )
!     call this%plot( [x(i)], [sin(x(i))], "w p pt 7 ps 3 lc 'red' " )
!   end do
!   call this%animation_end()
!
!   call exit( 0 )
!   contains
!
!   real function sinx( x )
!     implicit none
!     real, intent(in) :: x
!     sinx = sin(x)
!     return
!   end function sinx
!
! end program

! program name
!   use xslib_plot
!   implicit none
!   integer, parameter :: NP = 55
!   real               :: x(NP,NP), y(NP,NP), z(NP,NP)
!   real               :: a = 0.5, b = 2.0
!   integer            :: i, j
!   type(plot_t)       :: this
!
!   call this%gridspace( x, -5., 5., y, -5., 5. )
!   do i = 1, NP
!     do j = 1, NP
!       z(j,i) = x(j,i)**2/a + y(j,i)**2/b
!     end do
!   end do
!
!   call this%palette( "temp", PM3D=.true. )
!
!   call this%splot( x, y, z, "w points ps 0.1 " )
!
!
!   call exit( 0 )
! end program
