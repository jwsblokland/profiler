!> \cond _INTERNAL_
!> \brief Module containing functions and subroutines for formatting date and time information.
module profiler_datetime
  use, intrinsic :: iso_fortran_env,  only: int32
  implicit none

  private
  public  :: prof_datetime

  !> \ingroup versioni
  !> \{
  integer(int32), parameter :: PROFILER_MAJOR = 0 !@PROJECT_VERSION_MAJOR@
  integer(int32), parameter :: PROFILER_MINOR = 1 !@PROJECT_VERSION_MINOR@
  integer(int32), parameter :: PROFILER_PATCH = 0 !@PROJECT_VERSION_PATCH@
  !> \}
  
contains
  !> \ingroup versioni
  !> \{
  !> \brief Constructs time, date, and zone string for the current time.
  subroutine prof_datetime(time, date, zone)
    character(len=8)  :: time  !< Time string formatted as hh:mm:ss.
    character(len=17) :: date  !< Date string formatted as d mmmm yyyy.
    character(len=12) :: zone  !< Zone string formatted as (+h:mm UTC).

    ! Locals
    integer(int32),  dimension(8) :: values
    character(len=1)              :: sign_str

    call date_and_time(values=values)
    sign_str = "+"
    if (values(4) < 0)  sign_str = "-"
    
    write(time, 100) values(5), values(6), values(7)
    write(date, 200) values(3), trim(month_name(values(2))), values(1)
    write(zone, 300) sign_str, values(4) / 60, mod(values(4),60)
    
100 format(i0.2, ":", i0.2 , ":", i0.2)
200 format(i0, " ", a, " ", i4)
300 format("(", a1, i0, ":", i0.2, " UTC)")
  end subroutine prof_datetime
  !> \}

  !> \ingroup layouti
  !> \{
  !> \brief Returns the name of the month for a given month number.
  function month_name(month_number) result(name)
    use, intrinsic :: iso_fortran_env,  only: int32

    integer(int32),   intent(in) :: month_number  !< Month number.
    character(len=9)             :: name          !< Name of the month.

    ! Parameters
    character(len=9), dimension(12) :: names = [ 'January  ', 'February ', 'March    ', 'April    ', 'May      ', 'June     ',   &
                                                 'July     ', 'Augustus ', 'September', 'October  ', 'November ', 'December ' ]

    name = names(month_number)
  end function month_name
  !> \}
end module profiler_datetime
!> \endcond
