!> \brief Module which contains the derived types needed by the profiler library.
module profiler_types
  use, intrinsic :: iso_fortran_env,  only: int32, int64
  implicit none

  private
  
  !> \ingroup profiler
  !> \{
  integer(int32), parameter, public :: STR_LEN = 40  !< Length of a string.
  !> \}

  !> \cond _INTERNAL_
  !> \ingroup profileri
  !> \brief Derived type representing a watch.
  type, public :: watch_t
     type(watch_t),          pointer :: parent     => null()  !< Parent watch.
     type(watch_t),          pointer :: sibling    => null()  !< Sibling watches
     type(watch_t),          pointer :: children   => null()  !< Child watches.
     character(len=STR_LEN)          :: name                  !< Name of the watch.
     integer(int32)                  :: generation =  0       !< Family generation.
     integer(int64)                  :: nused      =  0       !< Number of times this watch has been used.
     integer(int64)                  :: etime      =  0       !< Sum of the elasped counting time.
     integer(int64)                  :: start      =  0       !< Starting counting time.
     character(len=STR_LEN)          :: unit       = ''       !< Base name of the unit.
     integer(int64)                  :: nunits     =  0       !< Sum of the number of units that have been used.
  end type watch_t
  !> \endcond

  !> \cond _INTERNAL_
  !> \ingroup layouti
  !> \{
  !> \brief Layout properties of the report.
  type, public :: props_t
     integer(int32) :: count_maxlen  !< Maximum string length needed to represent the maximum number how many time a watch has been used.
     integer(int32) :: name_maxlen   !< Maximum string length needed to represent the name of the watch (including the generation offset).
     integer(int32) :: frac_maxlen   !< Maximum string length needed to represent the percentage (including the generation offset).
     integer(int32) :: etime_maxlen  !< Maximum string length needed to represent the elapsed time (including the data rate).
  end type props_t
  !> \}
  !> \endcond

  !> \ingroup profiler
  !> \{
  !> \brief Derived type representing the profiler.
  type, public :: prof_t
     type(watch_t)          :: mwatch            !< Master watch.
     type(watch_t), pointer :: cwatch => null()  !< Current watch.
  end type prof_t
  !> \}
end module profiler_types
