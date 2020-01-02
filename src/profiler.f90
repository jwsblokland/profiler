!> \brief Profiler module
module profiler
  use, intrinsic :: iso_fortran_env,  only: int32, int64
  implicit none

  private
  public  :: prof_init, prof_tic, prof_toc, prof_report

  integer(int32), parameter :: STR_LEN = 40  !< Length of a string.
  
  !> \brief Derived type representing a watch.
  type :: watch_t
     type(watch_t),          pointer               :: parent     => null()  !< Parent watch.
     type(watch_t),          dimension(:), pointer :: children   => null()  !< Child watches.
     character(len=STR_LEN)                        :: name                  !< Name of the watch.
     integer(int32)                                :: generation =  0       !< Family generation.
     integer(int64)                                :: nused      =  0       !< Number of times this watch has been used.
     integer(int64)                                :: etime      =  0       !< Sum of the elasped counting time.
     integer(int64)                                :: start      =  0       !< Starting counting time.
     character(len=STR_LEN)                        :: unit       = ''       !< Base name of the unit.
     integer(int64)                                :: nunits     =  0       !< Sum of the number of units that have been used.
  end type watch_t

  !> \brief Layout properties of the report.
  type :: props_t
     integer(int32) :: count_maxlen  !< Maximum string length needed to represent the maximum number how many time a watch has been used.
     integer(int32) :: name_maxlen   !< Maximum string length needed to represent the name of the watch (including the generation offset).
     integer(int32) :: etime_maxlen  !< Maximum string length needed to represent the elapsed time (including the data rate).
  end type props_t
  
  type(watch_t), target,  save :: prof_mwatch            !< Master watch.
  type(watch_t), pointer, save :: prof_cwatch => null()  !< Current watch.
  
contains
  !> \brief Initializes the profiler.
  !!
  !! \note Currently, the maximum number of watches has been set to a hundred.
  subroutine prof_init(name)
    character(len=*), intent(in) :: name  !< Name of the master watch.

    ! Locals
    integer(int32) :: i, offset
    
    ! The name of the master watch.
    offset = 1
    do i=len_trim(name), 1, -1
       if (name(i:i) == '/') then
          offset = i + 1
          exit
       end if
    end do
    
    prof_cwatch            => prof_mwatch
    prof_cwatch%name       =  name(offset:)
    prof_cwatch%generation =  prof_cwatch%generation + 1
    prof_cwatch%nused      =  prof_cwatch%nused      + 1
    call system_clock(count=prof_cwatch%start)
  end subroutine prof_init
  
  !> \brief Profiler tic subroutine.
  subroutine prof_tic(name, nunits, unit)
    character(len=*),           intent(in) :: name    !< Name of the child watch.
    integer(int64),   optional, intent(in) :: nunits  !< Number of units that will be used.
    character(len=*), optional, intent(in) :: unit    !< Name of the unit.
    
    ! Locals
    integer(int32)                       :: ichild, nchildren
    type(watch_t), dimension(:), pointer :: children
    
    if (.not. associated(prof_cwatch%children)) then
       ! No children.
       allocate(prof_cwatch%children(1))
       prof_cwatch%children(1)%parent => prof_cwatch
       prof_cwatch                    => prof_cwatch%children(1)
       prof_cwatch%name               =  trim(name)
       prof_cwatch%unit               =  trim(unit)
       prof_cwatch%generation         =  prof_cwatch%parent%generation + 1
    else
       ! Has children.
       nchildren = size(prof_cwatch%children)
       do ichild = 1, nchildren
          if (trim(name) == trim(prof_cwatch%children(ichild)%name)) then
             ! Existing child watch found.
             prof_cwatch => prof_cwatch%children(ichild)
             exit
          end if
       end do

       if (ichild > nchildren) then
          ! New child watch.
          allocate(children(nchildren + 1))
          children(1:nchildren) = prof_cwatch%children
          nullify(prof_cwatch%children)
          prof_cwatch%children => children
          nullify(children)

          ! Set the parent of the new child watch.
          prof_cwatch%children(nchildren + 1)%parent => prof_cwatch

          prof_cwatch            => prof_cwatch%children(nchildren + 1)
          prof_cwatch%name       =  trim(name)
          prof_cwatch%unit       =  trim(unit)
          prof_cwatch%generation =  prof_cwatch%parent%generation + 1
       end if
    end if
    
    prof_cwatch%nused = prof_cwatch%nused + 1
    call system_clock(count=prof_cwatch%start)
    if (present(nunits))  prof_cwatch%nunits = prof_cwatch%nunits + nunits
  end subroutine prof_tic

  !> \brief Profiler toc subroutine
  subroutine prof_toc()
    ! Locals
    integer(int64) :: end

    call system_clock(count=end)
    prof_cwatch%etime =  prof_cwatch%etime + (end - prof_cwatch%start)
    prof_cwatch       => prof_cwatch%parent
  end subroutine prof_toc
  
  !> \brief Prints a report of the profiling results.
  subroutine prof_report()
    use, intrinsic :: iso_fortran_env,  only: int64, real64, error_unit
    
    ! Locals
    integer(int64)               :: end, count_rate, i
    character(len=60)            :: fmt, time_str, date_str
    character(len=255)           :: line
    integer(int32), dimension(8) :: values
    type(props_t)                :: props

    if (associated(prof_cwatch%parent)) then
       write(error_unit, '(A)') "ERROR: Unbalanced prof_tic and prof_toc combinations."
       return
    end if
    nullify(prof_cwatch)
    
    ! Current watch is the master watch.
    call system_clock(count=end, count_rate=count_rate)
    prof_mwatch%etime = prof_mwatch%etime + (end - prof_mwatch%start)

    ! Layout properties.
    call prof_layout_props(prof_mwatch, props)

    ! Write report header.
    do i=1,255
       line(i:i) = '-'
    end do
    
    
    call date_and_time(values=values)
    write(time_str, '(i0.2, ":", i0.2, ":", i0.2)') values(5), values(6), values(7)
    write(date_str ,'(i0.2, "-" ,i0.2, "-", i0.2)') values(3), values(2), values(1)
    write(error_unit, '(a)') "Profiler information of the program " // trim(prof_mwatch%name)
    write(error_unit, '(a)') "Created on " // trim(date_str) // " at " // trim(time_str) 
    write(error_unit, '(a)')   ""
    write(fmt, '("(a5, ", i0, "x, a4, ", i0, "x, 2x, a)")') props%count_maxlen - 5 + 4, props%name_maxlen - 4
    write(error_unit, fmt)     "Count", "Name", "Elapsed time"
    write(fmt, '("(a", i0, ")")') props%count_maxlen + 4 + props%name_maxlen + 2 + 16
    write(error_unit, fmt)     line
    
    write(fmt, '("(", i0, "x, a", i0, ", ", i0, "x, a)")')  &
         props%count_maxlen + 4, len_trim(prof_mwatch%name), props%name_maxlen - len_trim(prof_mwatch%name)+ 2
    write(error_unit, fmt) prof_mwatch%name, etime_str(prof_mwatch)
    if (associated(prof_mwatch%children)) then
       call prof_summary_family(prof_mwatch, props)
    end if
  end subroutine prof_report

  !> \brief Determines the layout properties of the report.
  subroutine prof_layout_props(mwatch, props)
    use, intrinsic :: iso_fortran_env,  only: int64
    
    type(watch_t), intent(in)  :: mwatch  !< Master watch.
    type(props_t), intent(out) :: props   !< Layout properties of the reports

    ! Locals
    integer(int64)         :: nused_max
    character(len=STR_LEN) :: count_str

    nused_max         = mwatch%nused
    props%name_maxlen = len_trim(mwatch%name)
    if (associated(mwatch%children)) then
       call prof_layout_props_update(props%name_maxlen, nused_max, mwatch)
    end if
    write(count_str, '(i0)') nused_max

    props%name_maxlen  = max(props%name_maxlen,   40)
    props%count_maxlen = max(len_trim(count_str), 3)
  end subroutine prof_layout_props

  !> \brief Updates the layout properties based on the information of the child watches.
  recursive subroutine prof_layout_props_update(name_maxlen, nused_max, watch)
    integer(int32), intent(inout) :: name_maxlen  !< Maximum string length to represent the name of the watch (including generation offset).
    integer(int64), intent(inout) :: nused_max    !< Maximum number of an watch has been used.
    type(watch_t),  intent(in)    :: watch        !< Watch.

    ! Locals
    integer(int32)          :: ichild
    type(watch_t),  pointer :: cwatch

    do ichild = 1, size(watch%children)
       cwatch => watch%children(ichild)

       nused_max   = max(nused_max,   cwatch%nused)
       name_maxlen = max(name_maxlen, len_trim(cwatch%name) + 2 * cwatch%generation)
       
       if (associated(cwatch%children)) then
          call prof_layout_props_update(name_maxlen, nused_max, cwatch)
       end if
    end do
    nullify(cwatch)
  end subroutine prof_layout_props_update
       
  !> \brief Prints the summary report of all the child watches.
  !!
  !! \warning At the end the pointer to the child watches is nullified.
  recursive subroutine prof_summary_family(watch, props)
    use, intrinsic :: iso_fortran_env,  only: int32, int64, real64, error_unit
    
    type(watch_t), target, intent(inout) :: watch  !< Watch.
    type(props_t),         intent(in)    :: props  !< Layout properties of the report.

    ! Locals
    integer(int32)         :: ichild
    integer(int64)         :: etime_others
    character(len=60)      :: fmt
    type(watch_t)          :: lwatch
    type(watch_t), pointer :: cwatch

    etime_others = 0
    do ichild = 1, size(watch%children)
       cwatch => watch%children(ichild)

       etime_others = etime_others + cwatch%etime
       write(fmt, '("(""["", i", i0, ", ""]"", ", i0, "x, a", i0, ", ", i0, "x, a)")')  &
            props%count_maxlen, 2 * cwatch%generation, len_trim(cwatch%name),           &
            props%name_maxlen - len_trim(cwatch%name) - 2 * watch%generation + 2
       write(error_unit, fmt) cwatch%nused, cwatch%name, etime_str(cwatch)
          
       if (associated(cwatch%children)) then
          call prof_summary_family(cwatch, props)
       end if
    end do
    nullify(cwatch, watch%children)
    
    lwatch%name   = "(others)"
    lwatch%etime  = watch%etime - etime_others
    lwatch%nunits = 0
    write(fmt, '("(", i0, "x, a", i0, ", ", i0, "x, a)")')                        &
         props%count_maxlen + 2 * (watch%generation + 2), len_trim(lwatch%name),  &
         props%name_maxlen - len_trim(lwatch%name) - 2 * watch%generation + 2
    write(error_unit, fmt) lwatch%name, etime_str(lwatch)
  end subroutine prof_summary_family

  !> \brief Computes the statistics of the watch, like elapsed time and data rate.
  subroutine prof_stats(elapsed, data_rate, watch)
    use, intrinsic :: iso_fortran_env,  only: int64, real64
    
    real(kind=real64),   intent(out) :: elapsed     !< Elapsed time in secondes.
    real(kind=real64),   intent(out) :: data_rate   !< Data rate in \em unit per seconde.
    type(watch_t),       intent(in)  :: watch       !< Watch.

    ! Locals
    integer(int64) :: count_rate
    
    call system_clock(count_rate=count_rate)
    elapsed   = real(a=watch%etime,  kind=real64) / real(a=count_rate, kind=real64)
    data_rate = real(a=watch%nunits, kind=real64) / elapsed
  end subroutine prof_stats

  !> \brief Constructs a string from the determined elapsed time and number of units.
  function etime_str(watch)
    use, intrinsic :: iso_fortran_env
    
    type(watch_t),           intent(in) :: watch       !< Watch.
    character(len=2*STR_LEN)            :: etime_str   !< Elapsed time and possibly data rate as a string.

    ! Parameters
    real(real64),                   parameter :: THRESHOLD = 1000._real64
    character(len=1), dimension(6), parameter :: PREFIX    = [ ' ', 'k', 'M', 'G', 'T', 'E' ]

    ! Locals
    integer(int32)         :: iunit
    real(real64)           :: etime, data_rate
    character(len=STR_LEN) :: data_rate_str

    call prof_stats(etime, data_rate, watch)
    
    data_rate_str = ''
    if (watch%nunits > 0) then
       iunit = 1
       do while (data_rate >= THRESHOLD)
          data_rate = data_rate / 1024._real64
          iunit     = iunit + 1
       end do
       write(data_rate_str, '("[", f6.2, 1x, a, "]")') data_rate, trim(PREFIX(iunit)) // trim(watch%unit) // '/sec'
    end if

    write(etime_str, '(f12.3, 1x, a3, 2x, a)') etime, 'sec', data_rate_str
  end function etime_str
end module profiler

