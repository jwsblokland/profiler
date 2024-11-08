!> \brief Profiler module
module profiler
  use :: profiler_types,  only: STR_LEN, prof_t
  implicit none

  private
  public  :: STR_LEN, prof_t, prof_init, prof_tic, prof_tic_rate, prof_toc, prof_report
  
  !> \cond _INTERNAL_
  !> \ingroup profileri
  !> \{
  type(prof_t), save :: prof_profiler  !< Internal profiler object.
  !> \}
  !> \endcond

  !> \ingroup profiler
  !> \{
  !> \brief Initializes the profiler.
  interface prof_init
     module procedure prof_init_external
     module procedure prof_init_internal
  end interface prof_init
  !> \}
  
  !> \ingroup profiler
  !> \{
  !> \brief Profiler tic subroutine.
  interface prof_tic
     module procedure prof_tic_external
     module procedure prof_tic_internal
  end interface prof_tic
  !> \}

  !> \ingroup profiler
  !> \{
  !> \brief Profiler tic_rate subroutine.
  interface prof_tic_rate
     module procedure prof_tic_rate_external
     module procedure prof_tic_rate_internal
  end interface prof_tic_rate
  !> \}

  !> \ingroup profiler
  !> \{
  !> \brief Profiler toc subroutine.
  interface prof_toc
     module procedure prof_toc_external
     module procedure prof_toc_internal
  end interface prof_toc
  !> \}

  !> \ingroup profiler
  !> \{
  !> \brief Prints a report of the profiling results.
  interface prof_report
     module procedure prof_report_external
     module procedure prof_report_internal
  end interface prof_report
  !> \}

contains
  !> \ingroup eprofiler
  !> \{
  !> \brief Initializes the profiler.
  !!
  !> \warning The variable \em name should not exceed 40 characters otherwise it will be truncated.
  subroutine prof_init_external(profiler, name)
    use, intrinsic :: iso_fortran_env,  only: int32
    use            :: profiler_types,   only: prof_t

    type(prof_t),     target, intent(inout) :: profiler  !< Profiler.
    character(len=*),         intent(in)    :: name      !< Name of the master watch.

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

    profiler%cwatch            => profiler%mwatch
    profiler%cwatch%name       =  name(offset:)
    profiler%cwatch%generation =  profiler%cwatch%generation + 1
    profiler%cwatch%nused      =  profiler%cwatch%nused      + 1
    nullify(profiler%cwatch%parent)
    call system_clock(count=profiler%cwatch%start)
  end subroutine prof_init_external
  !> \}

  !> \ingroup eprofiler
  !> \{
  !> \brief Profiler tic subroutine.
  !!
  !> \warning The variable \em name should not exceed 40 characters otherwise it will be truncated.
  !> \warning No check on base name of the unit will be performed for an existing name.
  subroutine prof_tic_external(profiler, name, nunits, unit)
    use, intrinsic :: iso_fortran_env,  only: int32, int64
    use            :: profiler_types,   only: prof_t, watch_t

    type(prof_t),     target,   intent(inout) :: profiler  !< Profiler.
    character(len=*),           intent(in)    :: name      !< Name of the child watch.
    integer(int64),   optional, intent(in)    :: nunits    !< Number of units that will be used.
    character(len=*), optional, intent(in)    :: unit      !< Base name of the unit.
    
    ! Locals
    integer(int32)                       :: ichild, nchildren
    type(watch_t), dimension(:), pointer :: children
    
    if (.not. associated(profiler%cwatch%children)) then
       ! No children.
       allocate(profiler%cwatch%children(1))
       profiler%cwatch%children(1)%parent => profiler%cwatch
       profiler%cwatch                    => profiler%cwatch%children(1)
       profiler%cwatch%name               =  trim(name)
       if (present(unit))  profiler%cwatch%unit =  trim(unit)
       profiler%cwatch%generation         =  profiler%cwatch%parent%generation + 1
    else
       ! Has children.
       nchildren = size(profiler%cwatch%children)
       do ichild = 1, nchildren
          if (trim(name) == trim(profiler%cwatch%children(ichild)%name)) then
             ! Existing child watch found.
             profiler%cwatch => profiler%cwatch%children(ichild)
             exit
          end if
       end do

       if (ichild > nchildren) then
          ! New child watch.
          allocate(children(nchildren + 1))
          children(1:nchildren) = profiler%cwatch%children
          nullify(profiler%cwatch%children)
          profiler%cwatch%children => children
          nullify(children)

          ! Set the parent of the new child watch.
          profiler%cwatch%children(nchildren + 1)%parent => profiler%cwatch

          profiler%cwatch            => profiler%cwatch%children(nchildren + 1)
          profiler%cwatch%name       =  trim(name)
          if (present(unit)) profiler%cwatch%unit = trim(unit)
          profiler%cwatch%generation =  profiler%cwatch%parent%generation + 1
       end if
    end if
    
    profiler%cwatch%nused = profiler%cwatch%nused + 1
    call system_clock(count=profiler%cwatch%start)
    if (present(nunits))  profiler%cwatch%nunits = profiler%cwatch%nunits + nunits
  end subroutine prof_tic_external
  !> \}

  !> \ingroup eprofiler
  !> \{
  !> \brief Profiler tic_rate subroutine.
  !!
  !> \warning The variable \em name should not exceed 40 characters otherwise it will be truncated.
  !> \warning No check on base name of the unit will be performed for an existing name.
  subroutine prof_tic_rate_external(profiler, name, nunits, unit)
    use, intrinsic :: iso_fortran_env,  only: int32, int64
    use            :: profiler_types,   only: prof_t, watch_t

    type(prof_t),     target,   intent(inout) :: profiler  !< Profiler.
    character(len=*),           intent(in)    :: name      !< Name of the child watch.
    integer(int64),             intent(in)    :: nunits    !< Number of units that will be used.
    character(len=*),           intent(in)    :: unit      !< Base name of the unit.

    call prof_tic_external(profiler, name, nunits, unit)
  end subroutine prof_tic_rate_external
  !> \}
  
  !> \ingroup eprofiler
  !> \{
  !> \brief Profiler toc subroutine
  subroutine prof_toc_external(profiler)
    use, intrinsic :: iso_fortran_env,  only: int64
    use            :: profiler_types,   only: prof_t

    type(prof_t), target, intent(inout) :: profiler  !< Profiler.

    ! Locals
    integer(int64) :: end

    call system_clock(count=end)
    profiler%cwatch%etime =  profiler%cwatch%etime + (end - profiler%cwatch%start)
    profiler%cwatch       => profiler%cwatch%parent
  end subroutine prof_toc_external
  !> \}

  !> \ingroup eprofiler
  !> \{
  !> \brief Prints a report of the profiling results.
  subroutine prof_report_external(profiler, unit)
    use, intrinsic :: iso_fortran_env,    only: int32, int64, error_unit
    use            :: profiler_datetime,  only: prof_datetime
    use            :: profiler_layout,    only: prof_layout_props
    use            :: profiler_types,     only: prof_t, props_t
    use            :: profiler_version,   only: prof_version

    type(prof_t),   target,   intent(inout) :: profiler  !< Profiler.
    integer(int32), optional, intent(in)    :: unit      !< File unit.
   
    ! Intrinsics
    intrinsic :: exit

    ! Locals
    integer(int32)                   :: lunit
    integer(int64)                   :: end, count_rate, i
    character(len=8)                 :: time_str
    character(len=17)                :: date_str
    character(len=12)                :: zone_str
    character(len=60)                :: fmt
    character(len=511)               :: line
    type(props_t)                    :: props

    lunit = error_unit
    if (present(unit)) then
       lunit = unit
    end if

    if (associated(profiler%cwatch%parent)) then
       write(lunit, '(A)') "ERROR: Unbalanced prof_tic and prof_toc combinations."
       call exit(1)
    end if
    nullify(profiler%cwatch)
    
    ! Current watch is the master watch.
    call system_clock(count=end, count_rate=count_rate)
    profiler%mwatch%etime = profiler%mwatch%etime + (end - profiler%mwatch%start)

    ! Layout properties.
    call prof_layout_props(profiler%mwatch, props)
    call prof_datetime(time_str, date_str, zone_str)
    do i=1,len(line)
       line(i:i) = '-'
    end do

    ! Write report header.
    write(lunit, '(a)') "Profiler " // trim(prof_version())
    write(lunit, '(a)') "Timing information of the master watch """ // trim(profiler%mwatch%name) // """"
    write(lunit, '(a)') "Report created at " // trim(time_str) // " on " // trim(date_str) // " "  // trim(zone_str)
    write(lunit, '(a)')   ""
    write(fmt, '("(a5, ", i0, "x, a4, ", i0, "x, 2x, a12, 6x, a)")') props%count_maxlen - 5 + 4, props%name_maxlen - 4
    write(lunit, fmt)     "Count", "Name", "Elapsed time", "Fraction"
    write(fmt, '("(a", i0, ")")') props%count_maxlen + 4 + props%name_maxlen + 2 + props%frac_maxlen + 2 + 16
    write(lunit, fmt)     line
    
    write(fmt, '("(", i0, "x, a", i0, ", ", i0, "x, a)")')  &
         props%count_maxlen + 4, len_trim(profiler%mwatch%name), props%name_maxlen - len_trim(profiler%mwatch%name)+ 2
    write(lunit, fmt) profiler%mwatch%name, etime_str(profiler%mwatch, profiler%mwatch%etime, props%frac_maxlen)
    if (associated(profiler%mwatch%children)) then
       call prof_summary_family(profiler%mwatch, props, lunit)
    end if
  end subroutine prof_report_external
  !> \}
  
  !> \ingroup iprofiler
  !> \{
  !> \brief Initializes the profiler.
  !!
  !> \warning The variable \em name should not exceed 40 characters otherwise it will be truncated.
  !> \note    It makes use of the internally defined profiler variable.
  subroutine prof_init_internal(name)
    character(len=*), intent(in) :: name  !< Name of the master watch.

    call prof_init_external(prof_profiler, name)
  end subroutine prof_init_internal
  !> \}
  
  !> \ingroup iprofiler
  !> \{
  !> \brief Profiler tic subroutine.
  !!
  !> \warning The variable \em name should not exceed 40 characters otherwise it will be truncated.
  !> \warning No check on base name of the unit will be performed for an existing name.
  !> \note    It makes use of the internally defined profiler variable.
  subroutine prof_tic_internal(name, nunits, unit)
    use, intrinsic :: iso_fortran_env,  only: int64
    
    character(len=*),           intent(in) :: name    !< Name of the child watch.
    integer(int64),   optional, intent(in) :: nunits  !< Number of units that will be used.
    character(len=*), optional, intent(in) :: unit    !< Name of the unit.
    
    if (present(nunits) .and. present(unit)) then
       call prof_tic_external(prof_profiler, name, nunits, unit)
    else if (present(nunits)) then
       call prof_tic_external(prof_profiler, name, nunits=nunits)
    else if (present(unit)) then
       call prof_tic_external(prof_profiler, name, unit=unit)
    else
       call prof_tic_external(prof_profiler, name)
    end if
  end subroutine prof_tic_internal
  !> \}

  !> \ingroup iprofiler
  !> \{
  !> \brief Profiler tic_rate subroutine.
  !!
  !> \warning The variable \em name should not exceed 40 characters otherwise it will be truncated.
  !> \warning No check on base name of the unit will be performed for an existing name.
  !> \note    It makes use of the internally defined profiler variable.
  subroutine prof_tic_rate_internal(name, nunits, unit)
    use, intrinsic :: iso_fortran_env,  only: int32, int64
    use            :: profiler_types,   only: prof_t, watch_t

    character(len=*), intent(in) :: name    !< Name of the child watch.
    integer(int64),   intent(in) :: nunits  !< Number of units that will be used.
    character(len=*), intent(in) :: unit    !< Base name of the unit.

    call prof_tic_rate_external(prof_profiler, name, nunits, unit)
  end subroutine prof_tic_rate_internal
  !> \}

  !> \ingroup iprofiler
  !> \{
  !> \brief Profiler toc subroutine
  !!
  !> \note It makes use of the internally defined profiler variable.
  subroutine prof_toc_internal()
    call prof_toc_external(prof_profiler)
  end subroutine prof_toc_internal
  !> \}
  
  !> \ingroup iprofiler
  !> \{
  !> \brief Prints a report of the profiling results.
  !!
  !> \note It makes use of the internally defined profiler variable.
  subroutine prof_report_internal(unit)
    use, intrinsic :: iso_fortran_env,  only: int32, error_unit

    integer(int32), optional, intent(in) :: unit  !< File unit.

    ! Locals
    integer(int32) :: lunit

    lunit = error_unit
    if (present(unit)) then
       lunit = unit
    end if

    call prof_report_external(prof_profiler, unit)
  end subroutine prof_report_internal
  !> \}
  
  !> \cond _INTERNAL_
  !> \ingroup profileri
  !> \{
  !> \brief Prints the summary report of all the child watches.
  !!
  !! \warning At the end the pointer to the child watches is nullified.
  recursive subroutine prof_summary_family(watch, props, unit)
    use, intrinsic :: iso_fortran_env,  only: int32, int64
    use            :: profiler_types,   only: props_t, watch_t
    
    type(watch_t), target, intent(inout) :: watch  !< Watch.
    type(props_t),         intent(in)    :: props  !< Layout properties of the report.
    integer(int32),        intent(in)    :: unit   !< File unit.

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
       write(unit, fmt) cwatch%nused, cwatch%name, etime_str(cwatch, watch%etime, props%frac_maxlen)
          
       if (associated(cwatch%children)) then
          call prof_summary_family(cwatch, props, unit)
       end if
    end do
    if (associated(cwatch))  nullify(cwatch)
    deallocate(watch%children)
    
    lwatch%name       = "(others)"
    lwatch%etime      = watch%etime - etime_others
    lwatch%generation = watch%generation + 1
    lwatch%nunits     = 0
    write(fmt, '("(", i0, "x, a", i0, ", ", i0, "x, a)")')                        &
         props%count_maxlen + 2 * (watch%generation + 2), len_trim(lwatch%name),  &
         props%name_maxlen - len_trim(lwatch%name) - 2 * watch%generation + 2
    write(unit, fmt) lwatch%name, etime_str(lwatch, watch%etime, props%frac_maxlen)
  end subroutine prof_summary_family
  !> \}
  !> \endcond

  !> \cond _INTERNAL_
  !> \ingroup profileri
  !> \{
  !> \brief Computes the statistics of a watch, like elapsed time and data rate.
  subroutine prof_stats(elapsed, data_rate, watch)
    use, intrinsic :: iso_fortran_env,  only: int64,real64
    use            :: profiler_types,   only: watch_t
    
    real(kind=real64),   intent(out) :: elapsed    !< Elapsed time in secondes.
    real(kind=real64),   intent(out) :: data_rate  !< Data rate in \em unit per seconde.
    type(watch_t),       intent(in)  :: watch      !< Watch.

    ! Locals
    integer(int64) :: count_rate
    
    call system_clock(count_rate=count_rate)
    elapsed   = real(a=watch%etime,  kind=real64) / real(a=count_rate, kind=real64)
    data_rate = real(a=watch%nunits, kind=real64) / elapsed
  end subroutine prof_stats
  !> \}
  !> \endcond

  !> \cond _INTERNAL_
  !> \ingroup profileri
  !> \{
  !> \brief Constructs a string from the determined elapsed time and number of units.
  function etime_str(watch, etime_parent, frac_maxlen)
    use, intrinsic :: iso_fortran_env,  only: int32, int64, real64
    use            :: profiler_types,   only: STR_LEN, watch_t
    
    type(watch_t),           intent(in) :: watch         !< Watch.
    integer(int64),          intent(in) :: etime_parent  !< The total elapsed time of the parent watch.
    integer(int32),          intent(in) :: frac_maxlen   !< Maximum string length to represent the elasped time as a fraction (including generation offset).
    character(len=2*STR_LEN)            :: etime_str     !< Elapsed time and possibly data rate as a string.

    ! Parameters
    real(real64),                   parameter :: THRESHOLD = 1000._real64
    character(len=2), dimension(6), parameter :: PREFIX    = [ '  ', 'ki', 'Mi', 'Gi', 'Ti', 'Ei' ]

    ! Locals
    integer(int32)         :: iunit, offset, frac_size
    real(real64)           :: etime, fraction, data_rate
    character(len=255)     :: fmt
    character(len=STR_LEN) :: data_rate_str

    fraction = 100._real64 * real(watch%etime, real64) / real(etime_parent, real64)
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

    offset    = 1 + 2 * (watch%generation - 1)
    frac_size = 5
    if (fraction >= 99.995_real64)  frac_size = frac_size + 1
    write(fmt, '("(f12.3, 1x, a3, 1x, ", i0, "x, f", i0, ".2, a1, ", i0, "x, a)")')  &
         offset, frac_size, 2 + frac_maxlen - offset - frac_size
    write(etime_str, fmt) etime, 'sec', fraction, '%', data_rate_str
  end function etime_str
  !> \}
  !> \endcond
end module profiler
