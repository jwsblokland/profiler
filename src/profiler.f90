!> \brief Profiler module
module profiler
  use, intrinsic :: iso_fortran_env,  only: int32, int64
  implicit none

  private
  public  :: prof_init, prof_tic, prof_toc, prof_report

  integer(int32), parameter :: STR_LEN = 40
  
  type :: watch_t
     type(watch_t),          pointer               :: parent     => null()  !< Parent watch.
     type(watch_t),          dimension(:), pointer :: children   => null()  !< Child watches.
     character(len=STR_LEN)                        :: name                  !< Name of the watch.
     integer(int32)                                :: generation = -1       !< Family generation.
     integer(int64)                                :: start      =  0       !< Starting counting time.
     integer(int64)                                :: time       =  0       !< Sum of the elasped counting time.
     integer(int64)                                :: nbytes     =  0       !< Sum of the number of bytes that have been used.
     integer(int64)                                :: nused      =  0       !< Number of times this watch has been used.
  end type watch_t

  type(watch_t), target,  save :: prof_mwatch            !< Master watch.
  type(watch_t), pointer, save :: prof_cwatch => null()  !< Current watch.
  
contains
  !> \brief Initializes the profiler.
  !!
  !! \note Currently, the maximum number of watches has been set to a hundred.
  subroutine prof_init(name)
    character(len=*), intent(in) :: name  !< Name of the master watch.

    ! Locals
    integer(int32) :: offset
    
    ! If needed remove './' from the name of the master watch.
    offset = 1
    if (name(1:2) == './')  offset = 3
    
    prof_cwatch            => prof_mwatch
    prof_cwatch%name       =  name(offset:)
    prof_cwatch%generation =  prof_cwatch%generation + 1
    prof_cwatch%nused      =  prof_cwatch%nused      + 1
    call system_clock(count=prof_cwatch%start)
  end subroutine prof_init
  
  !> \brief Profiler tic subroutine.
  subroutine prof_tic(name, nbytes)
    character(len=*),           intent(in) :: name    !< Name of the child watch.
    integer(int32),   optional, intent(in) :: nbytes  !< Number of bytes (or words) that will be used.
    
    ! Locals
    integer(int32)                       :: ichild, nchildren
    type(watch_t), dimension(:), pointer :: children
    
    if (.not. associated(prof_cwatch%children)) then
       ! No children.
       allocate(prof_cwatch%children(1))
       prof_cwatch%children(1)%parent => prof_cwatch
       prof_cwatch                    => prof_cwatch%children(1)
       prof_cwatch%name               =  trim(name)
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

          ! Set the parent of the new child watch.
          prof_cwatch%children(nchildren + 1)%parent => prof_cwatch

          prof_cwatch            => prof_cwatch%children(nchildren + 1)
          prof_cwatch%name       =  trim(name)
          prof_cwatch%generation =  prof_cwatch%parent%generation + 1
       end if
    end if
    
    prof_cwatch%nused = prof_cwatch%nused + 1
    call system_clock(count=prof_cwatch%start)
    if (present(nbytes))  prof_cwatch%nbytes = prof_cwatch%nbytes + nbytes
  end subroutine prof_tic

  !> \brief Profiler toc subroutine
  subroutine prof_toc()
    ! Locals
    integer(int64) :: end

    call system_clock(count=end)
    prof_cwatch%time =  prof_cwatch%time + (end - prof_cwatch%start)
    prof_cwatch      => prof_cwatch%parent
  end subroutine prof_toc
  
  !> \brief Prints a report of the profiling results.
  subroutine prof_report()
    use, intrinsic :: iso_fortran_env,  only: int64, real64, error_unit
    
    ! Locals
    integer(int64)         :: end, count_rate
    real(real64)           :: etime, data_rate
    character(len=STR_LEN) :: name

    if (associated(prof_cwatch%parent)) then
       write(error_unit, '(A)') "ERROR: Unbalanced prof_tic and prof_toc combinations."
       return
    end if
    
    ! Current watch is the master watch.
    call system_clock(count=end, count_rate=count_rate)
    prof_cwatch%time = prof_cwatch%time + (end - prof_cwatch%start)

    ! Write report header.
    name = "Name"
    write(error_unit, 100) "Count   ", name, "Elapsed time [s]", "Data rate"
    write(error_unit, 110) "-------------------------------------------------------------------------------"
    
    call prof_stats(etime, data_rate, prof_cwatch, count_rate)
    write(error_unit, 200) prof_cwatch%name, etime
    if (associated(prof_cwatch%children)) then
       call prof_summary_family(prof_cwatch, count_rate)
    end if
    
100 format(a8, 2x, a40, 2x, a16, 2x, a9)
110 format(a79)
200 format(8x, 2x, a40, 2x, f16.3)
  end subroutine prof_report

  !> \brief Prints the summary report of all the child watches.
  recursive subroutine prof_summary_family(watch, count_rate)
    use, intrinsic :: iso_fortran_env,  only: int32, int64, real64, error_unit
    
    type(watch_t),  target, intent(inout) :: watch       !< Watch.
    integer(int64),         intent(in)    :: count_rate  !< Number of clock ticks per seconde.

    ! Locals
    integer(int32)         :: ichild
    integer(int64)         :: time
    real(real64)           :: etime, data_rate
    character(len=50)      :: fmt_full, fmt_time, fmt_min
    type(watch_t)          :: lwatch
    type(watch_t), pointer :: cwatch

    ! Create format descriptions.
    write(fmt_full, '("(""["", i6, ""]"", ", I0, "x, a", I0, ", 2x, f16.3, 2x, f9.2)")')  &
         2 * (watch%generation + 1), 40 - 2 * watch%generation
    write(fmt_time, '("(""["", i6, ""]"", ", I0, "x, a", I0, ", 2x, f16.3)")')   &
         2 * (watch%generation + 1), 40 - 2 * watch%generation
    write(fmt_min, '("(", I0, "x, a", I0, ", 2x, f16.3)")') 8 + 2 * (watch%generation + 1), 40 - 2 * watch%generation
    
    time = 0
    do ichild = 1, size(watch%children)
       cwatch => watch%children(ichild)

       time = time + cwatch%time
       call prof_stats(etime, data_rate, cwatch, count_rate)
       if (cwatch%nbytes > 0) then
          write(error_unit, fmt_full) cwatch%nused, cwatch%name, etime, data_rate
       else
          write(error_unit, fmt_time) cwatch%nused, cwatch%name, etime
       end if
          
       if (associated(cwatch%children)) then
          call prof_summary_family(cwatch, count_rate)
       end if
    end do
    nullify(watch%children)
    
    lwatch%name   = "(others)"
    lwatch%time   = watch%time - time
    lwatch%nbytes = 0
    call prof_stats(etime, data_rate, lwatch, count_rate)
    write(error_unit, fmt_min) lwatch%name, etime
  end subroutine prof_summary_family

  !> \brief Computes the statistics of the watch, like elapsed time and data rate.
  subroutine prof_stats(elapsed, data_rate, watch, count_rate)
    use, intrinsic :: iso_fortran_env,  only: real64
    
    real(kind=real64),   intent(out) :: elapsed     !< Elapsed time in secondes.
    real(kind=real64),   intent(out) :: data_rate   !< Data rate in \em unit per seconde.
    type(watch_t),       intent(in)  :: watch       !< Watch.
    integer(kind=int64), intent(in)  :: count_rate  !< Number of clock ticks per seconde.

    elapsed   = real(a=watch%time,   kind=real64) / real(a=count_rate, kind=real64)
    data_rate = real(a=watch%nbytes, kind=real64) / elapsed
  end subroutine prof_stats
end module profiler

