!> \cond _INTERNAL_
!> \brief Module containing functions and subroutines needed for printing the profiling report.
module profiler_layout
  implicit none

  private
  public  :: prof_layout_props
  
contains
  !> \ingroup layouti
  !> \{
  !> \brief Determines the layout properties of the report.
  subroutine prof_layout_props(mwatch, props)
    use, intrinsic :: iso_fortran_env,  only: int64
    use            :: profiler_types,   only: STR_LEN, props_t, watch_t
    
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

    props%count_maxlen = max(len_trim(count_str), 3)
    props%name_maxlen  = max(props%name_maxlen,   40)
    props%perc_maxlen  = 10
  end subroutine prof_layout_props
  !> \}

  !> \ingroup layouti
  !> \{
  !> \brief Updates the layout properties based on the information of the child watches.
  recursive subroutine prof_layout_props_update(name_maxlen, nused_max, watch)
    use, intrinsic :: iso_fortran_env,  only: int32, int64
    use            :: profiler_types,   only: watch_t

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
  !> \}
end module profiler_layout
!> \endcond
