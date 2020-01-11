!> \cond _INTERNAL_
!> \brief Module containing functions and subroutines regarding the version number.
module profiler_version
  use, intrinsic :: iso_fortran_env,  only: int32
  implicit none

  private
  public  :: prof_version

  !> \ingroup versioni
  !> \{
  integer(int32), parameter :: PROFILER_MAJOR = 0 !@PROJECT_VERSION_MAJOR@
  integer(int32), parameter :: PROFILER_MINOR = 1 !@PROJECT_VERSION_MINOR@
  integer(int32), parameter :: PROFILER_PATCH = 0 !@PROJECT_VERSION_PATCH@
  !> \}
  
contains
  !> \ingroup versioni
  !> \{
  !> \brief Returns the version number as string.
  function prof_version() result(version)
    character(len=8) :: version  !< Version number.

    write(version, 100) PROFILER_MAJOR, PROFILER_MINOR, PROFILER_PATCH

100 format(i0, ".", i0 , ".", i0)
  end function prof_version
  !> \}
end module profiler_version
!> \endcond
