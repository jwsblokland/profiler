program program
  use, intrinsic :: iso_fortran_env
  use            :: profiler
  implicit none

  integer(int32)     :: i
  character(len=255) :: name

  call get_command_argument(0, name)
  call prof_init(trim(name))
  
  call prof_tic("level 1")
     call execute_command_line("sleep 0.1", wait=.true.)

     do i=1,2
        call prof_tic("level 1.1", 1000_int64 * kind(i), 'B')
          call execute_command_line("sleep 0.11", wait=.true.)
          call prof_tic("level 1.1.1")
            call execute_command_line("sleep 0.111", wait=.true.)
          call prof_toc()
          call prof_tic("level 1.1.2")
            call execute_command_line("sleep 0.112", wait=.true.)
          call prof_toc()
        call prof_toc()
     end do
        
     call prof_tic("level 1.2")
       call execute_command_line("sleep 0.12", wait=.true.)
 
       call prof_tic("level 1.2.1")
         call execute_command_line("sleep 0.121", wait=.true.)
       call prof_toc()
     call prof_toc()
  call prof_toc()

  call prof_report()
end program program
