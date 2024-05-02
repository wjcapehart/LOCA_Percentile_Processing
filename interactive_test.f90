PROGRAM test_get_command_argument
    INTEGER :: myhuc_low
    CHARACTER(len=8) :: cmd_line_arg


    CALL get_command_argument(int(1), cmd_line_arg)
    read(cmd_line_arg,*) myhuc_low

    print*, myhuc_low


END PROGRAM

!  ifort -o interactive_test.exe  ./interactive_test.f90 
