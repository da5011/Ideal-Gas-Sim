program test
    use iso_fortran_env, only: stdin => input_unit, stdout => output_unit
    use simFunctions

    real :: a, b
    integer :: i
    a = 1

    do i = 1, 100
    b = plusminone(a)
    print *, a
    enddo

end program test