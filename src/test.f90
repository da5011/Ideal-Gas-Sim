program test
    use iso_fortran_env, only: stdin => input_unit, stdout => output_unit

    integer :: i
    real :: x, num, vel
    num = 0
    vel = 4

    open(stdout, file = "rand.txt", action = "write")

    do i = 1, 10
        x = vel*boxmuller(num)
        write(stdout, *) x
    enddo

    contains

    real function boxmuller(mean)
    real, optional, intent(in) :: mean
    real :: u, v, m, pi
    pi = 3.14159265359

    if(present(mean)) then
        m = mean
    else
        m = 0
    endif

    u = rand()
    v = rand()
    
    boxmuller = sqrt(-2*log(u))*cos(2*pi*v) + mean

    end function boxmuller

end program test