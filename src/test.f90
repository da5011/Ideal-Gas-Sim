program test
    use iso_fortran_env, only: stdin => input_unit, stdout => output_unit

    integer :: n, i, j, nSteps, cols, seed
    real :: bx, by, k, T, m, stepSize, vrms, vel, bruh
    real, allocatable :: x(:), y(:), vx(:), vy(:), xguess(:), yguess(:)

    call preInit
    call initialize

    contains
    subroutine preInit
        k = 1
        !k = (1.380649)*(0.00001) !Boltzmann in nm
        cols = 0
        open(stdin, file = "IO/input.txt", action = "read")
        read(stdin, *) n
        read(stdin, *) T
        read(stdin, *) m
        read(stdin, *) bx
        read(stdin, *) by
        read(stdin, *) nSteps
        read(stdin, *) stepSize
        read(stdin, *) seed
    end subroutine preInit

    subroutine initialize
        allocate(x(n))
        allocate(y(n))
        allocate(vx(n))
        allocate(vy(n))
        vrms = sqrt(3*k*T/m)
    
        open(stdout, file = "IO/output.txt", action = "write")
        write(stdout, *) "Populating box and setting intial velocities"
        do i = 1, n
            bruh = plusminone()
            x(i) = rand()*bx
            y(i) = rand()*by
            vel = plusminone()*boxmuller(vrms)
            vx(i) = plusminone()*rand()*vel
            vy(i) = plusminone()*sqrt(vrms**2 - vx(i)**2)
            write(stdout, *) bruh, vel, vx(i), vy(i)
        enddo
    end subroutine initialize

    real function boxmuller(mean)
    real, optional, intent(in) :: mean
    real :: u, v, hm, pi
    pi = 3.14159265359

    if(present(mean)) then
        hm = mean
    else
        hm = 0
    endif

    u = rand()
    v = rand()
    
    boxmuller = sqrt(-2*log(u))*cos(2*pi*v) + mean

    end function boxmuller

    real function plusminone()
    real :: a
    a = rand()
    if (a < 0.5) then
        plusminone = -1
    else 
        plusminone = 1
    endif
end function plusminone

end program test