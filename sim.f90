program sim
    use iso_fortran_env, only: stdin => input_unit, stdout => output_unit
    implicit none
    
    integer :: n, i, j, nSteps
    real :: bx, by, k, T, m, vrms, stepSize
    real, allocatable :: x(:), y(:), vx(:), vy(:), xguess(:), yguess(:)
    k = 1
    !k = (1.380649)*(0.00001) !Boltzmann in nm

    call preInit
    call initialize
    call loop

    contains

    subroutine preInit
        open(stdin, file = "input.txt", action = "read")
        read(stdin, *) n
        read(stdin, *) T
        read(stdin, *) m
        read(stdin, *) bx
        read(stdin, *) by
        read(stdin, *) nSteps
        read(stdin, *) stepSize
    end subroutine preInit
    
    subroutine initialize
        allocate(x(n))
        allocate(y(n))
        allocate(vx(n))
        allocate(vy(n))
        vrms = sqrt(3*k*T/m)
    
        open(stdout, file = "output.txt", action = "write")
        write(stdout, *) "Populating box and setting intial velocities"
        do i = 1, n
            x(i) = rand()*bx
            y(i) = rand()*by
            vx(i) = rand()*vrms
            vy(i) = sqrt(vrms**2 - vx(i)**2)
            write(stdout, *) x(i), y(i), vx(i), vy(i)
        enddo
    end subroutine initialize

    subroutine loop
        allocate(xguess(n))
        allocate(yguess(n))
        write(stdout, *) "Beginning Simulation"
        do j = 0, nSteps
            do i = 1, n
                xguess(i) = x(i) + vx(i)*stepSize
                yguess(i) = y(i) + vy(i)*stepSize

                if (xguess(i) > bx) then
                    x(i) = 2*bx - xguess(i)
                    vx(i) = -vx(i)
                elseif (xguess(i) < 0) then
                    x(i) = x(i) - vx(i)*stepSize
                    vx(i) = -vx(i)
                else
                    x(i) = xguess(i)
                endif

                if (yguess(i) > by) then
                    y(i) = 2*by - yguess(i)
                    vy(i) = -vy(i)
                elseif (yguess(i) < 0) then
                    y(i) = y(i) - vy(i)*stepSize
                    vy(i) = -vy(i)
                else
                    y(i) = yguess(i)
                endif

                write(stdout, *) x(i), y(i), vx(i), vy(i)
            end do
            write(stdout, *) "Step ", j, "complete"
        end do
        write(stdout, *) "Simulation Complete"
    end subroutine loop

end program sim