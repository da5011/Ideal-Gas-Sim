module simProcedure
    use iso_fortran_env, only: stdin => input_unit, stdout => output_unit
    use simFunctions
    implicit none

    integer :: n, i, j, nSteps, cols, seed
    real :: bx, by, k, T, m, stepSize, vrms, vel, interimForce, totalForce, interimPressure, avgPressure
    real, allocatable :: x(:), y(:), vx(:), vy(:), xguess(:), yguess(:)

    contains
    subroutine preInit
        k = 1
        !k = (1.380649)*(0.00001) !Boltzmann in nm
        cols = 0
        totalForce = 0
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
            x(i) = rand()*bx
            y(i) = rand()*by
            vel = plusminone()*boxmuller(vrms)
            vx(i) = plusminone()*rand()*vel
            vy(i) = plusminone()*sqrt(vel**2 - vx(i)**2)
            write(stdout, *) x(i), y(i), vx(i), vy(i)
        enddo
    end subroutine initialize

    subroutine loop
        allocate(xguess(n))
        allocate(yguess(n))
        write(stdout, *) "Beginning Simulation: ", nSteps, "steps, with ", n, "particles."
        do j = 1, nSteps
            write(stdout, *) "Step ", j, "of", nSteps
            interimForce = 0
            do i = 1, n
                xguess(i) = x(i) + vx(i)*stepSize
                yguess(i) = y(i) + vy(i)*stepSize

                if (xguess(i) > bx) then
                    x(i) = 2*bx - xguess(i)
                    vx(i) = -vx(i)
                    cols = cols + 1
                elseif (xguess(i) < 0) then
                    x(i) = x(i) - vx(i)*stepSize
                    vx(i) = -vx(i)
                    cols = cols + 1
                    interimForce = interimForce + (2*vx(i)/stepSize)
                else
                    x(i) = xguess(i)
                endif

                if (yguess(i) > by) then
                    y(i) = 2*by - yguess(i)
                    vy(i) = -vy(i)
                    cols = cols + 1
                elseif (yguess(i) < 0) then
                    y(i) = y(i) - vy(i)*stepSize
                    vy(i) = -vy(i)
                    cols = cols + 1
                    interimForce = interimForce + (2*vy(i)/stepSize)
                else
                    y(i) = yguess(i)
                endif
                write(stdout, *) x(i), y(i), vx(i), vy(i)
            enddo
            totalForce = totalForce + interimForce
            interimPressure = interimForce/(2*bx + 2*by)
            write(stdout, *) interimPressure
        enddo
        deallocate(xguess)
        deallocate(yguess)
        avgPressure = (totalForce)/(nSteps*(2*bx + 2*by))
        write(stdout, *) "Simulation Complete"
        write(stdout, *) "# of collisions:", cols
        write(stdout, *) "Average Pressure:", avgPressure
    end subroutine loop
end module simProcedure