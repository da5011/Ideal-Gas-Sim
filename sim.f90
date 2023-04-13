program sim
    use iso_fortran_env, only: stdin => input_unit, stdout => output_unit
    implicit none
    
    integer :: n, i
    real :: bx, by, R, T, m
    real, allocatable :: x(:), y(:), vx(:), vy(:)
    R = 8.314

    open(stdin, file = "input.txt", action = "read")
    read(*) n
    read(*) T
    read(*) m
    read(*) bx
    read(*) by

    !Want to initialize by randomizing positions of all atoms
    !and their velocities such that the average corresponds with the temperature

    allocate(x(n))
    allocate(y(n))
    allocate(vx(n))
    allocate(vy(n))

    open(stdout, file = "output.txt", action = "write")
    write(*) "Populating box and setting intial velocities..."
    do i = 1, n
        x(i) = rand()*bx
        write(x(i))
        y(i) = rand()*by
        write(y(i))
    enddo

end program sim