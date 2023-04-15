module simFunctions
    use iso_fortran_env, only: stdin => input_unit, stdout => output_unit
    implicit none
private
public plusminone

contains
real function plusminone()
    real :: a, b
    a = rand()
    b = rand()
    if (a < 0.5) then
        a = -1
    else 
        a = 1
    endif
    plusminone = a*b
end function plusminone


end module simFunctions