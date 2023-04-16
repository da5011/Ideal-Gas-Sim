module simFunctions
    use iso_fortran_env, only: stdin => input_unit, stdout => output_unit
    implicit none
private
public plusminone, pdf

contains
real pure function plusminone()
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

real pure function pdf(x, mean, stdev)
    real :: x, mean, stdev, pi
    pi = 3.14159265359
    pdf = (1/(stdev*sqrt(2*pi)))*exp(-0.5*(((x - mean)/stdev)**2))
end function pdf


end module simFunctions