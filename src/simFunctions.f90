module simFunctions
    use iso_fortran_env, only: stdin => input_unit, stdout => output_unit
    implicit none
private
public plusminone, pdf, boxmuller

contains
real function plusminone()
    real :: a
    a = rand()
    if (a < 0.5) then
        plusminone = -1
    else 
        plusminone = 1
    endif
end function plusminone

real pure function pdf(x, mean, stdev)
    real, intent(in) :: x, mean, stdev
    real :: pi
    pi = 3.14159265359
    pdf = (1/(stdev*sqrt(2*pi)))*exp(-0.5*(((x - mean)/stdev)**2))
end function pdf

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

end module simFunctions