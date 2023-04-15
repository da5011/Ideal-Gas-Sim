program sim
    use iso_fortran_env, only: stdin => input_unit, stdout => output_unit
    use simFunctions
    use simProcedure
    implicit none
    
    call preInit
    call initialize
    call loop

end program sim