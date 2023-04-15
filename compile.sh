#!/bin/bash

gfortran src/simFunctions.f90 src/simProcedure.f90 src/sim.f90 -J obj/ -o sim.exe