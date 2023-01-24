#!/bin/sh
ifort -c treeType_mod.f90
ifort -c AVL_mod.f90
ifort -check bounds treeType_mod.o AVL_mod.o AVL_driver.f90 -o AVL 
rm *.o
rm *.mod
#./tracer
