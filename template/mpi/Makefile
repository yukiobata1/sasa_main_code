############################################################################
# Makefile for MPI computation
############################################################################

#Define compiler
#  F_COMP=ifort
   F_COMP=mpiifort
   
#Set compile options (Remove needless options after developing phase for )
   #-fast      :High speed option
   #-openmp    :Activate OpenMP commands
   #-traceback :Display line number of the code in case abnornal termination
   #-warn all  :Display all warning message when compiling
   #-assume byterecl :Set unit of direct access "recl" as byte 
   #
   #OPT_COMPILES= -fast -warn all
   OPT_COMPILES= -assume byterecl -O3 -ipo -xCORE-AVX512 -g -CB -traceback
   
#Link option 1
   #= -openmp :Enable OpenMP expression
   OPT_LINK1= 
   
#Link option 2
   #= -pg :Enable profiler
   OPT_LINK2=
   
#Define execution file name 
   EXE= go.out
   
#Define Source Files
   F_SRC=  modules.f90 \
           start_mpi.f90 \
           main.f90 \
           initialize.f90 \
           metabolic.f90 \
           output.f90 \
           physics.f90 \
           population_regu.f90 \
           spatial_calc.f90 \
           SFLXALL_SRC_VER_2.7.1.f90 \
           etc.f90
   
$(EXE): $(F_SRC)
	$(F_COMP) -c $(OPT_COMPILES) $(?)
	$(F_COMP) $(OPT_LINK1) -o $(EXE)  *.o $(OPT_LINK2)
   
# Definition for make clean
clean:
	rm -f $(EXE) *.o *.L gmon.out *.mod core

