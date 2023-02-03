# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../user_build_options

OBJS =  mpi_land.o 

all:	$(OBJS)
mpi_land.o: mpi_land.f90
	@echo ""
	rm -f $(*).o $(*).mod 
	$(COMPILERF90) -cpp $(F90FLAGS) $(FREESOURCE) -c $(*).f90

clean:
	rm -f  *.o *.mod 
