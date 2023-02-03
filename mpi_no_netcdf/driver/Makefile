# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = test_vector_sum.o

all:	$(OBJS)

test_vector_sum.o: test_vector_sum.f90
	$(COMPILERF90) -cpp -c $(F90FLAGS) $(FREESOURCE) $(MODFLAG) $(*).f90

# This command cleans up object files, etc.
clean:
	$(RM) *.o *.mod
