.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = ../driver/test_vector_sum.o \
       ../mpi/mpi_land.o 

CMD = ufsLand.exe
all:	$(CMD)

ufsLand.exe: $(OBJS)
	$(COMPILERF90) -o $(@) $(OBJS) $(LIBFLAG)

clean:
	$(RM) *~ $(CMD)

