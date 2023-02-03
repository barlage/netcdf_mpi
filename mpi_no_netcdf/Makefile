
include ./user_build_options

all: user_build_options
	(cd mpi;                        make)
	(cd driver;			make)
	(cd run;			make)

clean:
	(cd mpi;                        make clean)
	(cd driver;			make clean)
	(cd run;			make clean)

