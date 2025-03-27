.SUFFIXES: .o .f90

all: 
	gfortran-13 -o test_netcdf.exe -I$(netcdf_fortran_ROOT)/include test_netcdf.f90 -L$(netcdf_fortran_ROOT)/lib -lnetcdff
clean:
	$(RM) test_netcdf.exe *.nc

