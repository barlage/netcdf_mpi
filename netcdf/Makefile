.SUFFIXES: .o .f90

all: 
	gfortran -o test_netcdf.exe -I/usr/include test_netcdf.f90 -lnetcdff
clean:
	$(RM) test_netcdf.exe *.nc

