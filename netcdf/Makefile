.SUFFIXES: .o .f90

all: 
#	gfortran-mp-devel -o test_netcdf.exe -I/Users/barlage/programs/netcdf-fortran/lib/include test_netcdf.f90 -L/Users/barlage/programs/netcdf-fortran/lib/lib -lnetcdff
	gfortran-mp-12 -o test_netcdf.exe -I/opt/local/include test_netcdf.f90 -L/opt/local/lib -lnetcdff

clean:
	$(RM) test_netcdf.exe *.nc

