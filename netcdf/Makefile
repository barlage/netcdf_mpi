.SUFFIXES: .o .f90

all: 
	ifort -o test_netcdf.exe -I$(NETCDF) test_netcdf.f90 -L$(NETCDF) -lnetcdff

clean:
	$(RM) test_netcdf.exe *.nc

