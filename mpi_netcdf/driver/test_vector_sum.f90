program test_vector_sum

  use module_mpi_land, only: mpi_land_init, mpi_land_type, mpi_land_finalize
  use mpi
  use netcdf

  implicit none

  type(mpi_land_type) :: blah
  integer :: fullx, xbeg, xend, numx, ix
  integer :: status, ncid, latid, timid, varid
  real, allocatable :: process(:)
  
  fullx = 100
  
  call  mpi_land_init(fullx,blah)
  
  xbeg = blah%location_begin
  xend = blah%location_end
  numx = blah%location_end - blah%location_begin + 1
  
  allocate(process(xbeg:xend))
  
  print*, 'numprocs: ',blah%numprocs
  print*, 'full:', blah%my_id,fullx
    print*, 'range:', blah%my_id,blah%location_begin,blah%location_end
  
  do ix = xbeg, xend
    process(ix) = ix
  end do
  
  status = nf90_create('test.nc', NF90_NETCDF4 ,ncid, comm = MPI_COMM_WORLD, &
       info = MPI_INFO_NULL)
  status = nf90_def_dim(ncid, "time", NF90_UNLIMITED, timid)
  status = nf90_def_dim(ncid, "lat" , fullx         , latid)
  status = nf90_def_var(ncid, "data", nf90_float    ,(/latid,timid/),varid)
  status = nf90_enddef(ncid)
  status = nf90_close(ncid)

  status = nf90_open('test.nc', NF90_WRITE, ncid, comm = MPI_COMM_WORLD, &
       info = MPI_INFO_NULL)

  status = nf90_var_par_access(ncid,varid,NF90_COLLECTIVE)
  status = nf90_inq_varid(ncid, "data", varid)
  status = nf90_put_var(ncid,varid,process,start=(/xbeg,1/),count=(/numx,1/))
  
  process = process*2
  
  status = nf90_put_var(ncid,varid,process,start=(/xbeg,2/),count=(/numx,1/))

  status = nf90_close(ncid)
  
  call mpi_land_finalize()
  
end program
