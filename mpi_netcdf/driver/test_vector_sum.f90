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
    if (status /= nf90_noerr) call handle_err(status,"nf90_create")
  status = nf90_def_dim(ncid, "time", NF90_UNLIMITED, timid)
    if (status /= nf90_noerr) call handle_err(status,"nf90_def_dim time")
  status = nf90_def_dim(ncid, "lat" , fullx         , latid)
    if (status /= nf90_noerr) call handle_err(status,"nf90_def_dim lat")
  status = nf90_def_var(ncid, "data", nf90_float    ,(/latid,timid/),varid)
    if (status /= nf90_noerr) call handle_err(status,"nf90_def_var data")
  status = nf90_enddef(ncid)
    if (status /= nf90_noerr) call handle_err(status,"nf90_enddef")
  status = nf90_close(ncid)
    if (status /= nf90_noerr) call handle_err(status,"nf90_close")

  status = nf90_open('test.nc', NF90_WRITE, ncid, comm = MPI_COMM_WORLD, &
       info = MPI_INFO_NULL)
    if (status /= nf90_noerr) call handle_err(status,"nf90_open")

  status = nf90_var_par_access(ncid,varid,NF90_COLLECTIVE)
    if (status /= nf90_noerr) call handle_err(status,"nf90_var_par_access")
  status = nf90_inq_varid(ncid, "data", varid)
    if (status /= nf90_noerr) call handle_err(status,"nf90_inq_varid data")
  status = nf90_put_var(ncid,varid,process,start=(/xbeg,1/),count=(/numx,1/))
    if (status /= nf90_noerr) call handle_err(status,"nf90_put_var process1")
  
  process = process*2
  
  status = nf90_put_var(ncid,varid,process,start=(/xbeg,2/),count=(/numx,1/))
    if (status /= nf90_noerr) call handle_err(status,"nf90_put_var process2")

  status = nf90_close(ncid)
    if (status /= nf90_noerr) call handle_err(status,"nf90_close")
  
  call mpi_land_finalize()
  
end program

subroutine handle_err(status, message)
  use netcdf
  integer, intent ( in) :: status
  character(len=*), intent ( in) :: message
 
  if(status /= nf90_noerr) then
    print *, trim(nf90_strerror(status))
    if(present(message)) print *, trim(message)
    stop "Stopped"
  end if
end subroutine handle_err

