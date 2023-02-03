program test_netcdf

  use netcdf

  implicit none

  integer :: status, varid, dim_id, ncid
  real, dimension(3) :: data
  
  data = (/1.0,3.0,2.0/)


  status = nf90_create("blah.nc", NF90_CLOBBER, ncid)
  status = nf90_def_dim(ncid, "location", 3, dim_id)
  status = nf90_def_var(ncid, "data", NF90_FLOAT, (/dim_id/), varid)
  status = nf90_enddef(ncid)

  status = nf90_put_var(ncid, varid , data)   
  status = nf90_close(ncid)
  
  data = -999.0

  status = nf90_open("blah.nc", NF90_NOWRITE, ncid)
  status = nf90_inq_varid(ncid, "data", varid)
  status = nf90_get_var(ncid, varid , data)
  status = nf90_close(ncid)

  print*, 'data: ',data

end program
