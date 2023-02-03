program test_vector_sum

  use module_mpi_land, only: mpi_land_init, mpi_land_type, mpi_land_finalize
  use mpi

  implicit none

  type(mpi_land_type) :: blah
  integer :: fullx, xstart, xend
  
  fullx = 999
  
  call  mpi_land_init(fullx,blah)
  
  print*, 'numprocs: ',blah%numprocs

  print*, 'full:', blah%my_id,fullx
  
  print*, 'range:', blah%my_id,blah%location_begin,blah%location_end
  
  call mpi_land_finalize()
  
end program
