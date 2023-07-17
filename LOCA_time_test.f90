



program LOCA_time_test

  use netcdf  ! the netcdf module is at /usr/local/netcdf/include/NETCDF.mod
  implicit none


  integer, parameter :: nens       =    27
  integer, parameter :: nscen      =     3
  integer, parameter :: nvars      =     3
  integer, parameter :: nlon       =   450
  integer, parameter :: nlat       =   302
  integer, parameter :: nhucs      =   307
  integer, parameter :: ntime_hist = 20454
  integer, parameter :: ntime_futr = 34333

  integer (kind=4), allocatable          :: start_t(:)
  integer (kind=4), allocatable          :: end_t(:)
  integer (kind=4), allocatable          :: span_t(:)


  integer (kind=4) :: myhuc_low   = 10120000 ! 10170000 (Big Sioux) !  10120000 (Chey)  !  10160000 (James)
  integer (kind=4) :: myhuc_high  = 10129999 ! 10170000 (Big Sioux) !  10120000 (Chey)  !  10160000 (James)


  integer :: e, s, h, v, t, tt, ntime, huc_counter, n_reads, last_read

  integer (kind=4), dimension(nlon,nlat) :: mask_map
  real    (kind=4), dimension(nlon,nlat) :: masked_variable_map
  integer (kind=4), dimension(nlon,nlat) :: huc_map
  integer (kind=4), dimension(nhucs)     :: hucs

  character (len=090) :: file_front_root
  character (len=090) :: filename_map
  character (len=090) :: filename_times
  character (len=180) :: filename_pr
  character (len=180) :: filename_tasmax
  character (len=180) :: filename_tasmin
  character (len=090) :: file_output_root
  character (len=180) :: basin_file_name

  integer (kind=4), dimension(nhucs) :: unit_huc

  integer (kind=4) :: npull, t_in_tt


  integer (kind=2), allocatable :: input_map(:,:,:)
  real    (kind=4), allocatable :: map_pr(:,:,:)
  real    (kind=4), allocatable :: map_tasmax(:,:,:)
  real    (kind=4), allocatable :: map_tasmin(:,:,:)


  real (kind=4), allocatable          :: sort_pr(:)
  real (kind=4), allocatable          :: sort_tasmax(:)
  real (kind=4), allocatable          :: sort_tasmin(:)


  real (kind=4), dimension(nlat*nlon) :: linear_array, mask_linear

  integer (kind=4), dimension(ntime_hist) :: time_cord_hist
  integer (kind=4), dimension(ntime_futr) :: time_cord_futr

  character (len = 19), dimension(ntime_hist) :: caldate_hist
  character (len = 19), dimension(ntime_futr) :: caldate_futr

  character (len=19)  :: caldate, caldate_pull, caldate_end

  logical :: first_huc


  character (len=21), dimension(nens)   :: ensembles

  character (len=06), dimension(nvars)  :: variables
  character (len=10), dimension(nscen)  :: scenarios

  character (len = (2+1+21+1+10)) :: pr_variable_name
  character (len = (6+1+21+1+10)) :: tasmax_variable_name
  character (len = (6+1+21+1+10)) :: tasmin_variable_name

  real (kind=4) :: pr_add_offset,   tasmax_add_offset,   tasmin_add_offset
  real (kind=4) :: pr_scale_factor, tasmax_scale_factor, tasmin_scale_factor
  real (kind=4) :: pr_FillValue,    tasmax_FillValue,    tasmin_FillValue

  real (kind=4) :: quantile7

  integer (kind=4), dimension(nhucs) :: nhuccells

  character (len=*), PARAMETER  :: map_variable_name = "HUC08_Code"
  character (len=*), PARAMETER  :: map_values_name   = "HUC08_Code_ID"

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! NetCDF Identifiers

  integer :: ncstat             ! generic netcdf status return variable


  INTEGER :: netcdf_id_file_dates     ! netcdf file id
  INTEGER :: netcdf_id_file_map     ! netcdf file id
  INTEGER :: netcdf_id_file_tasmax    ! netcdf file id
  INTEGER :: netcdf_id_file_tasmin   ! netcdf file id
  INTEGER :: netcdf_id_file_pr     ! netcdf file id

  INTEGER :: netcdf_id_time_hist      ! netcdf lon variable ID
  INTEGER :: netcdf_id_time_futr     ! netcdf lon variable ID
  INTEGER :: netcdf_id_cal_hist     ! netcdf lon variable ID
  INTEGER :: netcdf_id_cal_futr     ! netcdf lon variable ID


  INTEGER :: netcdf_id_map      ! netcdf lon variable ID
  INTEGER :: netcdf_id_hucs      ! netcdf lat variable ID
  INTEGER :: netcdf_id_pr     ! netcdf pres variable ID
  INTEGER :: netcdf_id_tasmax     ! netcdf time variable ID
  INTEGER :: netcdf_id_tasmin     ! netcdf pres variable ID






  INTEGER, DIMENSION(3) :: netcdf_dims_3d         ! NX,NY,NT netcdf dim macro
  INTEGER, DIMENSION(3) :: netcdf_dims_3d_start   !  1, 1, 1 array
  INTEGER, DIMENSION(3) :: netcdf_dims_3d_count   ! NX,NY,NT array

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  first_huc = .TRUE.




  file_front_root = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/"

  file_output_root = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/huc_08_basins/NGP_LOCA_HUC08_"

  variables = (/ "pr    ", &
                 "tasmax", &
                 "tasmin"  /)

  scenarios = (/ "historical", &
                 "rcp45     ", &
                 "rcp85     " /)





!!!!!!!!!!!!!!!!  Get input_map




  filename_times  = "./LOCA_Calendar_Lookup_Table.nc"

  ncstat = NF90_OPEN(filename_times, NF90_NOWRITE, netcdf_id_file_dates)
    if(ncstat /= nf90_noerr) call handle_err(ncstat)


  ncstat = NF90_INQ_VARID(netcdf_id_file_dates, "time_hist", netcdf_id_time_hist)
     if(ncstat /= nf90_noerr) call handle_err(ncstat)
  ncstat = NF90_GET_VAR(netcdf_id_file_dates,  netcdf_id_time_hist, time_cord_hist)
     if(ncstat /= nf90_noerr) call handle_err(ncstat)


  ncstat = NF90_INQ_VARID(netcdf_id_file_dates, "time_futr", netcdf_id_time_futr)
    if(ncstat /= nf90_noerr) call handle_err(ncstat)
  ncstat = NF90_GET_VAR(netcdf_id_file_dates, netcdf_id_time_futr, time_cord_futr)
    if(ncstat /= nf90_noerr) call handle_err(ncstat)

print*, "got the times"

  ncstat = NF90_INQ_VARID(netcdf_id_file_dates, "calendar_date_hist", netcdf_id_cal_hist)
    if(ncstat /= nf90_noerr) call handle_err(ncstat)
  ncstat = NF90_GET_VAR(netcdf_id_file_dates, netcdf_id_cal_hist, caldate_hist)
    if(ncstat /= nf90_noerr) call handle_err(ncstat)


  ncstat = NF90_INQ_VARID(netcdf_id_file_dates, "calendar_date_futr", netcdf_id_cal_futr)
    if(ncstat /= nf90_noerr) call handle_err(ncstat)
  ncstat = NF90_GET_VAR(netcdf_id_file_dates, netcdf_id_cal_futr, caldate_futr)
    if(ncstat /= nf90_noerr) call handle_err(ncstat)


    print*, "got the celandar"

  ncstat = NF90_CLOSE(netcdf_id_file_dates)
    if(ncstat /= nf90_noerr) call handle_err(ncstat)








  !!!!!!!!!!!!!!!!!!


  do s = 1,  nscen
    npull = 50           ! 2, 3, 7, 487

    if (trim(scenarios(s)) .eq. "historical") then
      ntime = ntime_hist
    else
      ntime = ntime_futr
    end if

    n_reads   = ceiling(real(ntime)/real(npull))   ! number of reads
    last_read = int(mod(real(ntime),real(npull)))  ! last N's

    print*, scenarios(s)," ", ntime, " ", npull, " ", n_reads, last_read



    allocate( span_t(n_reads) )
    allocate(start_t(n_reads) )
    allocate(  end_t(n_reads) )

    span_t(1:n_reads-1) = npull
    span_t(n_reads)     = last_read

    print*, sum(span_t)

    start_t(1) = 1
    end_t(1)   = span_t(1)


    do tt = 2, n_reads
      start_t(tt) = start_t(tt-1) + span_t(tt-1)
      end_t(tt)   = end_t(tt-1)   + span_t(tt)
    end do



    print*, (start_t(2:n_reads) - start_t(1:n_reads-1)  )






            do tt = 1, n_reads

              if (trim(scenarios(s)) .eq. "historical") then
                caldate_pull = caldate_hist(start_t(tt))
                caldate_end = caldate_hist(end_t(tt))

              else
                caldate_pull = caldate_futr(start_t(tt))
                caldate_end = caldate_futr(end_t(tt))

              end if

              write(*,'(3i6.5,"|",i6.5,2(x,A))')  tt, start_t(tt), end_t(tt),ntime, caldate_pull,caldate_end

              netcdf_dims_3d_start   = (/    1,    1,     start_t(tt) /)
              netcdf_dims_3d_count   = (/    1,    1,     span_t(tt) /)


            end do

    deallocate(span_t)
    deallocate(start_t)
    deallocate(end_t)

  end do




end program LOCA_time_test



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



subroutine handle_err(status)
  USE netcdf  ! the netcdf module is at /usr/local/netcdf/include/NETCDF.mod

  integer, intent ( in) :: status
  if(status /= nf90_noerr) then
    print *, trim(nf90_strerror(status))
    stop "Stopped"
  end if
end subroutine handle_err


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
