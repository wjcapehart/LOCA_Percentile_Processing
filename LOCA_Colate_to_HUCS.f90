



program LOCA_Colate_to_HUCS

  use netcdf  ! the netcdf module is at /usr/local/netcdf/include/NETCDF.mod

  implicit none


  integer, parameter :: nens       =    27
  integer, parameter :: nscen      =     3
  integer, parameter :: nvars      =     3
  integer, parameter :: nlon       =   450
  integer, parameter :: nlat       =   302
  integer, parameter :: npull      =     1
  integer, parameter :: nhucs      =   307
  integer, parameter :: ntime_hist = 20454
  integer, parameter :: ntime_futr = 34333

  ! hust = 487 :  42 ;
  ! futr = 247 : 139

  integer :: e, s, h, v, t, ntime

  integer (kind=2), dimension(nlon,nlat) :: input_map
  real    (kind=4), dimension(nlon,nlat) :: map_pr, map_tasmax, map_tasmin
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

  real (kind=4), allocatable          :: sort_array(:)
  real (kind=4), dimension(nlat*nlon) :: linear_array

  integer (kind=4), dimension(ntime_hist) :: time_cord_hist
  integer (kind=4), dimension(ntime_futr) :: time_cord_futr

  character (len = 19), dimension(ntime_hist) :: caldate_hist
  character (len = 19), dimension(ntime_futr) :: caldate_futr

  character (len=19)  :: caldate


  character (len=21), dimension(nens)   :: ensembles

  character (len=06), dimension(nvars)  :: variables
  character (len=10), dimension(nscen)  :: scenarios

  character (len = (2+1+21+1+10)) :: pr_variable_name
  character (len = (6+1+21+1+10)) :: tasmax_variable_name
  character (len = (6+1+21+1+10)) :: tasmin_variable_name

  real (kind=4) :: pr_add_offset,   tasmax_add_offset,   tasmin_add_offset
  real (kind=4) :: pr_scale_factor, tasmax_scale_factor, tasmin_scale_factor

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


  !netcdf_dims_3d_count = (/ nlon, nlat, npull /)
  netcdf_dims_3d_count = (/ nlon, nlat, npull /)



  file_front_root = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/"
  ! file_front_root = "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/LOCA_NGP/"

  file_output_root = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/NGP_LOCA_HUC08_"
  file_output_root = "./NGP_LOCA_HUC08_"

  variables = (/ "pr    ", &
                 "tasmax", &
                 "tasmin"  /)

  scenarios = (/ "historical", &
                 "rcp45     ", &
                 "rcp85     " /)



  ensembles = (/ "ACCESS1-0_r1i1p1     ", &
                 "ACCESS1-3_r1i1p1     ", &
                 "CCSM4_r6i1p1         ", &
                 "CESM1-BGC_r1i1p1     ", &
                 "CESM1-CAM5_r1i1p1    ", &
                 "CMCC-CMS_r1i1p1      ", &
                 "CMCC-CM_r1i1p1       ", &
                 "CNRM-CM5_r1i1p1      ", &
                 "CSIRO-Mk3-6-0_r1i1p1 ", &
                 "CanESM2_r1i1p1       ", &
                 "FGOALS-g2_r1i1p1     ", &
                 "GFDL-CM3_r1i1p1      ", &
                 "GFDL-ESM2G_r1i1p1    ", &
                 "GFDL-ESM2M_r1i1p1    ", &
                 "HadGEM2-AO_r1i1p1    ", &
                 "HadGEM2-CC_r1i1p1    ", &
                 "HadGEM2-ES_r1i1p1    ", &
                 "IPSL-CM5A-LR_r1i1p1  ", &
                 "IPSL-CM5A-MR_r1i1p1  ", &
                 "MIROC-ESM-CHEM_r1i1p1", &
                 "MIROC-ESM_r1i1p1     ", &
                 "MIROC5_r1i1p1        ", &
                 "MPI-ESM-LR_r1i1p1    ", &
                 "MPI-ESM-MR_r1i1p1    ", &
                 "MRI-CGCM3_r1i1p1     ", &
                 "NorESM1-M_r1i1p1     ", &
                 "bcc-csm1-1-m_r1i1p1  " /)



!!!!!!!!!!!!!!!!  Get input_map



  filename_map = "./HUC08_Missouri_River_Basin.nc"

  ncstat = NF90_OPEN(filename_map, NF90_NOWRITE, netcdf_id_file_map)
    if(ncstat /= nf90_noerr) call handle_err(ncstat)

  ncstat = NF90_INQ_VARID(netcdf_id_file_map, map_variable_name, netcdf_id_map)
     if(ncstat /= nf90_noerr) call handle_err(ncstat)
  ncstat = NF90_GET_VAR(netcdf_id_file_map,   netcdf_id_map,  huc_map)
     if(ncstat /= nf90_noerr) call handle_err(ncstat)

  ncstat = NF90_INQ_VARID(netcdf_id_file_map, map_values_name,   netcdf_id_hucs)
    if(ncstat /= nf90_noerr) call handle_err(ncstat)

  ncstat = NF90_GET_VAR(netcdf_id_file_map,   netcdf_id_hucs, hucs)
    if(ncstat /= nf90_noerr) call handle_err(ncstat)

  ncstat = NF90_CLOSE(netcdf_id_file_map)
    if(ncstat /= nf90_noerr) call handle_err(ncstat)


  print*, "Listing Available Hucs"
  print*, hucs

  do h = 1, 1 !nhucs

    mask_map = huc_map

    where( mask_map .eq. hucs(h) )
      mask_map = 1
    elsewhere
      mask_map = 0
    end where

    nhuccells(h) = sum(mask_map)



    write(basin_file_name,'(A, i8.8,".csv")') trim(file_output_root), hucs(h)
    print*, hucs(h), nhuccells(h) , trim(basin_file_name)


    !open(1, FILE=trim(basin_file)name), form="FORMATTED", access="APPEND")
    !write(1,*) "Time,Division,Ensemble,Scenario,Percentile,tasmax,tasmin,pr"
    !close (1)

  end do

  !!!!!!!!!!!!!!!!!!


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


  do t = 1, ntime_hist
    print*, time_cord_hist(t), caldate_hist(t)
  end do

  print*, "--"

  do t = 1, ntime_futr
    print*, time_cord_futr(t), caldate_futr(t)
  end do

  !!!!!!!!!!!!!!!!!!


  do s = 1, 1 ! nscen

    if (trim(scenarios(s)) .eq. "historical") then
      ntime = ntime_hist
    else
      ntime = ntime_futr
    end if

    print*, "====================="


    do e = 1, 1 ! nens

      print*, "---------------------"


      pr_variable_name     = "pr_"     // trim(ensembles(e)) // "_" // trim(scenarios(s))
      tasmax_variable_name = "tasmax_" // trim(ensembles(e)) // "_" // trim(scenarios(s))
      tasmin_variable_name = "tasmin_" // trim(ensembles(e)) // "_" // trim(scenarios(s))

      filename_pr     = trim(file_front_root)  // trim(scenarios(s)) //     "/pr/NGP_LOCA_" //     trim(pr_variable_name) // ".nc"
      filename_tasmax = trim(file_front_root)  // trim(scenarios(s)) // "/tasmax/NGP_LOCA_" // trim(tasmax_variable_name) // ".nc"
      filename_tasmin = trim(file_front_root)  // trim(scenarios(s)) // "/tasmin/NGP_LOCA_" // trim(tasmin_variable_name) // ".nc"



      ncstat = NF90_OPEN(filename_pr, NF90_NOWRITE, netcdf_id_file_pr)
        if(ncstat /= nf90_noerr) call handle_err(ncstat)

      ncstat = NF90_INQ_VARID(netcdf_id_file_pr, trim(pr_variable_name), netcdf_id_pr)
         if(ncstat /= nf90_noerr) call handle_err(ncstat)

      ncstat = NF90_GET_ATT(netcdf_id_file_pr,   netcdf_id_pr, "scale_factor",  pr_scale_factor)
        if(ncstat /= nf90_noerr) call handle_err(ncstat)

      ncstat = NF90_GET_ATT(netcdf_id_file_pr,   netcdf_id_pr, "add_offset",  pr_add_offset)
         if(ncstat /= nf90_noerr) call handle_err(ncstat)

      print*, trim(filename_pr), pr_scale_factor, pr_add_offset


      ncstat = NF90_OPEN(filename_tasmax, NF90_NOWRITE, netcdf_id_file_tasmax)
        if(ncstat /= nf90_noerr) call handle_err(ncstat)

      ncstat = NF90_INQ_VARID(netcdf_id_file_tasmax, trim(tasmax_variable_name), netcdf_id_tasmax)
         if(ncstat /= nf90_noerr) call handle_err(ncstat)

      ncstat = NF90_GET_ATT(netcdf_id_file_tasmax, netcdf_id_tasmax, "scale_factor",  tasmax_scale_factor)
        if(ncstat /= nf90_noerr) call handle_err(ncstat)

      ncstat = NF90_GET_ATT(netcdf_id_file_tasmax, netcdf_id_tasmax, "add_offset",  tasmax_add_offset)
         if(ncstat /= nf90_noerr) call handle_err(ncstat)

      print*, trim(filename_tasmax), tasmax_scale_factor, tasmax_add_offset



      ncstat = NF90_OPEN(filename_tasmin, NF90_NOWRITE, netcdf_id_file_tasmin)
        if(ncstat /= nf90_noerr) call handle_err(ncstat)

      ncstat = NF90_INQ_VARID(netcdf_id_file_tasmin, trim(tasmin_variable_name), netcdf_id_tasmin)
         if(ncstat /= nf90_noerr) call handle_err(ncstat)

      ncstat = NF90_GET_ATT(netcdf_id_file_tasmin, netcdf_id_tasmin, "scale_factor",  tasmin_scale_factor)
        if(ncstat /= nf90_noerr) call handle_err(ncstat)

      ncstat = NF90_GET_ATT(netcdf_id_file_tasmin, netcdf_id_tasmin, "add_offset",  tasmin_add_offset)
         if(ncstat /= nf90_noerr) call handle_err(ncstat)

      print*, trim(filename_tasmin), tasmin_scale_factor, tasmin_add_offset






      do t = 1, 1! ntime, npull


        if (trim(scenarios(s)) .eq. "historical") then
          caldate = caldate_hist(t)
        else
          caldate = caldate_futr(t)
        end if

        netcdf_dims_3d_start = (/ 1, 1, t /)

        ncstat = NF90_GET_VAR(netcdf_id_file_pr,   netcdf_id_pr,  input_map,  &
                              start = netcdf_dims_3d_start, &
                              count = netcdf_dims_3d_count  )
          if(ncstat /= nf90_noerr) call handle_err(ncstat)


        map_pr = input_map * pr_scale_factor + pr_add_offset




        ncstat = NF90_GET_VAR(netcdf_id_file_tasmax,   netcdf_id_tasmax,  input_map,  &
                              start = netcdf_dims_3d_start, &
                              count = netcdf_dims_3d_count  )
          if(ncstat /= nf90_noerr) call handle_err(ncstat)

        map_tasmax = input_map * tasmax_scale_factor + tasmax_add_offset

        print*,"input_map",  maxval(input_map)
        print*,"map_tasmax", maxval(map_tasmax)



        ncstat = NF90_GET_VAR(netcdf_id_file_tasmin,   netcdf_id_tasmin,  input_map,  &
                              start = netcdf_dims_3d_start, &
                              count = netcdf_dims_3d_count  )
          if(ncstat /= nf90_noerr) call handle_err(ncstat)

        map_tasmin = input_map * tasmin_scale_factor + tasmin_add_offset






        do h = 1, 1 ! nhucs

          mask_map = huc_map

          where( mask_map .eq. hucs(h) )
            mask_map = 1
          elsewhere
            mask_map = 0
          end where
          print*, "mask", MINVAL(mask_map),MAXVAL(mask_map), sum(mask_map)

          !!! tasmax

          print*,"map_tasmax", minval(map_tasmax),maxval(map_tasmax)

          masked_variable_map =  map_tasmax * merge(1,0,mask_map .eq. hucs(h))

          print*,"merged field " , maxval(merge(1,0,huc_map .eq. hucs(h))), sum(merge(1,0,huc_map .eq. hucs(h)))

          print*, "masked", MAXVAL(masked_variable_map), maxval( map_tasmax * merge(1,0,huc_map.eq.hucs(h)))

          linear_array = reshape(masked_variable_map, (/ nlon*nlat /))

          print*, "sorted", MAXVAL(linear_array)

          call QSort(linear_array, nlon*nlat)

          !write(basin_file_name,'(A, i8.8,".csv")') trim(file_output_root), hucs(h)
          !open(1, FILE=trim(basin_file)name), form="FORMATTED", access="APPEND")
          !write(1,*)  "Time,Division,Ensemble,Scenario,Percentile,tasmax,tasmin,pr"
          ! dclose (1)



        end do

      end do



    end do


  end do



  !!!!!!!!!!!!!!!!!!


end program LOCA_Colate_to_HUCS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine handle_err(status)
  USE netcdf  ! the netcdf module is at /usr/local/netcdf/include/NETCDF.mod

  integer, intent ( in) :: status
  if(status /= nf90_noerr) then
    print *, trim(nf90_strerror(status))
    stop "Stopped"
  end if
end subroutine handle_err


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
recursive subroutine QSort(A,nA)

! DUMMY ARGUMENTS
integer (kind=4), intent(in) :: nA
real    (kind=4), dimension(nA), intent(in out) :: A

! LOCAL VARIABLES
integer (kind=4) :: left, right
real    (kind=4) :: random
real    (kind=4) :: pivot
real    (kind=4) :: temp
integer (kind=4) :: marker

    if (nA > 1) then

        call random_number(random)
        pivot = A(int(random*real(nA-1))+1)  ! random pivor (not best performance, but avoids worst-case)
        left = 0
        right = nA + 1

        do while (left < right)
            right = right - 1
            do while (A(right) > pivot)
                right = right - 1
            end do
            left = left + 1
            do while (A(left) < pivot)
                left = left + 1
            end do
            if (left < right) then
                temp = A(left)
                A(left) = A(right)
                A(right) = temp
            end if
        end do

        if (left == right) then
            marker = left + 1
        else
            marker = left
        end if

        call QSort(A(:marker-1),marker-1)
        call QSort(A(marker:),nA-marker+1)

    end if

end subroutine QSort
