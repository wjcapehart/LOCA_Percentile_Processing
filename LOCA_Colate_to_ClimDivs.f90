program LOCA_Colate_to_ClimDivs

  use netcdf  ! the netcdf module is at /usr/local/netcdf/include/NETCDF.mod
  use omp_lib

  implicit none

  integer, parameter :: nens       =    27
  integer, parameter :: nscen      =     3
  integer, parameter :: nvars      =     3
  integer, parameter :: nlon       =   450
  integer, parameter :: nlat       =   302
  integer, parameter :: ntime_hist = 20454
  integer, parameter :: ntime_futr = 34333

  integer, parameter :: nhucs      =   248
  integer, parameter :: len_hucstr =     4
  integer, parameter :: len_outbuf =   100

  character (len=*), PARAMETER  :: map_variable_name = "US_CAN_Zones"
  character (len=*), PARAMETER  :: map_values_name   = "US_CAN_Zones_ID"
  character (len=*), PARAMETER  :: filename_map      = "./USCAN_Climate_Divisions.nc"
  character (len=*), PARAMETER  :: file_front_root   = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/"
  character (len=*), PARAMETER  :: file_output_root  = &
                    "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climate_divisions/NGP_LOCA_nCLIMDIV_"


  integer (kind=4) :: myhuc_low    = 0001 ! 10170000 (Big Sioux) !  10120000 (Chey)  !  10160000 (James)
  integer (kind=4) :: myhuc_high   = 0999 ! 10170000 (Big Sioux) !  10120000 (Chey)  !  10160000 (James)

  integer, parameter :: npull = 90    ! 2, 3, 7, 487

  integer (kind=4) :: t_buffer

  integer (kind=4), allocatable          :: start_t(:)
  integer (kind=4), allocatable          :: end_t(:)
  integer (kind=4), allocatable          :: span_t(:)

  integer :: e, s, h, t, tt, ntime, huc_counter, n_reads, last_read

  integer (kind=4), dimension(nlon,nlat) :: mask_map
  real    (kind=4), dimension(nlon,nlat) :: masked_variable_map
  integer (kind=4), dimension(nlon,nlat) :: huc_map
  integer (kind=4), dimension(nhucs)     :: hucs

  character (len=090) :: filename_times
  character (len=180) :: filename_pr
  character (len=180) :: filename_tasmax
  character (len=180) :: filename_tasmin
  character (len=180) :: basin_file_name

  integer (kind=4) :: t_in_tt

  integer (kind=2), allocatable :: input_map(:,:,:)
  real    (kind=4), allocatable :: map_pr(:,:,:)
  real    (kind=4), allocatable :: map_tasmax(:,:,:)
  real    (kind=4), allocatable :: map_tasmin(:,:,:)

  real (kind=4), allocatable          :: sort_pr(:)
  real (kind=4), allocatable          :: sort_tasmax(:)
  real (kind=4), allocatable          :: sort_tasmin(:)

  real (kind=4), dimension(nlat*nlon) :: linear_array

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

  integer (kind=4)              :: nmyhucs
  integer (kind=4)              :: num_procs

  integer  (kind=4),         allocatable :: myhucs(:) ! nmyhucs
  integer  (kind=4),         allocatable :: nhuccells(:) !nmyhucs
  integer  (kind=4),         allocatable :: unit_huc(:) !nmyhucs
  character(len=len_outbuf), allocatable :: output_buffer(:) ! span_t,
  character(len=100),        allocatable :: csv_filename(:)   ! \, nmyhucs


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

  INTEGER, DIMENSION(3) :: netcdf_dims_3d_start   !  1, 1, 1 array
  INTEGER, DIMENSION(3) :: netcdf_dims_3d_count   ! NX,NY,NT array

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  first_huc = .TRUE.

  num_procs = omp_get_max_threads()


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


  print*, "Listing Available Climate Divisions"

  huc_counter = 8

  nmyhucs = 0

  ! Pass One - get the nuber of matching hucs
  print*, " "
  print*, "--- Pass One for Polygons ---"
  print*, " "
  do h = 1, nhucs

    if ((hucs(h) .ge. myhuc_low) .and. (hucs(h) .le. myhuc_high)) then

      nmyhucs = nmyhucs + 1

      print*, h, hucs(h)

    end if

  end do


  ! allocate our hucs to be USCAN_Climate_Divisions

  print*, "Allocating myhucs, nhuccells, unit_huc"

  allocate(    myhucs( nmyhucs ) )
  allocate( nhuccells( nmyhucs ) )
  allocate(  unit_huc( nmyhucs ) )

    !Pass 2

  t = 0
  print*, " "
  print*, "--- Pass Two for Polygons ---"
  print*, " "
  do h = 1, nhucs

    if ((hucs(h) .ge. myhuc_low) .and. (hucs(h) .le. myhuc_high)) then

      t = t + 1

      myhucs(t) = hucs(h)
      unit_huc(t) = 8 + t

      mask_map = huc_map

      where( mask_map .eq. hucs(h) )
        mask_map = 1
      elsewhere
        mask_map = 0
      end where

      nhuccells(t) = sum(mask_map)

      write(basin_file_name,'(A, I4.4)') trim(file_output_root), myhucs(t)
      write(*,'("h:",I3.3," u:",I3.3," Div:",I4.4," size:",I8," ",A)') t, &
                                                                       unit_huc(t), &
                                                                       myhucs(t), &
                                                                       nhuccells(t), &
                                                                       trim(basin_file_name)

    end if

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


        print*, "got the calendar"

  ncstat = NF90_CLOSE(netcdf_id_file_dates)
    if(ncstat /= nf90_noerr) call handle_err(ncstat)






          if (nmyhucs .lt. num_procs) then
            call omp_set_num_threads(nmyhucs)
            num_procs = nmyhucs
            print*, "adjusting total number of cores to ",num_procs
          else
            print*, "using default number of cores: ",num_procs
          end if
          print*, "myhucs: ", myhucs


  !!!!!!!!!!!!!!!!!!



  do s = 2, nscen

    print*, "==============================="
    print*, "== "
    print*, "== ", trim(scenarios(s))
    print*, "== "


    if (trim(scenarios(s)) .eq. "historical") then
      ntime = ntime_hist
    else
      ntime = ntime_futr
    end if

    n_reads   = ceiling(real(ntime)/real(npull))   ! number of reads
    last_read = int(mod(real(ntime),real(npull)))  ! last N's

    print*, "==               Number of Time Steps", ntime
    print*, "== Normal Length of Time Record Pull ", npull
    print*, "==                   Number of Pulls ", n_reads
    print*, "==  Length of Final Time Record Pull ", last_read
    print*, "== "
    print*, "== Allocating csv_filename"
    print*, "== "

    allocate(character(100) :: csv_filename(nmyhucs))


    do h = 1, nmyhucs

      write(csv_filename(h),'(A, I4.4,"_",A,".csv")') trim(file_output_root), myhucs(h), trim(scenarios(s))
      write(*,'("h:",I3.3," u:",I3.3," Div:",I4.4," size:",I8," ",A)') h, &
                                                                       unit_huc(h), &
                                                                       myhucs(h), &
                                                                       nhuccells(h), &
                                                                       trim(csv_filename(h))


      open(unit_huc(h), FILE=trim(csv_filename(h)), form="FORMATTED")
      write(unit_huc(h),*) "Time,Division,Ensemble,Scenario,Percentile,tasmax,tasmin,pr"
      close(unit_huc(h))

    end do
    print*, "== "

    print*, "== Allocating span_t, start_t, end_t"
    print*, "== "

    allocate( span_t(n_reads) )
    allocate(start_t(n_reads) )
    allocate(  end_t(n_reads) )

    span_t(1:n_reads-1) = npull
    span_t(n_reads)     = last_read


    start_t(1) = 1
    end_t(1)   = span_t(1)


    do tt = 2, n_reads
      start_t(tt) = start_t(tt-1) + span_t(tt-1)
      end_t(tt)   = end_t(tt-1)   + span_t(tt)
    end do

    print*, "---------------------"

    do e = 1, nens

      print*, "== processing ensemble ", trim(ensembles(e)), ", scenario " , trim(scenarios(s))
      print*, "==  "



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

          ncstat = NF90_GET_ATT(netcdf_id_file_pr,   netcdf_id_pr, "_FillValue",  pr_FillValue)
            if(ncstat /= nf90_noerr) call handle_err(ncstat)

      ncstat = NF90_CLOSE(netcdf_id_file_pr)
        if(ncstat /= nf90_noerr) call handle_err(ncstat)

        print*, "==         PR:",trim(filename_pr)
        print*, "==          scale:",pr_scale_factor
        print*, "==         offset:",pr_add_offset
        print*, "==      FillValue:",pr_FillValue
        print*, "== "


      ncstat = NF90_OPEN(filename_tasmax, NF90_NOWRITE, netcdf_id_file_tasmax)
        if(ncstat /= nf90_noerr) call handle_err(ncstat)

          ncstat = NF90_INQ_VARID(netcdf_id_file_tasmax, trim(tasmax_variable_name), netcdf_id_tasmax)
             if(ncstat /= nf90_noerr) call handle_err(ncstat)

          ncstat = NF90_GET_ATT(netcdf_id_file_tasmax, netcdf_id_tasmax, "scale_factor",  tasmax_scale_factor)
            if(ncstat /= nf90_noerr) call handle_err(ncstat)

          ncstat = NF90_GET_ATT(netcdf_id_file_tasmax, netcdf_id_tasmax, "add_offset",  tasmax_add_offset)
             if(ncstat /= nf90_noerr) call handle_err(ncstat)

          ncstat = NF90_GET_ATT(netcdf_id_file_tasmax,   netcdf_id_tasmax, "_FillValue",  tasmax_FillValue)
             if(ncstat /= nf90_noerr) call handle_err(ncstat)

       ncstat = NF90_CLOSE(netcdf_id_file_tasmax)
         if(ncstat /= nf90_noerr) call handle_err(ncstat)

         print*, "==     TASMAX:",trim(filename_tasmin)
         print*, "==          scale:",tasmax_scale_factor
         print*, "==         offset:",tasmax_add_offset
         print*, "==      FillValue:",tasmax_FillValue
         print*, "== "


      ncstat = NF90_OPEN(filename_tasmin, NF90_NOWRITE, netcdf_id_file_tasmin)
        if(ncstat /= nf90_noerr) call handle_err(ncstat)

          ncstat = NF90_INQ_VARID(netcdf_id_file_tasmin, trim(tasmin_variable_name), netcdf_id_tasmin)
             if(ncstat /= nf90_noerr) call handle_err(ncstat)

          ncstat = NF90_GET_ATT(netcdf_id_file_tasmin, netcdf_id_tasmin, "scale_factor",  tasmin_scale_factor)
            if(ncstat /= nf90_noerr) call handle_err(ncstat)

          ncstat = NF90_GET_ATT(netcdf_id_file_tasmin, netcdf_id_tasmin, "add_offset",  tasmin_add_offset)
             if(ncstat /= nf90_noerr) call handle_err(ncstat)

          ncstat = NF90_GET_ATT(netcdf_id_file_tasmin,   netcdf_id_tasmin, "_FillValue",  tasmin_FillValue)
            if(ncstat /= nf90_noerr) call handle_err(ncstat)

     ncstat = NF90_CLOSE(netcdf_id_file_tasmin)
       if(ncstat /= nf90_noerr) call handle_err(ncstat)

        print*, "==     TASMIN:",trim(filename_tasmin)
        print*, "==          scale:",tasmin_scale_factor
        print*, "==         offset:",tasmin_add_offset
        print*, "==      FillValue:",tasmin_FillValue
        print*, "== "






      do tt = 1, n_reads

        if (trim(scenarios(s)) .eq. "historical") then
          caldate_pull = caldate_hist(start_t(tt))
          caldate_end = caldate_hist(end_t(tt))

        else
          caldate_pull = caldate_futr(start_t(tt))
          caldate_end = caldate_futr(end_t(tt))

        end if

        if ((tt .eq. 1) .or. (tt .eq. n_reads)) then
          print*, "=="
          print*, "Allocating OMP Arrays for large bulk reads in tt loop ", &
                   " (input_map,map_tasmax,map_tasmin,map_pr,output_buffer) ", &
                   tt, n_reads
          print*, "=="

          allocate (                  input_map(nlon, nlat, span_t(tt)) )
          allocate (                  map_tasmax(nlon, nlat, span_t(tt)) )
          allocate (                  map_tasmin(nlon, nlat, span_t(tt)) )
          allocate (                  map_pr(nlon, nlat, span_t(tt)) )
          allocate (character(len_outbuf) :: output_buffer(span_t(tt)*6))

        end if



        write(*,'(A,"  ",A,"   ",A,"_",A," NP:",I2)')  trim(caldate_pull),trim(caldate_end), &
                    trim(ensembles(e)), &
                    trim(scenarios(s)), &
                    num_procs

        netcdf_dims_3d_start   = (/    1,    1, start_t(tt) /)
        netcdf_dims_3d_count   = (/ nlon, nlat,  span_t(tt) /)




        !!!!!!!!!!!!!!!!!!!!!!!!!
        !
        ! Read Precip Block
        !

        ncstat = NF90_OPEN(filename_pr, NF90_NOWRITE, netcdf_id_file_pr)
          if(ncstat /= nf90_noerr) call handle_err(ncstat)

            ncstat = NF90_INQ_VARID(netcdf_id_file_pr, trim(pr_variable_name), netcdf_id_pr)
               if(ncstat /= nf90_noerr) call handle_err(ncstat)

            ncstat = NF90_GET_VAR(netcdf_id_file_pr,   netcdf_id_pr,  input_map,  &
                                  start = netcdf_dims_3d_start, &
                                  count = netcdf_dims_3d_count  )
              if(ncstat /= nf90_noerr) call handle_err(ncstat)


            map_pr = input_map * pr_scale_factor + pr_add_offset
            where (input_map .eq. pr_FillValue) map_pr = pr_FillValue

        ncstat = NF90_CLOSE(netcdf_id_file_pr)
            if(ncstat /= nf90_noerr) call handle_err(ncstat)

        !
        !!!!!!!!!!!!!!!!!!!!!!!!!



        !!!!!!!!!!!!!!!!!!!!!!!!!
        !
        ! Read Tasmax Block
        !

        ncstat = NF90_OPEN(filename_tasmax, NF90_NOWRITE, netcdf_id_file_tasmax)
          if(ncstat /= nf90_noerr) call handle_err(ncstat)

            ncstat = NF90_INQ_VARID(netcdf_id_file_tasmax, trim(tasmax_variable_name), netcdf_id_tasmax)
               if(ncstat /= nf90_noerr) call handle_err(ncstat)


            ncstat = NF90_GET_VAR(netcdf_id_file_tasmax,   netcdf_id_tasmax,  input_map,  &
                                  start = netcdf_dims_3d_start, &
                                  count = netcdf_dims_3d_count  )
              if(ncstat /= nf90_noerr) call handle_err(ncstat)

            map_tasmax = input_map * tasmax_scale_factor + tasmax_add_offset
            where (input_map .eq. tasmax_FillValue) map_tasmax = tasmax_FillValue

        ncstat = NF90_CLOSE(netcdf_id_file_tasmax)
            if(ncstat /= nf90_noerr) call handle_err(ncstat)

        !
        !!!!!!!!!!!!!!!!!!!!!!!!!


        !!!!!!!!!!!!!!!!!!!!!!!!!
        !
        ! Read Tasmin Block
        !

        ncstat = NF90_OPEN(filename_tasmin, NF90_NOWRITE, netcdf_id_file_tasmin)
          if(ncstat /= nf90_noerr) call handle_err(ncstat)

            ncstat = NF90_INQ_VARID(netcdf_id_file_tasmin, trim(tasmin_variable_name), netcdf_id_tasmin)
               if(ncstat /= nf90_noerr) call handle_err(ncstat)


            ncstat = NF90_GET_VAR(netcdf_id_file_tasmin,   netcdf_id_tasmin,  input_map,  &
                                  start = netcdf_dims_3d_start, &
                                  count = netcdf_dims_3d_count  )
              if(ncstat /= nf90_noerr) call handle_err(ncstat)

            map_tasmin = input_map * tasmin_scale_factor + tasmin_add_offset
            where (input_map .eq. tasmin_FillValue) map_tasmin = tasmin_FillValue

        ncstat = NF90_CLOSE(netcdf_id_file_tasmin)
            if(ncstat /= nf90_noerr) call handle_err(ncstat)

        !
        !!!!!!!!!!!!!!!!!!!!!!!!!


!$OMP PARALLEL DO PRIVATE (h,                   &
!$OMP&                     t,                   &
!$OMP&                     linear_array,        &
!$OMP&                     mask_map,            &
!$OMP&                     t_buffer,            &
!$OMP&                     masked_variable_map, &
!$OMP&                     caldate,             &
!$OMP&                     output_buffer,       &
!$OMP&                     sort_tasmax,         &
!$OMP&                     sort_tasmin,         &
!$OMP&                     sort_pr              ), &
!$OMP&             SHARED (e, tt,                  &
!$OMP&                     s,  n_reads,                 &
!$OMP&                     csv_filename,        &
!$OMP&                     ensembles,           &
!$OMP&                     scenarios,           &
!$OMP&                     nhuccells,           &
!$OMP&                     start_t,             &
!$OMP&                     span_t,              &
!$OMP&                     t_in_tt,             &
!$OMP&                     huc_map,             &
!$OMP&                     caldate_hist,        &
!$OMP&                     caldate_futr,        &
!$OMP&                     map_pr,              &
!$OMP&                     map_tasmax,          &
!$OMP&                     map_tasmin,          &
!$OMP&                     pr_FillValue,        &
!$OMP&                     tasmax_FillValue,    &
!$OMP&                     tasmin_FillValue,    &
!$OMP&                     unit_huc,            &
!$OMP&                     num_procs,           &
!$OMP&                     nmyhucs,             &
!$OMP&                     myhucs               ), &
!$OMP&            DEFAULT (NONE)                 , &
!$OMP&           SCHEDULE (STATIC)

        do h = 1, nmyhucs, 1


          t_buffer = 1

          do t = 1,  span_t(tt), 1


            t_in_tt = start_t(tt) + t - 1

            if (trim(scenarios(s)) .eq. "historical") then
              caldate = caldate_hist(t_in_tt)
            else
              caldate = caldate_futr(t_in_tt)
            end if


!print*, "proc:(",omp_get_thread_num(),":",num_procs,") caldat: ",trim(caldate)," HUC:",myhucs(h)

              mask_map = merge(1,0, (huc_map           .eq.        myhucs(h)) .and. &
                                    (map_pr(:,:,t)     .ne.     pr_FillValue) .and. &
                                    (map_tasmax(:,:,t) .ne. tasmax_FillValue) .and. &
                                    (map_tasmin(:,:,t) .ne. tasmin_FillValue)       )

              nhuccells(h) = sum(mask_map)

              !!!!  Allocating sort_tasmax,sort_tasmin,sort_pr in t loop
              allocate ( sort_tasmax(nhuccells(h)) )
              allocate ( sort_tasmin(nhuccells(h)) )
              allocate (     sort_pr(nhuccells(h)) )


              !!! tasmax

                masked_variable_map = map_tasmax(:,:,t)

                where (mask_map .eq. 0) masked_variable_map = tasmin_FillValue

                linear_array = reshape(masked_variable_map, (/ nlon*nlat /))


                call QSort(linear_array, nlon*nlat)

                sort_tasmax(:) = linear_array(nlon*nlat-nhuccells(h)+1:nlon*nlat)

              !!! tasmin

                masked_variable_map = map_tasmin(:,:,t)

                where (mask_map .eq. 0) masked_variable_map = tasmin_FillValue

                linear_array = reshape(masked_variable_map, (/ nlon*nlat /))


                call QSort(linear_array, nlon*nlat)

                sort_tasmin(:) = linear_array(nlon*nlat-nhuccells(h)+1:nlon*nlat)

              !!! pr

                masked_variable_map = map_pr(:,:,t)

                where (mask_map .eq. 0) masked_variable_map = pr_FillValue

                linear_array = reshape(masked_variable_map, (/ nlon*nlat /))


                call QSort(linear_array, nlon*nlat)

                sort_pr(:) = linear_array(nlon*nlat-nhuccells(h)+1:nlon*nlat)


              !!! output


                write(output_buffer(t_buffer),'(A,",",I4.4,3(",",A),3(",",F8.2))')  &
                            trim(caldate), &
                            myhucs(h), &
                            trim(ensembles(e)), &
                            trim(scenarios(s)), &
                            "P000",  &
                            minval(sort_tasmax), minval(sort_tasmin), minval(sort_pr)

                write(output_buffer(t_buffer+1),'(A,",",I4.4,3(",",A),3(",",F8.2))')  &
                            trim(caldate), &
                            myhucs(h), &
                            trim(ensembles(e)), &
                            trim(scenarios(s)), &
                            "P025",  &
                            quantile7(sort_tasmax, 0.25, nhuccells(h)), &
                            quantile7(sort_tasmin, 0.25, nhuccells(h)), &
                            quantile7(sort_pr,     0.25, nhuccells(h))

                write(output_buffer(t_buffer+2),'(A,",",I4.4,3(",",A),3(",",F8.2))')  &
                            trim(caldate), &
                            myhucs(h), &
                            trim(ensembles(e)), &
                            trim(scenarios(s)), &
                            "P050",  &
                            quantile7(sort_tasmax, 0.50, nhuccells(h)), &
                            quantile7(sort_tasmin, 0.50, nhuccells(h)), &
                            quantile7(sort_pr,     0.50, nhuccells(h))

                write(output_buffer(t_buffer+3),'(A,",",I4.4,3(",",A),3(",",F8.2))')   &
                            trim(caldate), &
                            myhucs(h), &
                            trim(ensembles(e)), &
                            trim(scenarios(s)), &
                            "P075",  &
                            quantile7(sort_tasmax, 0.75, nhuccells(h)), &
                            quantile7(sort_tasmin, 0.75, nhuccells(h)), &
                            quantile7(sort_pr,     0.75, nhuccells(h))

                write(output_buffer(t_buffer+4),'(A,",",I4.4,3(",",A),3(",",F8.2))')  &
                            trim(caldate), &
                            myhucs(h), &
                            trim(ensembles(e)), &
                            trim(scenarios(s)), &
                            "P100",  &
                            maxval(sort_tasmax), maxval(sort_tasmin), maxval(sort_pr)

                write(output_buffer(t_buffer+5),'(A,",",I4.4,3(",",A),3(",",F8.2))')  &
                            trim(caldate), &
                            myhucs(h), &
                            trim(ensembles(e)), &
                            trim(scenarios(s)), &
                            "MEAN",  &
                            (/ sum(sort_tasmax), sum(sort_tasmin), sum(sort_pr) /) / nhuccells(h)


              !!!!  De-Allocating sort_tasmax,sort_tasmin,sort_pr in t loop

              deallocate (sort_tasmax)
              deallocate (sort_tasmin)
              deallocate (sort_pr)

              t_buffer = t_buffer + 6

            end do  !!  Internal Time Loop (t)


            open( unit_huc(h), FILE=trim(csv_filename(h)), status="old", position="append", form="formatted", action="write")
            write(unit_huc(h),"(A)") output_buffer(:)
            close(unit_huc(h))



        end do  !! HUCS loop (h)

!$OMP END PARALLEL DO


      if ((tt .eq. n_reads-1)) then
        print*, "=="
        print*, "== De-allocating OMP Arrays for large bulk Reads inside tt loop ",  &
              " (input_map,map_tasmax,map_tasmin,map_pr,output_buffer) ",   &
              tt,n_reads-1, n_reads
        print*, "=="

        deallocate (     input_map )
        deallocate (    map_tasmax )
        deallocate (    map_tasmin )
        deallocate (        map_pr )
        deallocate ( output_buffer )

      end if


  end do  !!  NetCDF Time Loop (tt)

  print*, "=="
  print*, "== De-Allocating OMP Arrays for large bulk Reads Last Pull for end of ensemble ",  &
        "(input_map,map_tasmax,map_tasminmap_pr,output_buffer)"
  print*, "=="

    deallocate (     input_map )
    deallocate (    map_tasmax )
    deallocate (    map_tasmin )
    deallocate (        map_pr )
    deallocate ( output_buffer )


  end do  !! Ensemble Loop (e)

  print*, "== "
  print*, "== De-Allocating arrays at the end of scenario (span_t,start_t,end_t)"
  print*, "== "

  deallocate(span_t)
  deallocate(start_t)
  deallocate(end_t)

  print*, "== "
  print*, "== "
  print*, "==============================="



end do   !! Scenario Loop (s)




!!!!!!!!!!!!!!!!!!

print*, "De-Allocating arrays at the end of program (my hucs,nhuccells,unit_huc,csv_filename)"

deallocate(    myhucs )
deallocate( nhuccells )
deallocate(  unit_huc )
deallocate(csv_filename)

print*, "We're Out of Here Like Vladimir"


end program LOCA_Colate_to_ClimDivs


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


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


real (kind=4) function   quantile7(x,quart,nx)
    implicit none
    ! Excel's method to calculate quartiles.
    ! Based on discussion in this paper http://www.haiweb.org/medicineprices/manual/quartiles_iTSS.pdf
    !
    integer (kind=4), intent(in) :: nx
    real (kind=4), intent (in), dimension(nx)  :: x
    real (kind=4), intent (in) :: quart
    real (kind=4) :: a,b,c
    integer (kind=4) :: n,ib

    n=size(x)

    a=(n-1)*quart
    call getgp(a,b,c)

    ib=int(c)
    !print *,n,a,b,c,ib


    quantile7= (1-b)*x(ib+1) +b*x(ib+2)

    return

end function quantile7


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine getgp(a,b,c)
    ! Subroutine to that returns the Right hand and Left hand side digits of a decimal number
    real (kind=4), intent(in)  :: a
    real (kind=4), intent(out) :: b,c

    b=mod(a,1.0)
    c=a-b

end subroutine getgp


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
