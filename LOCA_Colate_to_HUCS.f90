program LOCA_Colate_to_HUCS


  USE netcdf  ! the netcdf module is at /usr/local/netcdf/include/NETCDF.mod



  integer, parameter :: nens       =    27
  integer, parameter :: nlon       =   450
  integer, parameter :: nlat       =   302
  integer, parameter :: nyear      =   365
  integer, parameter :: ntime_futr = 34333
  integer, parameter :: ntime_hist = 20454


  integer :: i, j, k, m, n, ntime

  integer (kind=2), allocatable :: in_array(:,:,:)
  integer (kind=4), allocatable :: input_map(:,:)

  character (len=*) :: file_front_root
  character (len=*) :: full_file_string


  character (len=*), dimension(nens) :: ensembles

  character (len=*), dimension(3)   :: variable_string
  character (len=*), dimension(3)   :: scenario_string

  real    :: pr_add_offset,   tasmax_add_offset,   tasmin_add_offset
  real    :: pr_scale_factor, tasmax_scale_factor, tasmin_scale_factor

  nens = 27

  nyear = 365

  variable_string = (/ "pr",     \
                       "tasmax", \
                       "tasmin" /)

  scenario_string = (/ "historical", \
                       "rcp45",      \
                       "rcp85"      /)

  file_front_root = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/historical/"

  ensembles = (/ "ACCESS1-0_r1i1p1",      \
                 "ACCESS1-3_r1i1p1",      \
                 "CCSM4_r6i1p1",          \
                 "CESM1-BGC_r1i1p1",      \
                 "CESM1-CAM5_r1i1p1",     \
                 "CMCC-CMS_r1i1p1",       \
                 "CMCC-CM_r1i1p1",        \
                 "CNRM-CM5_r1i1p1",       \
                 "CSIRO-Mk3-6-0_r1i1p1",  \
                 "CanESM2_r1i1p1",        \
                 "FGOALS-g2_r1i1p1",      \
                 "GFDL-CM3_r1i1p1",       \
                 "GFDL-ESM2G_r1i1p1",     \
                 "GFDL-ESM2M_r1i1p1",     \
                 "HadGEM2-AO_r1i1p1",     \
                 "HadGEM2-CC_r1i1p1",     \
                 "HadGEM2-ES_r1i1p1",     \
                 "IPSL-CM5A-LR_r1i1p1",   \
                 "IPSL-CM5A-MR_r1i1p1",   \
                 "MIROC-ESM-CHEM_r1i1p1", \
                 "MIROC-ESM_r1i1p1",      \
                 "MIROC5_r1i1p1",         \
                 "MPI-ESM-LR_r1i1p1",     \
                 "MPI-ESM-MR_r1i1p1",     \
                 "MRI-CGCM3_r1i1p1",      \
                 "NorESM1-M_r1i1p1",      \
                 "bcc-csm1-1-m_r1i1p1"    \)



!!!!!!!!!!!!!!!!  Get input_map


  ALLOCATE( input_map(nlon, nlat) )

  full_file_string = ""

!!!!!!!!!!!!!!!!!!



end program LOCA_Colate_to_HUCS
