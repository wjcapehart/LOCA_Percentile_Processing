
###################################################
##
## Libraries
##

library(package = "tidyverse")

library(package = "stringr")
library(package = "ncdf4")
library(package = "ncdf4.helpers")

library(package = "lubridate") # processing dates andTime
library(package = "reshape2")  # manipulating data frames

##
##  print(as.Date( sub("\uFEFF", "", x$Time)))
####################################################

lozone = 3000
hizone  = 3899

Scenario = c("rcp45",
             "rcp85")
             
###################################################
##
## IO URLs
##

root_input_URL  = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/"

root_output_URL = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climate_divisions/"

##
##
####################################################


###################################################
##
## LOCA Scenarios, Variables, and Ensemble
##



  Variable = c("tasmax",
               "tasmin",
               "pr")


  Ensemble       =  c("ACCESS1-0_r1i1p1",
                      "ACCESS1-3_r1i1p1",
                      "CCSM4_r6i1p1",
                      "CESM1-BGC_r1i1p1",
                      "CESM1-CAM5_r1i1p1",
                      "CMCC-CMS_r1i1p1",
                      "CMCC-CM_r1i1p1",
                      "CNRM-CM5_r1i1p1",
                      "CSIRO-Mk3-6-0_r1i1p1",
                      "CanESM2_r1i1p1",
                      "FGOALS-g2_r1i1p1",
                      "GFDL-CM3_r1i1p1",
                      "GFDL-ESM2G_r1i1p1",
                      "GFDL-ESM2M_r1i1p1",
                      "HadGEM2-AO_r1i1p1",
                      "HadGEM2-CC_r1i1p1",
                      "HadGEM2-ES_r1i1p1",
                      "IPSL-CM5A-LR_r1i1p1",
                      "IPSL-CM5A-MR_r1i1p1",
                      "MIROC-ESM_r1i1p1",
                      "MIROC-ESM-CHEM_r1i1p1",
                      "MIROC5_r1i1p1",
                      "MPI-ESM-LR_r1i1p1",
                      "MPI-ESM-MR_r1i1p1",
                      "MRI-CGCM3_r1i1p1",
                      "NorESM1-M_r1i1p1",
                      "bcc-csm1-1-m_r1i1p1")

##
##
####################################################



###################################################
##
## Pull Climate Divisions
##


  ncngp  = nc_open(filename = "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/CLASS_Examples/NGP_Climate_Zones.nc")

  lon         =  ncvar_get(nc    = ncngp,
                           varid = "lon")
  lat         =  ncvar_get(nc    = ncngp,
                           varid = "lat")

  climate_regions  =  array(data     = NA,
                            dim      = c(length(lon),
                                         length(lat)),
                            dimnames = list("lon" = lon,
                                            "lat" = lat))

  climate_regions[,]    = ncvar_get(nc    = ncngp,
                                    varid = "US_CAN_Zones")

  climate_regions = t(apply(X      = climate_regions,
                            MARGIN = 1,
                            FUN    = rev))
  nc_close(nc = ncngp)

  remove(ncngp)

  Climate_Zones = as.numeric(levels(unique(as.factor(climate_regions))))


  Climate_Zones = Climate_Zones[Climate_Zones>lozone]
  Climate_Zones = Climate_Zones[Climate_Zones<hizone]


  climate_regions_table = melt(data       = climate_regions,
                               value.name = "Climate_Zone")

  climate_regions_table$Climate_Zone = as.factor(climate_regions_table$Climate_Zone)



##
##
####################################################






###################################################
##
## ExtractTime Series
##

  scenario = Scenario[1]
  for (scenario in Scenario)
  {
    first    = TRUE
      ensemble = Ensemble[1]
      for (ensemble in Ensemble)
      {

        variable_name_tasmax = str_c("tasmax",
                                     "_",
                                     ensemble,
                                     "_",
                                     scenario,
                                     sep = "")

        variable_name_tasmin = str_c("tasmin",
                                     "_",
                                     ensemble,
                                     "_",
                                     scenario,
                                     sep = "")

        variable_name_pr     = str_c("pr",
                                     "_",
                                     ensemble,
                                     "_",
                                     scenario,
                                     sep = "")

        print(variable_name_tasmax)
        print(variable_name_tasmin)
        print(variable_name_pr)



        tasmax_URL =  str_c(root_input_URL,
                            scenario,
                            "/",
                            "tasmax",
                            "/",
                            "NGP_LOCA_",
                            variable_name_tasmax,
                            ".nc",
                            sep = "")

        tasmin_URL =  str_c(root_input_URL,
                            scenario,
                            "/",
                            "tasmin",
                            "/",
                            "NGP_LOCA_",
                            variable_name_tasmin,
                            ".nc",
                            sep = "")

        pr_URL     =  str_c(root_input_URL,
                            scenario,
                            "/",
                            "pr",
                            "/",
                            "NGP_LOCA_",
                            variable_name_pr,
                            ".nc",
                            sep = "")


        nc_tasmax = nc_open(filename = tasmax_URL)
        nc_tasmin = nc_open(filename = tasmin_URL)
        nc_pr     = nc_open(filename =     pr_URL)

        if (first) {

          Time = nc.get.time.series(f             =  nc_tasmax, # netCDF file handle
                                    time.dim.name =     "time") # netCDF time coordinate name

          Time = as.POSIXct(Time)

          year = year(Time)

          lat = ncvar_get(nc    = nc_tasmax,
                          varid =   "lat")
          lon = ncvar_get(nc    = nc_tasmax,
                          varid =   "lon")
          first = FALSE
        }

          for (k in seq(from = 1,
                        to   = length(Time),
                        by   = 1))
          {

            input_2d_tasmax = ncvar_get(nc    = nc_tasmax,
                                        varid = variable_name_tasmax,
                                        start = c(1,                     1, k),
                                        count = c(length(lon), length(lat), 1))

            input_2d_tasmin = ncvar_get(nc    = nc_tasmin,
                                        varid = variable_name_tasmin,
                                        start = c(1,                     1, k),
                                        count = c(length(lon), length(lat), 1))

            input_2d_pr     = ncvar_get(nc    = nc_pr,
                                        varid = variable_name_pr,
                                        start = c(1,                     1, k),
                                        count = c(length(lon), length(lat), 1))

            cli_div = Climate_Zones[1]
            for (cli_div in Climate_Zones)
            {

              output_URL = str_c(root_output_URL,
                                 "NGP_LOCA_",
                                 "nCLIMDIV_",
                                 sprintf("%04d",
                                         cli_div),
                                 "_",
                                 scenario,
                                 ".csv",
                                 sep = "")


              if ( (k        ==           1) &
                   (ensemble == Ensemble[1]) ) {
                 fileConn = file(description = output_URL)
                writeLines(text = "Time,Division,Ensemble,Scenario,Percentile,tasmax,tasmin,pr",
                           con  = fileConn)
                close(con = fileConn)

              }



              mask = climate_regions
              mask[] = NA

              mask[climate_regions == cli_div] = 1

              mask_tasmax = mask * input_2d_tasmax
              mask_tasmin = mask * input_2d_tasmin
              mask_pr     = mask * input_2d_pr

              mask_tasmax = quantile(x     = mask_tasmax,
                                     na.rm = TRUE)

              mask_tasmin = quantile(x     = mask_tasmin,
                                       na.rm = TRUE)

              mask_pr     = quantile(x     = mask_pr,
                                       na.rm = TRUE)




              mask_tasmax = tibble(Time     =Time[k],
                                   Division = sprintf("%04d",cli_div),
                                   Ensemble = ensemble,
                                   Scenario = scenario,
                                   P000     = round(mask_tasmax[1],1),
                                   P025     = round(mask_tasmax[2],1),
                                   P050     = round(mask_tasmax[3],1),
                                   P075     = round(mask_tasmax[4],1),
                                   P100     = round(mask_tasmax[5],1))

              mask_tasmax = gather(data = mask_tasmax,
                                   value = "tasmax",
                                   key   = "Percentile",
                                   "P000",
                                   "P025",
                                   "P050",
                                   "P075",
                                   "P100")




              mask_tasmin = tibble(Time     =Time[k],
                                   Division = sprintf("%04d",cli_div),
                                   Ensemble = ensemble,
                                   Scenario = scenario,
                                   P000     = round(mask_tasmin[1],1),
                                   P025     = round(mask_tasmin[2],1),
                                   P050     = round(mask_tasmin[3],1),
                                   P075     = round(mask_tasmin[4],1),
                                   P100     = round(mask_tasmin[5],1))

              mask_tasmin = gather(data = mask_tasmin,
                                   value = "tasmin",
                                   key   = "Percentile",
                                   "P000",
                                   "P025",
                                   "P050",
                                   "P075",
                                   "P100")




              mask_pr     = tibble(Time     =Time[k],
                                   Division = sprintf("%04d",cli_div),
                                   Ensemble = ensemble,
                                   Scenario = scenario,
                                   P000     = round(mask_pr[1],1),
                                   P025     = round(mask_pr[2],1),
                                   P050     = round(mask_pr[3],1),
                                   P075     = round(mask_pr[4],1),
                                   P100     = round(mask_pr[5],1))

              mask_pr     = gather(data = mask_pr,
                                   value = "pr",
                                   key   = "Percentile",
                                   "P000",
                                   "P025",
                                   "P050",
                                   "P075",
                                   "P100")



              mask_data = left_join(x = mask_tasmax,
                                    y = mask_tasmin,
                                    by = c("Time",
                                           "Division",
                                           "Ensemble",
                                           "Scenario",
                                           "Percentile"))


              mask_data = left_join(x = mask_data,
                                    y = mask_pr,
                                    by = c("Time",
                                           "Division",
                                           "Ensemble",
                                           "Scenario",
                                           "Percentile"))

              write_excel_csv(x      = mask_data,
                        path   = output_URL,
                        append = TRUE)

              # pro tip # awk '{ gsub(/\xef\xbb\xbf/,""); print }' INFILE > OUTFILE


            } # cli_div



          }  # day

        nc_close(nc_pr)
        nc_close(nc_tasmin)
        nc_close(nc_tasmax)


      } # ensemble
  } # scenario

##
####################################################
