

# Libraries

  # Tidyverse resources

  library(package = "tidyverse") # Multiple Tidyverse Resources
  library(package = "lubridate") # Date-Time Control

  # NCAR Libraries




# Pulling Hucs for Zone Lookup Tables

  HUC_LUT_URL = "http://kyrill.ias.sdsmt.edu/wjc/eduresources/HUC08_Missouri_River_Basin.Rdata"

  load(file = url(HUC_LUT_URL), verbose=TRUE)


  load(file=url("http://kyrill.ias.sdsmt.edu/wjc/eduresources/HUC08_Missouri_River_Basin.Rdata"))


  Divisions_factor = factor(HUC08_MRB_LUT$HUC08_Code_ID)



  Ensembles2 = c("ACCESS1-0",
                "ACCESS1-3",
                "CCSM4",
                "CESM1-BGC",
                "CESM1-CAM5",
                "CMCC-CMS",
                "CMCC-CM",
                "CNRM-CM5",
                "CSIRO-Mk3-6-0",
                "CanESM2",
                "FGOALS-g2",
                "GFDL-CM3",
                "GFDL-ESM2G",
                "GFDL-ESM2M",
                "HadGEM2-AO",
                "HadGEM2-CC",
                "HadGEM2-ES",
                "IPSL-CM5A-LR",
                "IPSL-CM5A-MR",
                "MIROC-ESM",
                "MIROC-ESM-CHEM",
                "MIROC5",
                "MPI-ESM-LR",
                "MPI-ESM-MR",
                "MRI-CGCM3",
                "NorESM1-M",
                "bcc-csm1-1-m")





  Completed_HUCS = HUC08_MRB_LUT$HUC08_Code_ID




huc_zone_lut = Completed_HUCS[1]

root_LOCA_URL = "/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_NGP/Specific_Regional_Aggregate_Sets/huc_08_basins/"


for (huc_zone_lut in Completed_HUCS)
{  # huc

  print(str_c("processing ",huc_zone_lut))




  loca_location_data = HUC08_MRB_LUT %>%
    filter(HUC08_Code_ID == huc_zone_lut)



  loca_filename = str_c("NGP_LOCA_HUC08_",
                        huc_zone_lut,
                        ".RData",
                        sep = "")




  LOCA_URL = str_c(root_LOCA_URL, "R_Daily_Files/",
                   loca_filename,
                   sep = "")

  print(LOCA_URL)

  load(file    = LOCA_URL,
       verbose = TRUE)

  levels(loca_daily$Ensemble) = Ensembles2


 LOCA_URL = str_c(root_LOCA_URL,
                  loca_filename,
                  sep = "")

  save(loca_daily, file    = LOCA_URL)



  filename = str_c(root_LOCA_URL,
                    "NGP_LOCA_HUC08_",
                        huc_zone_lut,
                   sep = "")




            loca_monthly = loca_daily %>%
              mutate(Time  = as.Date(str_c(year(Time),
                                           month(Time),
                                           "15",
                                           sep="-"),
                                     tryFormats = c("%Y-%m-%d")),
                     tasavg = (tasmin + tasmax)/2)   %>%
              group_by(Time,
                       Division,
                       Ensemble,
                       Scenario,
                       Percentile) %>%
              summarize(tasmax = mean(tasmax),
                        tasavg = mean(tasavg),
                        tasmin = mean(tasmin),
                        pr     = sum(pr))

              print(str_c(filename,  "_Monthly",
                                              ".RData",
                                              sep=""))

            save(loca_monthly, file = str_c(filename,
                                            "_Monthly",
                                            ".RData",
                                            sep=""))

            loca_yearly = loca_daily %>%
              mutate(Year  = year(Time),
                     tasavg = (tasmin + tasmax)/2)   %>%
              group_by(Year,
                       Division,
                       Ensemble,
                       Scenario,
                       Percentile) %>%
              summarize(tasmax = mean(tasmax),
                        tasavg = mean(tasavg),
                        tasmin = mean(tasmin),
                        pr     = sum(pr))

            print(str_c(filename,"_Yearly",
                                            ".RData",
                                            sep=""))

            save(loca_yearly, file = str_c(filename,
                                            "_Yearly",
                                            ".RData",
                                            sep=""))



} # huc



print("Outahere like Vladimir")
