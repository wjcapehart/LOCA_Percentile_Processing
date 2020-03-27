library(stringr)
library(forcats)
library(readr)
library(tidyverse)
library(lubridate)

directory = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/Specific_Regional_Aggregate_Sets/huc_08_basins/R_Daily_Files/"

prefix    = "NGP_LOCA_HUCS_"
outpref   = "NGP_LOCA_HUC08_"

csv_files = intersect(list.files(path    = str_c(directory,
                                                 "/work/",
                                                 sep = ""),
                                 pattern = prefix),
                      list.files(path    = str_c(directory,
                                                 "/work/",
                                                 sep = ""),
                                 pattern = "rcp85.RData"))

load(file=url("http://kyrill.ias.sdsmt.edu/wjc/eduresources/HUC08_Missouri_River_Basin.Rdata"))


Divisions_factor = factor(HUC08_MRB_LUT$HUC08_Code_ID)

Ensembles = c("ACCESS1-0_r1i1p1",
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


Divisions = str_sub(string = csv_files,
                    start  = str_length(string = prefix) + 1,
                    end    = str_length(string = prefix) + 8)

print(csv_files)
print(Divisions)

division=Divisions[1]

for (division in Divisions)
{




        filename = str_c(directory,
                         "/work/",
                         prefix,
                         division,
                         "_",
                         "historical",
                         sep = "")

        load(str_c(filename,".RData",sep=""))
        loca_hist = loca_daily
        loca_hist[nrow(loca_hist), ]

        filename = str_c(directory,
                         "/work/",
                         prefix,
                         division,
                         "_",
                         "rcp45",
                         sep = "")

        load(str_c(filename,".RData",sep=""))
        loca_45 = loca_daily
        loca_45[nrow(loca_45), ]

        filename = str_c(directory,
                         "/work/",
                         prefix,
                         division,
                         "_",
                         "rcp85",
                         sep = "")

        load(str_c(filename,".RData",sep=""))
        loca_85 = loca_daily
        loca_85[nrow(loca_85), ]

        loca_daily = rbind(loca_hist,  loca_45)
        loca_daily = rbind(loca_daily, loca_85)

        remove(loca_hist)
        remove(loca_45)
        remove(loca_85)


        last_record = loca_daily[nrow(loca_daily), ]
        last_record


        filename = str_c(directory,
                         outpref,
                         division,
                         sep = "")

        save(loca_daily, file = str_c(filename,
                                      ".RData",
                                      sep=""))





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

          save(loca_yearly, file = str_c(filename,
                                          "_Yearly",
                                          ".RData",
                                          sep=""))





}



prefix    = "NGP_LOCA_HUCS_"
outpref   = "NGP_LOCA_HUC08_"

csv_files = intersect(list.files(path    = str_c(directory,
                                                 "/work/",
                                                 sep = ""),
                                 pattern = prefix),
                      list.files(path    = str_c(directory,
                                                 "/work/",
                                                 sep = ""),
                                 pattern = "rcp85.RData"))

load(file=url("http://kyrill.ias.sdsmt.edu/wjc/eduresources/HUC08_Missouri_River_Basin.Rdata"))


Divisions_factor = factor(HUC08_MRB_LUT$HUC08_Code_ID)

Ensembles = c("ACCESS1-0_r1i1p1",
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


Divisions = str_sub(string = csv_files,
                    start  = str_length(string = prefix) + 1,
                    end    = str_length(string = prefix) + 8)

print(csv_files)
print(Divisions)


rData_files = intersect(list.files(path    = directory,
                                   pattern = outpref),
                        list.files(path    = directory,
                                   pattern = "_Yearly.RData"))



Completed_HUCS = str_sub(string = rData_files,
                              start  = str_length(string = outpref) + 1,
                              end    = str_length(string = outpref) + 8)

save(Completed_HUCS,   file = str_c(directory,
                              "Completed_HUCS",
                              ".RData",
                              sep=""))

print(Completed_HUCS)
