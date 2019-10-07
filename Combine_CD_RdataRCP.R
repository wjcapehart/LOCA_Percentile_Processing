library(stringr)
library(forcats)
library(readr)
library(tidyverse)
library(lubridate)

directory = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/climate_divisions/"

prefix    = "NGP_LOCA_nCLIMDIV_"
outpref   = "NGP_LOCA_nCLIMDIV_"



csv_files = list.files(path    = str_c(directory,
                                       "/done/",
                                       sep = ""),
                       pattern = "rcp85.RData")

load(file=url("http://kyrill.ias.sdsmt.edu/wjc/eduresources/Climate_Zones_Name_LUT.Rdata"))

Divisions_factor = Climate_Zones_Name_LUT$Full_Zone_Code

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

splitStrings = str_split(string   = csv_files,
                         pattern  = "_",
                         n        = Inf,
                         simplify = TRUE)

Divisions = unique(splitStrings[,4])

division=Divisions[1]

for (division in Divisions)
{
  {

    filename = str_c(directory,
                     "/done/",
                     prefix,
                     division,
                     "_",
                     "historical",
                     sep = "")

    load(file    = str_c(filename,
                         ".RData",
                         sep=""),
         verbose = TRUE)
    loca_hist = loca_daily
    loca_hist[nrow(loca_hist), ]

    filename = str_c(directory,
                     "/done/",
                     prefix,
                     division,
                     "_",
                     "rcp45",
                     sep = "")

    load(file    = str_c(filename,
                         ".RData",
                         sep=""),
         verbose = TRUE)
    loca_45 = loca_daily
    loca_45[nrow(loca_45), ]

    filename = str_c(directory,
                     "/done/",
                     prefix,
                     division,
                     "_",
                     "rcp85",
                     sep = "")

    load(file    = str_c(filename,
                         ".RData",
                         sep=""),
         verbose = TRUE)
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
}

rData_files = intersect(list.files(path    = directory,
                                   pattern = outpref),
                        list.files(path    = directory,
                                   pattern = "_Yearly.RData"))

splitStrings = str_split(string   = rData_files,
                         pattern  = "_",
                         n        = Inf,
                         simplify = TRUE)

Completed_Divisions = unique(splitStrings[,4])



save(Completed_Divisions, file = str_c(directory,
                              "Completed_Divisions",
                              ".RData",
                              sep=""))

print(Completed_Divisions)
