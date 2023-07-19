library(stringr)
library(forcats)
library(readr)
library(tidyverse)
library(lubridate)

directory = "/projects/ECEP/LOCA_MACA_Ensembles/LOCA2/LOCA2_CONUS/Specific_Regional_Aggregate_Sets/NCEI_Climate_Divisions/R_Daily_Files"
work_dir  = "/projects/ECEP/LOCA_MACA_Ensembles/LOCA2/LOCA2_CONUS/Specific_Regional_Aggregate_Sets/NCEI_Climate_Divisions/work"
prefix    = "NGP_LOCA_nCLIMDIV_"
outpref   = "NGP_LOCA_nCLIMDIV_"



csv_files = list.files(path    = str_c(work_dir,
                                       "/",
                                       sep = ""),
                       pattern = "rcp85.RData") 

load(file=url("http://kyrill.ias.sdsmt.edu/wjc/eduresources/Climate_Zones_Name_LUT.Rdata"))

FIPS_CD = Climate_Zones_Name_LUT$Full_Zone_Code

Ensembles = c("ACCESS-CM2", 
              "ACCESS-ESM1-5", 
              "AWI-CM-1-1-MR", 
              "BCC-CSM2-MR", 
              "CanESM5", 
              "CESM2-LENS", 
              "CNRM-CM6-1", 
              "CNRM-CM6-1-HR", 
              "CNRM-ESM2-1", 
              "EC-Earth3", 
              "EC-Earth3-Veg", 
              "FGOALS-g3", 
              "GFDL-CM4", 
              "GFDL-ESM4", 
              "HadGEM3-GC31-LL", 
              "HadGEM3-GC31-MM", 
              "INM-CM4-8", 
              "INM-CM5-0", 
              "IPSL-CM6A-LR", 
              "KACE-1-0-G", 
              "MIROC6", 
              "MPI-ESM1-2-HR", 
              "MPI-ESM1-2-LR", 
              "MRI-ESM2-0", 
              "NorESM2-LM", 
              "NorESM2-MM", 
              "TaiESM1")

members = c("r1i1p1f1", 
            "r1i1p1f2", 
            "r1i1p1f3", 
            "r2i1p1f1", 
            "r2i1p1f3", 
            "r3i1p1f1", 
            "r3i1p1f3", 
            "r4i1p1f1", 
            "r5i1p1f1", 
            "r6i1p1f1", 
            "r7i1p1f1", 
            "r8i1p1f1", 
            "r9i1p1f1", 
            "r10i1p1f1")

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
                     "/work/",
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
                     "/work/",
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
                     "/work/",
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
