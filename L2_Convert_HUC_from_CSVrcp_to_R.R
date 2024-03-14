library(stringr)
library(forcats)
library(readr)
library(tidyverse)
library(lubridate)
library(labelled)

directory     = "/data/DATASETS/LOCA_MACA_Ensembles/LOCA2/LOCA2_CONUS/Specific_Regional_Aggregate_Sets/USGS_HUC08_Basins/R_Daily_Files/work/"
out_directory = "/data/DATASETS/LOCA_MACA_Ensembles/LOCA2/LOCA2_CONUS/Specific_Regional_Aggregate_Sets/USGS_HUC08_Basins/R_Daily_Files/"

prefix    = "LOCA2_V1_HUC08_"
prefix    = "LOCA2_V1_HUC08_" 

csv_files = intersect(list.files(path    = directory,
                                 pattern = prefix),
                      list.files(path    = directory,
                                 pattern = "csv"))

csv_files  = str_remove(csv_files, ".csv")

csv_files = str_c(directory,csv_files,sep="")

print(csv_files)


models = c("ACCESS-CM2", 
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


load("./NCEI_ClimDivs.RData")



FIPS_CD = NCEI_ClimDivs$FIPS_CD



models_factor = factor(models)
members_factor = factor(members)



for (filename in csv_files)
{

  print(str_c("Begin Processing ",filename))
  command = str_c("gunzip -v ",
                  filename,
                  ".csv.gz",
                  sep = "")

  system(command)

        loca2_daily = read_csv(str_c(filename,
                                    ".csv",
                                    sep=""),
                      progress = show_progress())



        print(loca2_daily$Division[1])
        if (is.numeric(loca2_daily$Division[1]))
        {
          loca2_daily$Division = as.character(sprintf("%04d",loca2_daily$Division))
        }
        
        loca2_daily$Division   = factor(x    = loca2_daily$Division,
                                        levels = FIPS_CD)     
        
        loca2_daily$Time       = as.Date( sub("\uFEFF", "", loca2_daily$Time))
        
        
        loca2_daily = loca2_daily %>%
          mutate(Scenario = case_when(Scenario == "historical"  ~ "Historical",
                                      Scenario == "ssp245"      ~ "SSP2-4.5",
                                      Scenario == "ssp370"      ~ "SSP3-7.0",
                                      Scenario == "ssp585"      ~ "SSP5-8.5"))


        loca2_daily$Scenario   = factor(x      = loca2_daily$Scenario,
                                       levels = c("Historical",
                                                  "SSP2-4.5",
                                                  "SSP3-7.0",
                                                  "SSP5-8.5"))



        loca2_daily$Model   = factor(x      = loca2_daily$Model,
                                       levels = models)
        
        loca2_daily$Member   = factor(x      = loca2_daily$Member,
                                    levels = members)
        
        loca2_daily$Percentile = factor(x      = loca2_daily$Percentile,
                                       levels = c("P000",
                                                  "P025",
                                                  "P050",
                                                  "P075",
                                                  "P100",
                                                  "MEAN"))
        
        loca2_daily$tasmax = as.single(loca2_daily$tasmax)
        loca2_daily$tasmin = as.single(loca2_daily$tasmin)
        loca2_daily$pr     = as.single(loca2_daily$pr)
        
        last_record = loca2_daily[nrow(loca2_daily), ]
        print(last_record)



        loca2_daily = remove_attributes(x          = loca2_daily, 
                                        attributes = "Csingle") %>%
                      arrange(Division,
                      Scenario,
                      Model,
                      Member,
                      Percentile,
                      Time)

                                    


   

        save(loca2_daily,
             file = str_c(filename,
                          ".RData",
                          sep=""))





  command = str_c("gzip -9fv ",
                  filename,
                   ".csv &",
                  sep = "")

  system(command)


  print("")
}

print("We're Outahere like Vladimir")
