library(stringr)
library(forcats)
library(readr)
library(tidyverse)
library(lubridate)
library(labelled)

directory     = "/data/DATASETS/LOCA_MACA_Ensembles/LOCA2/LOCA2_CONUS/Specific_Regional_Aggregate_Sets/NCEI_Climate_Divisions/R_Daily_Files/work/"

out_directory = "/data/DATASETS/LOCA_MACA_Ensembles/LOCA2/LOCA2_CONUS/Specific_Regional_Aggregate_Sets/NCEI_Climate_Divisions/R_Daily_Files/"
mon_directory = "/data/DATASETS/LOCA_MACA_Ensembles/LOCA2/LOCA2_CONUS/Specific_Regional_Aggregate_Sets/NCEI_Climate_Divisions/R_Monthly_Files/"
ann_directory = "/data/DATASETS/LOCA_MACA_Ensembles/LOCA2/LOCA2_CONUS/Specific_Regional_Aggregate_Sets/NCEI_Climate_Divisions/R_Annual_Files/"

prefix     = "LOCA2_V1_nCLIMDIV_"
prefix_mon = "LOCA2_V1_nCLIMDIV_MONTHLY_" 
prefix_ann = "LOCA2_V1_nCLIMDIV_ANNUAL_" 


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


load("./NCEI_nClimDiv_LUT.RData", verbose=TRUE)

Division        = NCEI_nClimDiv_LUT$Division
division_factor = factor(Division)
models_factor   = factor(models)
members_factor  = factor(members)

print(NCEI_nClimDiv_LUT)

for (filename in csv_files)
{

  Division_Code = RData_files  = str_remove(filename, str_c(directory, prefix, sep=""))
  
  
  print(str_c("Begin Processing Divison ",Division_Code))
  

  command = str_c("gunzip -v ",
                  filename,
                  ".csv.gz",
                  sep = "")

  system(command)

        loca2_daily = read_csv(str_c(filename,
                                    ".csv",
                                    sep=""),
                      progress = show_progress())


        if (is.na(unique(loca2_daily$Division))) {
          
          print(str_c("   --- Reinforcing Daily Division Codes ", 
                      Division_Code))
          
        } else {
          
          print(str_c("   --- Enforcing Daily Division Codes ", 
                      Division_Code, " ", 
                      unique(loca2_daily$Division)))
        }
        
        loca2_daily$Division = Division_Code
        loca2_daily$Division = factor(x      = loca2_daily$Division, 
                                      levels = division_factor)
        
        
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

                            
          filename_daily = str_c(out_directory,
                               prefix,
                               as.character(Division_Code),
                               ".RData",
                               sep = "")
          
          print(filename_daily)


          save(loca2_daily,
               file = filename_daily)
        
        print("   Aggregate Monthly")
        
        loca2_monthly = loca2_daily %>% 
          group_by(Scenario,
                   Model,
                   Member,
                   Percentile,
                   Year  = year(Time),
                   Month = month(Time)) %>% 
          summarize(Scenario   = Scenario[1],
                    Division   = Division[1],
                    Model      = Model[1],
                    Percentile = Percentile[1],
                    Time       = mean(Time),
                    tasmax     = mean(tasmax),
                    tasmin     = mean(tasmin),
                    pr         = sum(pr)) %>% 
        ungroup()
        
        
        
        loca2_monthly$Scenario   = factor(x      = loca2_monthly$Scenario,
                                        levels = c("Historical",
                                                   "SSP2-4.5",
                                                   "SSP3-7.0",
                                                   "SSP5-8.5"))
        
        
        
        loca2_monthly$Model   = factor(x      = loca2_monthly$Model,
                                     levels = models)
        
        loca2_monthly$Member   = factor(x      = loca2_monthly$Member,
                                      levels = members)
        
        loca2_monthly$Percentile = factor(x      = loca2_monthly$Percentile,
                                        levels = c("P000",
                                                   "P025",
                                                   "P050",
                                                   "P075",
                                                   "P100",
                                                   "MEAN"))
        
        loca2_monthly$tasmax = as.single(loca2_monthly$tasmax)
        loca2_monthly$tasmin = as.single(loca2_monthly$tasmin)
        loca2_monthly$pr     = as.single(loca2_monthly$pr)
        
        
        filename_mon = str_c(mon_directory,
                             prefix_mon,
                             as.character(Division_Code),
                             ".RData",
                             sep = "")
        

        
        save(loca2_monthly,
             file = filename_mon)         
        print("   Aggregate Annual")
        
        loca2_annual = loca2_daily %>% 
          group_by(Scenario,
                   Model,
                   Member,
                   Percentile,
                   Year  = year(Time)) %>% 
          summarize(Scenario   = Scenario[1],
                    Division   = Division[1],
                    Model      = Model[1],
                    Percentile = Percentile[1],
                    Time       = mean(Time),
                    tasmax     = mean(tasmax),
                    tasmin     = mean(tasmin),
                    pr         = sum(pr)) %>% 
        ungroup()
        
        
        
        
        loca2_annual$Scenario   = factor(x      = loca2_annual$Scenario,
                                          levels = c("Historical",
                                                     "SSP2-4.5",
                                                     "SSP3-7.0",
                                                     "SSP5-8.5"))
        
        
        
        loca2_annual$Model   = factor(x      = loca2_annual$Model,
                                       levels = models)
        
        loca2_annual$Member   = factor(x      = loca2_annual$Member,
                                        levels = members)
        
        loca2_annual$Percentile = factor(x      = loca2_annual$Percentile,
                                          levels = c("P000",
                                                     "P025",
                                                     "P050",
                                                     "P075",
                                                     "P100",
                                                     "MEAN"))
        
        loca2_annual$tasmax = as.single(loca2_annual$tasmax)
        loca2_annual$tasmin = as.single(loca2_annual$tasmin)
        loca2_annual$pr     = as.single(loca2_annual$pr)
        
        
        filename_ann = str_c(ann_directory,
                             prefix_ann,
                             as.character(Division_Code),
                             ".RData",
                             sep = "")
        

        
        save(loca2_annual,
             file = str_c(filename_ann,
                          sep=""))




  command = str_c("gzip -9fv ",
                  filename,
                   ".csv &",
                  sep = "")

  system(command)


  print("")
}

print("We're Outahere like Vladimir")



