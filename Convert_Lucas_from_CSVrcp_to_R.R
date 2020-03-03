library(stringr)
library(forcats)
library(readr)
library(tidyverse)
library(lubridate)

directory = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/blackhills_domain/done/"

prefix    = "NGP_LOCA_LUCAS_"


csv_files = intersect(list.files(path    = directory,
                                 pattern = prefix),
                      list.files(path    = directory,
                                 pattern = "csv.gz"))

csv_files  = str_remove(csv_files, ".csv.gz")

csv_files = str_c(directory,csv_files,sep="")

print(csv_files)

load(file=("./Lucas_LUT.RData"))





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

Ensembles_factor = factor(Ensembles)

for (filename in csv_files)
{
  command = str_c("gunzip -v ",
                  filename,
                  ".csv.gz",
                  sep = "")

  system(command)

        loca_daily = read_csv(str_c(filename,
                                    ".csv",
                                    sep=""))



        print(loca_daily$Division[1])
        if (is.numeric(loca_daily$Division[1]))
        {
          loca_daily$Division = as.character(sprintf("%0d",loca_daily$Division))
        }
        loca_daily = loca_daily %>%
          mutate(Scenario = case_when(Scenario == "historical" ~ "Historical",
                                      Scenario == "rcp45"      ~ "RCP 4.5",
                                      Scenario == "rcp85"      ~ "RCP 8.5"))

        loca_daily$Time       = as.Date( sub("\uFEFF", "", loca_daily$Time))

        loca_daily$Scenario   = factor(x      = loca_daily$Scenario,
                                       levels = c("Historical",
                                                  "RCP 4.5",
                                                  "RCP 8.5"))

        loca_daily$Division   = factor(x    = loca_daily$Division,
                                       levels = Divisions_factor)

        loca_daily$Ensemble   = factor(x      = loca_daily$Ensemble,
                                       levels = Ensembles)

        loca_daily$Percentile = factor(x      = loca_daily$Percentile,
                                       levels = c("P000",
                                                  "P025",
                                                  "P050",
                                                  "P075",
                                                  "P100",
                                                  "MEAN"))
        
        loca_daily = rename(loca_daily, Basin = Division)
        
       # loca_daily = left_join(x  = loca_daily,
      #                         y  = Lucas_LUT,
      #                         by = "Basin")

        last_record = loca_daily[nrow(loca_daily), ]
        print(last_record)
        
        

        save(loca_daily,
             file = str_c(filename,
                          ".RData",
                          sep=""))





  command = str_c("gzip -9fv ",
                  filename,
                   ".csv",
                  sep = "")

  system(command)


  print("")
}

print("We're Outahere like Vladimir")
