library(stringr)
library(forcats)
library(readr)
library(tidyverse)
library(lubridate)

directory = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/huc_08_basins/"

prefix    = "NGP_LOCA_HUCS_"
outpref   = "NGP_LOCA_HUC08_"

csv_files = intersect(list.files(path    = directory,
                                 pattern = prefix),
                      list.files(path    = directory,
                                 pattern = "rcp85.csv"))

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

division=Divisions[1]

for (division in Divisions)
{

  filename = str_c(directory,
                   prefix,
                   division,
                   "_historical",
                   sep = "")

  shell_command = str_c("tail -n 1  ",
                        filename,
                        ".csv",
                        sep = "")

  ah = system(shell_command, intern = TRUE)


  print(ah)

  filename = str_c(directory,
                   prefix,
                   division,
                   "_rcp45",
                   sep = "")

  shell_command = str_c("tail -n 1  ",
                        filename,
                        ".csv",
                        sep = "")

  a4 = system(shell_command, intern = TRUE)


  print(a4)


  filename = str_c(directory,
                   prefix,
                   division,
                   "_rcp85",
                   sep = "")

  shell_command = str_c("tail -n 1  ",
                        filename,
                        ".csv",
                        sep = "")

  a8 = system(shell_command, intern = TRUE)


  print(a8)

  if (str_detect(ah,"bcc-csm1-1-m_r1i1p1,historical,MEAN") &
      str_detect(a4,"bcc-csm1-1-m_r1i1p1,rcp45,MEAN") &
      str_detect(a8,"bcc-csm1-1-m_r1i1p1,rcp85,MEAN") &
      str_detect(ah,"2005-12-31" ) &
      str_detect(a4,"2099-12-31" ) &
      str_detect(a8,"2099-12-31" )) {




        filename = str_c(directory,
                         prefix,
                         division,
                         "_",
                         "historical",
                         sep = "")

        loca_daily = read_csv(str_c(filename,".csv",sep=""))
        loca_daily[nrow(loca_daily), ]

        filename = str_c(directory,
                         prefix,
                         division,
                         "_",
                         "rcp45",
                         sep = "")

        loca_dailyrcp = read_csv(str_c(filename,".csv",sep=""))

        loca_daily = rbind(loca_daily,
                           loca_dailyrcp)

        loca_dailyrcp[nrow(loca_dailyrcp), ]


        remove(loca_dailyrcp)


         filename = str_c(directory,
                          prefix,
                          division,
                          "_",
                          "rcp85",
                          sep = "")

         loca_dailyrcp = read_csv(str_c(filename,".csv",sep=""))
         loca_dailyrcp[nrow(loca_dailyrcp), ]

         loca_daily = rbind(loca_daily,
                            loca_dailyrcp)

         remove(loca_dailyrcp)

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

        last_record = loca_daily[nrow(loca_daily), ]

        if ( ((last_record$Scenario == "Historical") & (last_record$Time != "2005-12-31")) |
             ((last_record$Scenario != "Historical") & (last_record$Time != "2099-12-31")) )
        {
            print(str_c("  truncating",
                        last_record$Ensemble,
                        last_record$Scenario,
                        last_record$Time,
                        sep = " "))

            loca_daily = loca_daily %>%
                filter( ! ((loca_daily$Scenario   == last_record$Scenario)   &
                           (loca_daily$Ensemble   == last_record$Ensemble)   &
                           (year(loca_daily$Time) == year(last_record$Time)) ) )
        }

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



  } else {
    print("nope!")
  }



}

rData_files = intersect(list.files(path    = directory,
                                   pattern = "NGP_LOCA_HUC08_"),
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
