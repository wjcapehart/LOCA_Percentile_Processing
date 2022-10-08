

  library(package = "stringr")
  library(package = "forcats")
  library(package = "readr")
  library(package = "tidyverse")
  library(package = "lubridate")
  library(package = "RCurl")



thredds_root= "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/LOCA_NGP/Specific_Regional_Aggregate_Sets/huc_08_basins/R_Daily_Files/"


get_metadata = "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/LOCA_NGP/Specific_Regional_Aggregate_Sets/huc_08_basins/HUC08_Missouri_River_Basin.Rdata"

load(url(get_metadata), verbose=TRUE)

hucs_to_process = c("10120110")

HUC08_MRB_LUT = HUC08_MRB_LUT %>% filter(HUC08_Code_ID %in% hucs_to_process)





  Output_Scenarios = c("RCP 4.5",
                       "RCP 8.5")

  for (basin in hucs_to_process)
  {

    metadata = HUC08_MRB_LUT %>% filter(HUC08_Code_ID == basin)
    
    spatial = HUC08_MRB_Code_Frame %>% 
      filter(HUC08_Code_ID == basin) %>% 
      group_by(HUC08_Code_ID) %>% 
      summarize(lat = mean(lat),
                lon = mean(lon))


    File_URL = str_c(thredds_root,
                     "NGP_LOCA_HUC08_",
                     basin,
                     ".RData",
                     sep = "")

    my.connection = url(description = File_URL)
      load(file = my.connection)
      close(con = my.connection)
      remove( my.connection)

    ensemble = unique(loca_daily$Ensemble)[1]

    for (ensemble in unique(loca_daily$Ensemble)[1])
    {

      for (scenario in Output_Scenarios)
      {

        subset = loca_daily %>%
                       filter(((Scenario  == "Historical") |
                               (Scenario  == scenario)) &
                              (Ensemble   == ensemble) &
                              (Percentile == "MEAN")) %>%
                       mutate(Year   = year(Time),
                              Julian = yday(Time))

        #
        # Write the Daily Max/Min Temperature File
        #

        filename_tmp = str_c("./SWAT_LOCA_",
                             basin,
                             "__",
                             ensemble,
                             "__",
                             str_replace(string      = scenario,
                                         pattern     = " ",
                                         replacement = ""),
                             "__",
                             basin,
                             ".TMP",
                             sep = "")
        filename_tmp = str_replace(string      = filename_tmp,
                                   pattern     = ".5",
                                   replacement = "5")

        print(filename_tmp)

        sink(filename_tmp)

          # Line 1 == Title
          record = filename_tmp
            cat(record)
            cat("\n")
          # Line 2 == Latitude
          record = sprintf("%17.6f\n", 44.40000)     
            cat(record)
          # Line 3 == Longitude
          record = sprintf("%17.5f\n",  -103.47000)
            cat(record)
          # Line 4 == Elevation
          record = sprintf("%17.5f\n", 1005.79999 ) # metadata$Mean_Elevation)
            cat(record)

          for (time in subset$Time)
          {
            record_subset = subset %>% filter(Time == time)

            # Line N == Data Recrord
            record = sprintf("%0.4i%0.3i%5.1f%5.1f\n",record_subset$Year,
                                                      record_subset$Julian,
                                                      record_subset$tasmax,
                                                      record_subset$tasmin)
               cat(record)
          }



        sink()





        #
        # Write the Daily Rainfall File
        #        

        filename_pcp = str_c("./SWAT_LOCA_",
                             basin,
                             "__",
                             ensemble,
                             "__",
                             str_replace(string      = scenario,
                                         pattern     = " ",
                                         replacement = ""),
                             "__",
                             basin,
                             ".PCP",
                             sep = "")
        filename_pcp = str_replace(string      = filename_pcp,
                                   pattern     = ".5",
                                   replacement = "5")

        print(filename_pcp)


        sink(filename_pcp)

          # Line 1 == Title
          record =         
            cat(record)
            cat("\n")
          # Line 2 == Latitude
          record = sprintf("%17.6f\n", spatial$lat)
            cat(record)
          # Line 3 == Longitude
          record = sprintf("%17.5f\n", spatial$lon)
            cat(record)
          # Line 4 == Elevation # Add me Lucas!
          record = sprintf("%17.5f\n", 1115.3359) # metadata$Mean_Elevation)
            cat(record)


          for (time in subset$Time)
          {
            record_subset = subset %>% filter(Time == time)

            # Line N == Data Recrord
            record = sprintf("%0.4i%0.3i%5.1f\n",record_subset$Year,
                                                      record_subset$Julian,
                                                      record_subset$pr)
               cat(record)
          }
         sink()



        #
        # Clean Up
        #

        remove(subset)
        remove(filename_tmp)
        remove(filename_pcp)
      } # scenario

    } # ensemble

  } # basin
  remove(ensemble)
  remove(scenario)
  remove(basin)

