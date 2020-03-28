

  library(package = "stringr")
  library(package = "forcats")
  library(package = "readr")
  library(package = "tidyverse")
  library(package = "lubridate")
  library(package = "RCurl")


dir.create("./swat_output")

thredds_root = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/Specific_Regional_Aggregate_Sets/blackhills_domain/"


  Inventory_URL = str_c(thredds_root,
                        "Lucas_LUT.RData",
                        sep = "")


  my.connection = (description = Inventory_URL)
       load(file = my.connection)
  #     close(con = my.connection)
       remove( my.connection)

  remove(Inventory_URL)

  print(Lucas_LUT)




  Output_Scenarios = c("RCP 4.5",
                       "RCP 8.5")

#  for (basin in Lucas_LUT$Basin[1])
  for (basin in c("0032"))
  {

    metadata = Lucas_LUT %>% filter(Basin == basin)

    File_URL = str_c(thredds_root,
                     "R_Daily_Files/",
                     "NGP_LOCA_LUCAS_",
                     basin,
                     ".RData",
                     sep = "")

    my.connection = (description = File_URL)
      load(file = my.connection)
    #  close(con = my.connection)
      remove( my.connection)

    ensemble = unique(loca_daily$Ensemble)

    for (ensemble in unique(loca_daily$Ensemble))
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

        filename_tmp = str_c("./swat_output/SWAT_LUCAS__",
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
          record = sprintf("%17.6f\n", metadata$Latitude)
            cat(record)
          # Line 3 == Longitude
          record = sprintf("%17.5f\n", metadata$Longitude)
            cat(record)
          # Line 4 == Elevation
          record = sprintf("%17.5f\n", metadata$Mean_Elevation)
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

        filename_pcp = str_c("./swat_output/SWAT_LUCAS__",
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
          record = sprintf("%17.6f\n", metadata$Latitude)
            cat(record)
          # Line 3 == Longitude
          record = sprintf("%17.5f\n", metadata$Longitude)
            cat(record)
          # Line 4 == Elevation # Add me Lucas!
          record = sprintf("%17.5f\n", metadata$Mean_Elevation)
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
