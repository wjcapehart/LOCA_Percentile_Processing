

# Libraries

  # Tidyverse resources

  library(package = "tidyverse") # Multiple Tidyverse Resources
  library(package = "lubridate") # Date-Time Control

  # NCAR Libraries

  library(package = "extRemes") # NCEI  Data Retrieval Package



# Pulling Hucs for Zone Lookup Tables

  HUC_LUT_URL = "http://kyrill.ias.sdsmt.edu/wjc/eduresources/HUC08_Missouri_River_Basin.Rdata"

  load(file = url(HUC_LUT_URL), verbose=TRUE)

  remove(HUC_LUT_URL)

  HUC08_MRB_LUT




  HUC_AVAIL_URL = "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/LOCA_NGP/huc_08_basins/Completed_HUCS.RData"

  load(file = url(HUC_AVAIL_URL), verbose=TRUE)

  remove(HUC_AVAIL_URL)

bigger_table =

# Select Periods

start_years    = c(1951, 1961, 1976, 2011, 2036, 2061, 2070, 1956, 2006, 2026, 2050)
period_length  = c(  30,  30,    30,   30,   30,   30,   30,   50,   50,   50,  50)
end_years      = start_years + period_length -1




center_years = 0.5 * (end_years + start_years) - 0.5
center_years

Periods = tibble(start_years   = start_years,
                 center_years  = center_years,
                 end_years     = end_years,
                 period_length = period_length)

Periods_filename = str_c("/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_ExtRemes/HUC08/",
                      "NGP_LOCA_HUC08_",
                      "_Available_Return_Periods.RData",
                      sep = "")
print(Periods)

save(Periods, file=Periods_filename)




huc_zone_lut = Completed_HUCS[1]

for (huc_zone_lut in Completed_HUCS)
{  # huc

  # LOCA Data Extraction from SD Mines Thredds Server

  # URL Information


  loca_location_data = HUC08_MRB_LUT %>%
    filter(HUC08_Code_ID == huc_zone_lut)

  root_LOCA_URL = "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/LOCA_NGP/huc_08_regions/"
  root_LOCA_URL = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/huc_08_basins/"

  loca_filename = str_c("NGP_LOCA_HUC08_",
                        huc_zone_lut,
                        ".RData",
                        sep = "")

  LOCA_URL = str_c(root_LOCA_URL,
                   loca_filename,
                   sep = "")

  load(file    =(LOCA_URL),
       verbose = TRUE)

  remove(LOCA_URL)

  Ensembles = unique(loca_daily$Ensemble)
  Scenarios = unique(loca_daily$Scenario)






# Period Extraction (using the zone maximum daily event)
  for (start_year in start_years)
  { # Start Year

    end_year = start_year + 29



      loca_period = loca_daily %>%
        filter((year(Time) >= start_year) &
               (year(Time) <= end_year)   &
               (Percentile == "P100")    ) %>%
        select(-c(tasmax,tasmin))%>%
        mutate(Scenario = str_c(Scenario,
                                " (",
                                start_year,
                                "-",
                                end_year,
                                ")",
                                sep = ""))










      Period_Scenarios = unique(loca_period$Scenario)



      for (scenario in Period_Scenarios)
      {

        for (ensemble in Ensembles)
        {
          print(str_c(huc_zone_lut, " ",scenario, " ", ensemble))

          loca_scen_ens = loca_period %>%
                             filter((Ensemble == ensemble),
                                    (Scenario == scenario))


          threshold = quantile(x     = loca_scen_ens$pr,
                               probs = 0.95)

          fit_GP_daily = fevd(x          = loca_scen_ens$pr,
                              threshold  = threshold,
                              units      = "mm",
                              time.units = "365/year",
                              type       = "GP"
                           )

          year_return   = c( 2:100.,
                            141.,
                            200.,
                            316.,
                            500.)


          return_ci = ci(x             = fit_GP_daily,
                         return.period = year_return)


          if ((scenario == Period_Scenarios[1]) & (ensemble == Ensembles[1]) & (start_year == start_years[1]))
          {
            return_events = tibble(HUC                = unique(loca_daily$Division),
                                   Scenario           = scenario,
                                   start_year         = start_year,
                                   center_year        = 0.5 * (start_year + end_year) - 0.5,
                                   end_year           = end_year,
                                   Ensemble           = ensemble,
                                   Return_Period      = year_return,
                                   Return_Estimate_05 = return_ci[,1],
                                   Return_Estimate    = return_ci[,2],
                                   Return_Estimate_95 = return_ci[,3])

          } else # first run
          {
            delete_me     = tibble(HUC                = unique(loca_daily$Division),
                                   Scenario           = scenario,
                                   start_year         = start_year,
                                   center_year        = 0.5 * (start_year + end_year) - 0.5,
                                   end_year           = end_year,
                                   Ensemble           = ensemble,
                                   Return_Period      = year_return,
                                   Return_Estimate_05 = return_ci[,1],
                                   Return_Estimate    = return_ci[,2],
                                   Return_Estimate_95 = return_ci[,3])
            return_events = rbind(return_events,
                                  delete_me)

          } # not the first run

        } # ensemble

      } # scenario

  } # years


  loca_filename = str_c("/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_ExtRemes/HUC08/",
                         "NGP_LOCA_HUC08_",
                         huc_zone_lut,
                         "_Daily_Rainfall_Returns.RData",
                         sep = "")
  print(loca_filename)

  save(Periods, return_events, file=loca_filename)
  remove(return_events)

  print("---------")

} # huc
