

# Libraries

  # Tidyverse resources

  library(package = "tidyverse") # Multiple Tidyverse Resources
  library(package = "lubridate") # Date-Time Control

  # NCAR Libraries

  library(package = "extRemes") # NCEI  Data Retrieval Package

  
  library(package = "climdex.pcic") # NCAR Extreme Analysis Package
  
  library(package = "PCICt")
  
  
# Pulling Hucs for Zone Lookup Tables

  HUC_LUT_URL = "http://kyrill.ias.sdsmt.edu/wjc/eduresources/HUC08_Missouri_River_Basin.Rdata"

  
  
  my.connection = url(description = HUC_LUT_URL)
  load(file = my.connection)
  close(con = my.connection)
  

  HUC08_MRB_LUT




  HUC_AVAIL_URL = "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/LOCA_NGP/huc_08_basins/Completed_HUCS.RData"


  
  my.connection = url(description = HUC_AVAIL_URL)
  load(file = my.connection)
  close(con = my.connection)
  
  remove(HUC_AVAIL_URL)
  
  
  
  huc_zone_lut = Completed_HUCS[1]
NAFF_Completed_HUCS = c("10030101")
for (huc_zone_lut in Completed_HUCS[25:307])
{  # huc
  
  print(str_c(" - ", huc_zone_lut))
  

  
  FIRST = TRUE

  # LOCA Data Extraction from SD Mines Thredds Server

  # URL Information


  loca_location_data = HUC08_MRB_LUT %>%
    filter(HUC08_Code_ID == huc_zone_lut)

  root_LOCA_URL = "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/LOCA_NGP/huc_08_basins/"
  root_LOCA_URL = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/huc_08_basins/"
  out_LOCA_URL = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/huc_08_basins/"
  
  loca_filename = str_c("NGP_LOCA_HUC08_",
                        huc_zone_lut,
                        ".RData",
                        sep = "")

  LOCA_URL = str_c(root_LOCA_URL,
                   loca_filename,
                   sep = "")

    load(file = LOCA_URL)

  remove(LOCA_URL)
  
  

  loca_daily = loca_daily %>% filter(Percentile == "P050")
  
  loca_daily$PCICtime = as.PCICt(x   = as.character(loca_daily$Time),
                                 cal = "gregorian")
  
  loca_daily$Year       = as.factor(year(loca_daily$PCICtime))
  loca_daily$Month      = as.factor(month(loca_daily$PCICtime))
  loca_daily$Year_Month = as.factor(str_c(loca_daily$Year,
                                          "-",
                                          sprintf("%02d", loca_daily$Month),
                                          sep = ""))
  
  loca_daily$tasavg = (loca_daily$tasmax + loca_daily$tasmin) / 2.
  

  

  Ensembles = unique(loca_daily$Ensemble)
  Scenarios = unique(loca_daily$Scenario)

  ensemble = Ensembles[1]
  for (ensemble in Ensembles)
  {  # Ensembles
    
    print(str_c(" -- ", ensemble))

    loca_ens = loca_daily   %>% filter(Ensemble == ensemble)
    
    
    
    scenario = Scenarios[2]
    for (scenario in Scenarios[2:3])
    {  # Scenarios
      
      print(str_c(" --- ", scenario))
      
      
      loca_sub = loca_ens %>% filter((Scenario == "Historical") | (Scenario == scenario))

      
      climdexInput = climdexInput.raw(tmax       =  loca_sub$tasmax, 
                                      tmin       =  loca_sub$tasmin, 
                                      prec       =  loca_sub$pr,
                                      tmax.dates =  loca_sub$PCICtime, 
                                      tmin.dates =  loca_sub$PCICtime, 
                                      prec.dates =  loca_sub$PCICtime,
                                      base.range = c(1961, 1990), 
                                      n = 5, 
                                      northern.hemisphere = TRUE,
                                      tavg =  loca_sub$tasavg, 
                                      tavg.dates =  loca_sub$PCICtime, 
                                      quantiles = NULL, 
                                      temp.qtiles = c(0.1,0.9), 
                                      prec.qtiles = c(0.95, 0.99), 
                                      max.missing.days = c(annual = 15, monthly = 3), 
                                      min.base.data.fraction.present = 0.1)

      climdex = climdex.get.available.indices(ci = climdexInput, function.names = TRUE)
      sort(climdex)
      
      loc_etccdi_annual = tibble(Year = unique(loca_daily$Year))
      temp = loca_daily %>% group_by(Year) %>% summarize(Time = mean(Time))
         loc_etccdi_annual$Time = temp$Time
      loc_etccdi_annual$Ensemble = ensemble
      loc_etccdi_annual$Scenario = scenario
      
      loc_etccdi_annual$cdd =  climdex.cdd(climdexInput)
      loc_etccdi_annual$csdi =  climdex.csdi(climdexInput)
      loc_etccdi_annual$cwd =  climdex.cwd(climdexInput)
      loc_etccdi_annual$dtr =  climdex.dtr(climdexInput,freq = c("annual"))
      loc_etccdi_annual$fd =  climdex.fd(climdexInput)
      loc_etccdi_annual$gsl =  climdex.gsl(climdexInput)
      loc_etccdi_annual$id =  climdex.id(climdexInput)
      
      loc_etccdi_annual$prcptot =  climdex.prcptot(climdexInput)
      temp = loca_daily %>% group_by(Year) %>% summarize(prcpmax = max(pr))
          loc_etccdi_annual$prcpmax =  temp$prcpmax 
      loc_etccdi_annual$r10mm =  climdex.r10mm(climdexInput)
      loc_etccdi_annual$r20mm =  climdex.r20mm(climdexInput)
      loc_etccdi_annual$r12mm =  climdex.rnnmm(climdexInput, threshold =  12.7)
      loc_etccdi_annual$r95ptot =  climdex.r95ptot(climdexInput)
      loc_etccdi_annual$r99ptot =  climdex.r99ptot(climdexInput)
      loc_etccdi_annual$rx1day =  climdex.rx1day(climdexInput,freq = c("annual"))
      loc_etccdi_annual$rx5day =  climdex.rx5day(climdexInput,freq = c("annual"))
      loc_etccdi_annual$spii =  climdex.sdii(climdexInput)
      loc_etccdi_annual$su =  climdex.su(climdexInput)
      loc_etccdi_annual$tn10p =  climdex.tn10p(climdexInput,freq = c("annual"))
      loc_etccdi_annual$tn90p =  climdex.tn90p(climdexInput,freq = c("annual"))
      loc_etccdi_annual$tnn =  climdex.tnn(climdexInput,freq = c("annual"))
      temp =  loca_daily %>% group_by(Year) %>% summarize(tna = mean(tasmin))
         loc_etccdi_annual$tna = temp$tna 
      loc_etccdi_annual$tnx =  climdex.tnx(climdexInput,freq = c("annual"))
      loc_etccdi_annual$tr =  climdex.tr(climdexInput)
      loc_etccdi_annual$tx10p =  climdex.tx10p(climdexInput,freq = c("annual"))
      loc_etccdi_annual$tx90p =  climdex.tx90p(climdexInput,freq = c("annual"))
      loc_etccdi_annual$txn =  climdex.txn(climdexInput,freq = c("annual"))
      temp =  loca_daily %>% group_by(Year) %>% summarize(txa = mean(tasmax))
         loc_etccdi_annual$txa = temp$txa 
      loc_etccdi_annual$txx =  climdex.txx(climdexInput,freq = c("annual"))
      loc_etccdi_annual$wsdi =  climdex.wsdi(climdexInput)
      loc_etccdi_annual$wetting_rain_int =   spell.length.max(loca_sub$pr,loca_sub$Year, 12.7, "<=", TRUE)
      
      
      
      
      
      
      
      
      loc_etccdi_monthly = tibble(Year_Month = unique(loca_daily$Year_Month))
      temp =  loca_daily %>% group_by(Year_Month) %>% summarize(Time = mean(Time))
         loc_etccdi_monthly$Time  = temp$Time 
      loc_etccdi_monthly$Year = as.factor(year(loc_etccdi_monthly$Time))
      loc_etccdi_monthly$Month = as.factor(month(loc_etccdi_monthly$Time))
      loc_etccdi_monthly$Ensemble = ensemble
      loc_etccdi_monthly$Scenario = scenario
      
      
      
      loc_etccdi_monthly$dtr =  climdex.dtr(climdexInput, freq = c("monthly"))
      temp =  loca_daily %>% group_by(Year_Month) %>% summarize(prcptot = sum(pr))
         loc_etccdi_monthly$prcptot  = temp$prcptot 

      temp =  loca_daily %>% group_by(Year_Month) %>% summarize(prcpmax = max(pr))
         loc_etccdi_monthly$prcpmax  = temp$prcpmax          
               
      loc_etccdi_monthly$r10mm =  climdex.r10mm(climdexInput)
      loc_etccdi_monthly$r20mm =  climdex.r20mm(climdexInput)
      loc_etccdi_monthly$r12mm =  number.days.op.threshold(loca_sub$pr,loca_sub$Year_Month, 12.7, ">=")
      loc_etccdi_monthly$rx1day =  climdex.rx1day(climdexInput,freq = c("monthly"))
      loc_etccdi_monthly$rx5day =  climdex.rx5day(climdexInput,freq = c("monthly"))
      loc_etccdi_monthly$spii = simple.precipitation.intensity.index(loca_sub$pr,loca_sub$Year_Month)
      loc_etccdi_monthly$tn10p =  climdex.tn10p(climdexInput,freq = c("monthly"))
      loc_etccdi_monthly$tn90p =  climdex.tn90p(climdexInput,freq = c("monthly"))
      loc_etccdi_monthly$tnn =  climdex.tnn(climdexInput,freq = c("monthly"))
      
      
      temp =  loca_daily %>% group_by(Year_Month) %>% summarize(tna = mean(tasmin))
         loc_etccdi_monthly$tna  = temp$tna          
      
      
      loc_etccdi_monthly$tnx =  climdex.tnx(climdexInput,freq = c("monthly"))
      loc_etccdi_monthly$tx10p =  climdex.tx10p(climdexInput,freq = c("monthly"))
      loc_etccdi_monthly$tx90p =  climdex.tx90p(climdexInput,freq = c("monthly"))
      loc_etccdi_monthly$txn =  climdex.txn(climdexInput,freq = c("monthly"))
      
      temp =  loca_daily %>% group_by(Year_Month) %>% summarize(txa = mean(tasmax))
          loc_etccdi_monthly$txa  = temp$txa     
      
      loc_etccdi_monthly$txx =  climdex.txx(climdexInput,freq = c("monthly"))
      
      
      
      loc_etccdi_monthly$wetting_rain_int =   spell.length.max(loca_sub$pr,loca_sub$Year_Month, 12.7, "<=", FALSE)
      
      loc_etccdi_annual = loc_etccdi_annual %>% mutate(Scenario = if_else(year(Time) <= 2005,
                                                                          "Historical",
                                                                          scenario))
 
      
      loc_etccdi_monthly = loc_etccdi_monthly %>% mutate(Scenario = if_else(year(Time) <= 2005,
                                                                          "Historical",
                                                                          scenario))
      
      
      if (scenario == Scenarios[3])
      {
        temp = loc_etccdi_annual %>% filter(Scenario == Scenarios[3])
        loc_etccdi_annual  = temp
        temp = loc_etccdi_monthly %>% filter(Scenario == Scenarios[3])
        loc_etccdi_monthly = temp
      }
      
      
      
      if (FIRST)  
      {
        
        FIRST  = FALSE
        
        loca_etccdi_annual  = loc_etccdi_annual
        loca_etccdi_monthly = loc_etccdi_monthly
        
        remove(loc_etccdi_annual)
        remove(loc_etccdi_monthly)
        
      } else {  # not first
        
        loca_etccdi_annual  = rbind(loca_etccdi_annual,  loc_etccdi_annual)
        loca_etccdi_monthly = rbind(loca_etccdi_monthly, loc_etccdi_monthly)
        
        remove(loc_etccdi_annual)
        remove(loc_etccdi_monthly)
        
        
      } # not First
      
      
      remove(climdexInput)
      remove(temp)
      remove(climdex)
      
    }  # Scenarios
    
    
  }   # Ensembles
      
  loca_filename = str_c("/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_ETCCDI/HUC08/",
                        "NGP_LOCA_ETCCDI_HUC08_",
                        huc_zone_lut,
                        ".RData",
                        sep = "")
  
  save(loca_etccdi_annual, 
       loca_etccdi_monthly, 
       file = loca_filename)
  
   
  
  
  print(1)
  

  remove(loca_etccdi_annual)
  remove(loca_etccdi_monthly)
  

} # huc
