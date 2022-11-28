
###############################
#
# Libraries
#

  library(package = "stringr")
  library(package = "forcats")
  library(package = "readr")
  library(package = "tidyverse")
  library(package = "lubridate")
  library(package = "RCurl")
  library(package = "labelled")

  library(package = "climdex.pcic")
  library(package = "PCICt")

#
##############################



###################################
#
# File and Basin Locations.
#



thredds_root= "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/LOCA_NGP/Specific_Regional_Aggregate_Sets/huc_08_basins/"
source_data_root= "/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_NGP/Specific_Regional_Aggregate_Sets/huc_08_basins/"
dest_output_root = "/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_NGP/climatology/LOCA_ETCCDI/huc_08_basins/"

#
# Load metadata for the missiouri river dataset
#

get_metadata = "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/LOCA_NGP/Specific_Regional_Aggregate_Sets/huc_08_basins/HUC08_Missouri_River_Basin.Rdata"



load(url(get_metadata), verbose=TRUE)

HUC08_MRB_LUT

Divisions = unique(HUC08_MRB_LUT$HUC08_Code_ID)

#
###################################

  
  
  
#########################
#
# Extract Unique Values of Scenarios and Ensembles 
#



#
#########################


newframe = TRUE

for (Div in Divisions) {

  print(str_c("-> ", Div, sep = ""))
  
  File_URL = str_c(source_data_root,
                   "R_Daily_Files/",
                   "NGP_LOCA_HUC08_",
                   Div,
                   ".RData",
                   sep = "")
  
  print(str_c("opening ", File_URL))
  
  #my.connection = url(description = File_URL)
  load(file = File_URL, verbose=TRUE)
  close(con = my.connection)
  #remove( my.connection)
  
  if (newframe) {
    TimePCICt = as.PCICt(as.character(loca_daily$Time), cal="gregorian")
    
    Ensembles   = unique(loca_daily$Ensemble)
    Percentiles = unique(loca_daily$Percentile)
    Scenarios   = unique(loca_daily$Scenario)
    
    print(  Ensembles)
    print(  Scenarios)
    print(Percentiles)
    
  } # Fix Time Calendar

  loca_daily$TimePCICt = TimePCICt
  loca_daily$pr[loca_daily$pr <  0.025] =  0
  
  for (Ens in Ensembles) {
    
    print(str_c("---> ",Ens))
  
    for (Sce in Scenarios[2:3]) {
      
      print(str_c("------> ",Sce))
      
      for (Til in c("P050","MEAN")) {
        
        print(str_c("---------> ",Til))
  
        subset = loca_daily %>%
          filter( (Ensemble   == Ens)            &
                 ((Scenario   == Sce)          | 
                  (Scenario   == "Historical") ) &
                 (Percentile == Til))
        
        # Create CI Dataframe
        
        ci = climdexInput.raw(tmax                = subset$tasmax,
                              tmin                = subset$tasmin,
                              prec                = subset$pr,
                              tmax.dates          = subset$TimePCICt, 
                              tmin.dates          = subset$TimePCICt, 
                              prec.dates          = subset$TimePCICt, 
                              base.range          = c(1976, 2005),
                              n                   = 5, 
                              northern.hemisphere = TRUE,
                              tavg                = NULL, 
                              tavg.dates          = NULL, 
                              quantiles           = NULL, 
                              temp.qtiles         = c(0.10, 0.90), 
                              prec.qtiles         = c(0.95, 0.99), 
                              max.missing.days    = c(annual = 15, 
                                                      monthly = 3),
                              min.base.data.fraction.present = 0.1)
  
        # Annual ECIs
        
        fd_ann      = climdex.fd(     ci = ci)
        su_ann      = climdex.su(     ci = ci)
        id_ann      = climdex.id(     ci = ci)
        tr_ann      = climdex.tr(     ci = ci)
        gsl_ann     = climdex.gsl(    ci = ci)
        tnn_ann     = climdex.tnn(    ci = ci, freq = "annual")
        txn_ann     = climdex.txn(    ci = ci, freq = "annual")
        txx_ann     = climdex.txx(    ci = ci, freq = "annual")
        tnx_ann     = climdex.tnx(    ci = ci, freq = "annual")
        tn10p_ann   = climdex.tn10p(  ci = ci, freq = "annual")
        tx10p_ann   = climdex.tx10p(  ci = ci, freq = "annual")
        tn90p_ann   = climdex.tn90p(  ci = ci, freq = "annual")
        tx90p_ann   = climdex.tx90p(  ci = ci, freq = "annual")
        wsdi_ann    = climdex.wsdi(   ci = ci, spells.can.span.years = TRUE)
        csdi_ann    = climdex.csdi(   ci = ci, spells.can.span.years = TRUE)
        dtr_ann     = climdex.dtr(    ci = ci, freq = "annual")
        rx1day_ann  = climdex.rx1day( ci = ci, freq = "annual")
        rx5day_ann  = climdex.rx5day( ci = ci, freq = "annual")
        sdii_ann    = climdex.sdii(   ci = ci)
        r10mm_ann   = climdex.r10mm(  ci = ci)
        r20mm_ann   = climdex.r20mm(  ci = ci)
        r03mm_ann   = climdex.rnnmm(  ci = ci, threshold = 2.54)
        cdd_ann     = climdex.cdd(    ci = ci, spells.can.span.years = TRUE)
        cwd_ann     = climdex.cwd(    ci = ci, spells.can.span.years = TRUE)
        r95ptot_ann = climdex.r95ptot(ci = ci)
        r99ptot_ann = climdex.r99ptot(ci = ci)
        prcptot_ann = climdex.prcptot(ci = ci)
        
        # Additonal Monthly ECIs
        
        tnn_mon     = climdex.tnn(   ci = ci, freq = "monthly")
        txn_mon     = climdex.txn(   ci = ci, freq = "monthly")
        txx_mon     = climdex.txx(   ci = ci, freq = "monthly")
        tnx_mon     = climdex.tnx(   ci = ci, freq = "monthly")
        dtr_mon     = climdex.dtr(   ci = ci, freq = "monthly")
        tn10p_mon   = climdex.tn10p( ci = ci, freq = "monthly")
        tx10p_mon   = climdex.tx10p( ci = ci, freq = "monthly")
        tn90p_mon   = climdex.tn90p( ci = ci, freq = "monthly")
        tx90p_mon   = climdex.tx90p( ci = ci, freq = "monthly")
        rx1day_mon  = climdex.rx1day(ci = ci, freq = "monthly")
        rx5day_mon  = climdex.rx5day(ci = ci, freq = "monthly")
        
  
        
        Time_ann  = as.Date(str_c(labels(tnx_ann), "-07-02", sep=""))
        Time_mon  = as.Date(str_c(labels(tnx_mon),    "-15", sep=""))
        
        temp_mon = tibble(division    = Div,
                          Scenario    = Sce,
                          Ensemble    = Ens,
                          Percentiles = Til,
                          Time        = Time_mon,
                          Year        = year(Time_mon),
                          Month       = month(Time_mon,
                                              abbr  = TRUE,
                                              label = TRUE),
                          TXx         = remove_attributes(x =    txx_mon, attributes = "names"),
                          TNx         = remove_attributes(x =    tnx_mon, attributes = "names"),
                          TXn         = remove_attributes(x =    txn_mon, attributes = "names"),
                          TNn         = remove_attributes(x =    tnn_mon, attributes = "names"),   
                          TN10p       = remove_attributes(x =  tn10p_mon, attributes = "names"),
                          TX10p       = remove_attributes(x =  tx10p_mon, attributes = "names"),
                          TN90p       = remove_attributes(x =  tn90p_mon, attributes = "names"),
                          TX90p       = remove_attributes(x =  tx90p_mon, attributes = "names"),
                          DTR         = remove_attributes(x =    dtr_mon, attributes = "names"),
                          Rx1day      = remove_attributes(x = rx1day_mon, attributes = "names"),
                          Rx5day      = remove_attributes(x = rx5day_mon, attributes = "names"))
        
        temp_ann = tibble(division    = Div,
                          Scenario    = Sce,
                          Ensemble    = Ens,
                          Percentiles = Til,
                          Time        = Time_ann,
                          Year        = year(Time_ann),
                          FD          = remove_attributes(x =      fd_ann, attributes = "names"),
                          SU          = remove_attributes(x =      su_ann, attributes = "names"),
                          ID          = remove_attributes(x =      id_ann, attributes = "names"),
                          TR          = remove_attributes(x =      tr_ann, attributes = "names"),
                          GSL         = remove_attributes(x =     gsl_ann, attributes = "names"),
                          TXx         = remove_attributes(x =     txx_ann, attributes = "names"),
                          TNx         = remove_attributes(x =     tnx_ann, attributes = "names"),
                          TXn         = remove_attributes(x =     txn_ann, attributes = "names"),
                          TNn         = remove_attributes(x =     tnn_ann, attributes = "names"),                  
                          TN10p       = remove_attributes(x =   tn10p_ann, attributes = "names"),
                          TX10p       = remove_attributes(x =   tx10p_ann, attributes = "names"),
                          TN90p       = remove_attributes(x =   tn90p_ann, attributes = "names"),
                          TX90p       = remove_attributes(x =   tx90p_ann, attributes = "names"),
                          WSDI        = remove_attributes(x =    wsdi_ann, attributes = "names"),
                          CSDI        = remove_attributes(x =    csdi_ann, attributes = "names"),
                          DTR         = remove_attributes(x =     dtr_ann, attributes = "names"),
                          Rx1day      = remove_attributes(x =  rx1day_ann, attributes = "names"),
                          Rx5day      = remove_attributes(x =  rx5day_ann, attributes = "names"),
                          SDII        = remove_attributes(x =    sdii_ann, attributes = "names"),
                          R10mm       = remove_attributes(x =   r10mm_ann, attributes = "names"),
                          R20mm       = remove_attributes(x =   r20mm_ann, attributes = "names"),
                          R03mm       = remove_attributes(x =   r03mm_ann, attributes = "names"),
                          CDD         = remove_attributes(x =     cdd_ann, attributes = "names"),
                          CWD         = remove_attributes(x =     cwd_ann, attributes = "names"),
                          R95pTOT     = remove_attributes(x = r95ptot_ann, attributes = "names"),
                          R99pTOT     = remove_attributes(x = r99ptot_ann, attributes = "names"),
                          PRCPTOT     = remove_attributes(x = prcptot_ann, attributes = "names"))
        
        if (Sce == "RCP 8.5") {
          
          temp_ann = temp_ann %>%
            filter(Year > 2005)
          temp_mon = temp_mon %>%
            filter(Year > 2005)        
          
        } else {  # Remove Historical Period from the RCP 8.5
          
          temp_ann = temp_ann %>%
            mutate(Scenario = if_else(condition = (Year < 2006),
                                      true      =  "Historical",
                                      false     =     "RCP 4.5"))
          
          temp_mon = temp_mon %>%
            mutate(Scenario = if_else(condition = (Year < 2006),
                                      true      =  "Historical",
                                      false     =     "RCP 4.5"))
          
        } # Label Historical and RCP 4.5
        
        
        
        if (newframe) { 
          newframe    =  FALSE
          CEI_MONTHLY = temp_mon
          CEI_ANNUAL  = temp_ann
        } else { # Create New CEI_MONTHLY & CEI_ANNUAL Frames
          CEI_MONTHLY = rbind(CEI_MONTHLY, temp_mon)
          CEI_ANNUAL  = rbind(CEI_ANNUAL,  temp_ann)
        } # Append CEI_MONTHLY & CEI_ANNUAL Frames
        
      } # Percentiles
    } # Scenarios
  } # Ensembles
  
  CEI_MONTHLY$Scenario = as.factor(CEI_MONTHLY$Scenario)
  CEI_ANNUAL$Scenario  = as.factor(CEI_ANNUAL$Scenario)
  
  File_Out = str_c(dest_output_root,
                   "./NGP_LOCA_ETCCDI_HUC08_",
                   Div,
                   ".RData",
                   sep = "")
  
  print(str_c("saving to ", File_Out))
  
  save(CEI_MONTHLY,
       CEI_ANNUAL,
       file = File_Out)

} # Division


remove(newframe)
remove(subset)
remove(ci)
remove(temp_ann)
remove(temp_mon)
remove(Sce)
remove(Til)
remove(Ens)
remove(Time_ann)
remove(Time_mon)

remove(tnn_mon)
remove(txn_mon)
remove(txx_mon)
remove(tnx_mon)
remove(dtr_mon)
remove(tn10p_mon)
remove(tx10p_mon)
remove(tn90p_mon)
remove(tx90p_mon)
remove(rx1day_mon)
remove(rx5day_mon)        


remove(fd_ann)
remove(su_ann)
remove(id_ann)
remove(tr_ann)
remove(gsl_ann)
remove(sdii_ann)
remove(r10mm_ann)
remove(r20mm_ann)
remove(r03mm_ann)
remove(cdd_ann)
remove(cwd_ann)
remove(tnn_ann)
remove(txn_ann)
remove(txx_ann)
remove(tnx_ann)
remove(dtr_ann)
remove(wsdi_ann)
remove(tn10p_ann)
remove(tx10p_ann)
remove(tn90p_ann)
remove(tx90p_ann)
remove(rx1day_ann)
remove(rx5day_ann)      
remove(r95ptot_ann)
remove(r99ptot_ann)
remove(prcptot_ann)      
      
   
