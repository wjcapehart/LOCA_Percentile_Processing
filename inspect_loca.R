

# Libraries

  # Tidyverse resources

  library(package = "tidyverse") # Multiple Tidyverse Resources
  library(package = "lubridate") # Date-Time Control

  # NCAR Libraries

  library(package = "extRemes") # NCEI  Data Retrieval Package



  
  
  # Pulling Hucs for Zone Lookup Tables
  
  HUC_LUT_URL = "http://kyrill.ias.sdsmt.edu/wjc/eduresources/HUC08_Missouri_River_Basin.Rdata"
  
  load(file = url(HUC_LUT_URL), verbose=TRUE)
  
  
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
  
  Ensembles_factor = factor(Ensembles)
  
  
  
  remove(HUC_LUT_URL)
  
  HUC08_MRB_LUT = HUC08_MRB_LUT %>% filter(HUC08_Code_ID == "10210007")
  
  
  target_scenario = "rcp85"
  
  Completed_HUCS = HUC08_MRB_LUT$HUC08_Code_ID
  
  
  
  
  huc_zone_lut = Completed_HUCS[1]
  
  

for (huc_zone_lut in Completed_HUCS)
{  # huc


  FIRST = TRUE

  # LOCA Data Extraction from SD Mines Thredds Server

  # URL Information


  loca_location_data = HUC08_MRB_LUT %>%
    filter(HUC08_Code_ID == huc_zone_lut)

  root_LOCA_URL = "http://kyrill.ias.sdsmt.edu:8080/thredds/fileServer/LOCA_NGP/huc_08_basins/problems/"
  root_LOCA_URL = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/huc_08_basins/problems/"

  loca_filename = str_c("NGP_LOCA_HUC08_",
                        huc_zone_lut,
                        ".RData",
                        sep = "")

  LOCA_URL = str_c(root_LOCA_URL,
                   loca_filename,
                   sep = "")

  load(file   = (LOCA_URL))
  
  print(LOCA_URL)
  
  
  remove(LOCA_URL)


      
      missing_time     = which(is.na(loca_daily$Time))
      missing_huc      = which(is.na(loca_daily$Division)) 
      missing_ensemble = which(is.na(loca_daily$Ensemble))
      missing_scenario = which(is.na(loca_daily$Scenario)) 
      missing_precent  = which(is.na(loca_daily$Percentile))
      missing_tasmax   = which(is.na(loca_daily$tasmax))
      missing_tasmin   = which(is.na(loca_daily$tasmin))
      missing_pr       = which(is.na(loca_daily$pr))
      
      
      

      if (length(missing_time)     > 0 ) { print(loca_daily[c(missing_time,missing_time+1), ])  }     
      if (length(missing_huc)      > 0 ) { print(loca_daily[c(missing_huc,missing_huc+1), ])  }     
      if (length(missing_ensemble) > 0 ) { print(loca_daily[c(missing_ensemble,missing_ensemble+1), ])  }     
      if (length(missing_scenario) > 0 ) { print(loca_daily[c(missing_scenario,missing_scenario+1), ])  }     
      if (length(missing_precent)  > 0 ) { print(loca_daily[c(missing_precent,missing_precent+1), ])  }     
      if (length(missing_tasmax)   > 0 ) { print(loca_daily[c(missing_tasmax,missing_tasmax+1), ])  }     
      if (length(missing_tasmin)   > 0 ) { print(loca_daily[c(missing_tasmin,missing_tasmin+1), ])  }     
      if (length(missing_pr)       > 0 ) { print(loca_daily[c(missing_pr,missing_pr+1), ])  }     
      


} # huc
