

# Libraries

  # Tidyverse resources

  library(package = "tidyverse") # Multiple Tidyverse Resources
  library(package = "lubridate") # Date-Time Control

  # NCAR Libraries




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

  Ensembles2 = c("ACCESS1-0",
                "ACCESS1-3",
                "CCSM4",
                "CESM1-BGC",
                "CESM1-CAM5",
                "CMCC-CMS",
                "CMCC-CM",
                "CNRM-CM5",
                "CSIRO-Mk3-6-0",
                "CanESM2",
                "FGOALS-g2",
                "GFDL-CM3",
                "GFDL-ESM2G",
                "GFDL-ESM2M",
                "HadGEM2-AO",
                "HadGEM2-CC",
                "HadGEM2-ES",
                "IPSL-CM5A-LR",
                "IPSL-CM5A-MR",
                "MIROC-ESM",
                "MIROC-ESM-CHEM",
                "MIROC5",
                "MPI-ESM-LR",
                "MPI-ESM-MR",
                "MRI-CGCM3",
                "NorESM1-M",
                "bcc-csm1-1-m")





  Completed_HUCS = HUC08_MRB_LUT$HUC08_Code_ID




huc_zone_lut = Completed_HUCS[1]

root_LOCA_URL = "/maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/"


for (huc_zone_lut in Completed_HUCS)
{  # huc




  loca_location_data = HUC08_MRB_LUT %>%
    filter(HUC08_Code_ID == huc_zone_lut)



  loca_filename = str_c("NGP_LOCA_HUC08_",
                        huc_zone_lut,
                        ".RData",
                        sep = "")


   print(loca_filename)


  LOCA_URL = str_c(root_LOCA_URL, "huc_08_basins/",
                   loca_filename,
                   sep = "")


  load(file    = LOCA_URL,
       verbose = TRUE)

  levels(loca_daily$Ensemble) = Ensembles2


 LOCA_URL = str_c(root_LOCA_URL,
                  loca_filename,
                  sep = "")

  save(loca_daily, file    = LOCA_URL,
                   verbose = TRUE)








} # huc



print("Outahere like Vladimir")
