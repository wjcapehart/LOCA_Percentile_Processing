

  library(package = "stringr")
  library(package = "forcats")
  library(package = "readr")
  library(package = "tidyverse")
  library(package = "lubridate")
  library(package = "RCurl")


directory = "./swat_output/"


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


for (ens in Ensembles) {
  target_directory = str_c("./", ens, "/",    sep="")
  tarball          = str_c("./", ens, ".tar", sep="")
  
  bundle_me = str_c("tar -cvf ",
                    tarball, " ",
                    target_directory,
                    sep = "")
  
 smoosh_me = str_c("gzip -9v ", tarball)
 
 print(bundle_me)
 print(smoosh_me)
 
}
