#!/bin/bash
cd /maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_Percentile_Processing
nohup gzip ../LOCA_NGP/huc_08_basins/done/*.csv
nohup gzip ../LOCA_NGP/climate_divisions/done/*.csv
nohup Rscript ./Convert_HUC_from_CSVrcp_to_R.R
nohup Rscript ./Convert_CD_from_CSVrcp_to_R.R
nohup Rscript ./Combine_HUC_RdataRCP.R
nohup Rscript ./Combine_CD_RdataRCP.R
nohup Rscript ./LOCA_Extreme_Event_Analysis_Offline_Loop_HUC.R
nohup Rscript ./LOCA_Extreme_Event_Analysis_Offline_Loop_CD.R
