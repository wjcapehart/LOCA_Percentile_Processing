#!/bin/bash
cd /maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_Percentile_Processing

nohup gzip -vfr /maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_NGP/huc_08_basins/done/*.csv >& post_gzip_huc.txt
nohup Rscript /maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_Percentile_Processing/Convert_HUC_from_CSVrcp_to_R.R >& post_huc_csv_to_rdata.txt
nohup Rscript /maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_Percentile_Processing/Combine_HUC_RdataRCP.R >& post_huc_merge.txt
nohup Rscript /maelstrom2/LOCA_GRIDDED_ENSEMBLES/LOCA_Percentile_Processing/LOCA_Extreme_Event_Analysis_Offline_Loop_HUC.R >& post_huc_extreme.txt
