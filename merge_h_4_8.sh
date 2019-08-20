

zones=(  "3202" "3203" "3204" "3205" "3206" "3207" "3208" "3209" "3401" "3402" "3403" "3404" "3405" "3406" "3407" "3408" "3409" )


for zone in ${zones[@]}; do
  echo $zone

  sed 1d ./NGP_LOCA_nCLIMDIV_${zone}_rcp45.csv > ./NGP_LOCA_nCLIMDIV_${zone}_rcp45.csv4
  sed 1d ./NGP_LOCA_nCLIMDIV_${zone}_rcp85.csv > ./NGP_LOCA_nCLIMDIV_${zone}_rcp85.csv4
  cat ./NGP_LOCA_nCLIMDIV_${zone}_historical.csv ./NGP_LOCA_nCLIMDIV_${zone}_rcp45.csv4 ./NGP_LOCA_nCLIMDIV_${zone}_rcp85.csv4 >& ./NGP_LOCA_nCLIMDIV_${zone}.csv

  ls -altr ./NGP_LOCA_nCLIMDIV_${zone}*.csv


done
