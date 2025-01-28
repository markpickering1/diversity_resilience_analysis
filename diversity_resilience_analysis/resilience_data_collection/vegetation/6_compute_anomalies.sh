#!/bin/bash

# callf or initialisation script
source main/initialise_bash.sh

##############################################
## compute anomalies of an input time-series file                                                                       
##############################################
# algorithm description
# 1) set working directories
# 2) check if ouput directories exist if not create them
# 3) calculate climatological average (20 years average for each 8 days timestep)
# 4) subtract climatological average at each time-step to calculate anomalies

# 1) set working directories
in_dir=${ROOT_DATA_INPUT}vegetation/8day/kNDVI/kNDVI_nc_merged_rescaled10_masked/
out_ydaymean_dir=${ROOT_DATA_INPUT}vegetation/8day/kNDVI/kNDVI_nc_merged_rescaled10_masked_ydaymean/
out_dir=${ROOT_DATA_INPUT}vegetation/8day/kNDVI/kNDVI_nc_merged_rescaled10_masked_deseasonalised/
in_file="merged_kndvi_2003_2021_rescaled10_masked50.nc"

# 2) check if ouput directories exist if not create them
if [ ! -d ${out_ydaymean_dir} ]; then
  mkdir ${out_ydaymean_dir}
  fi

if [ ! -d ${out_dir} ]; then
  mkdir ${out_dir}
  fi

# 3) calculate climatological average (20 years average for each 8 days timestep)
#cdo ydaymean ${in_dir}${in_file} ${out_ydaymean_dir}${in_file%.nc}_ydaymean.nc

# 3) break files into slices to better handle TAC calculation 
cdo distgrid,2,2 ${in_dir}${in_file} ${in_dir}${in_file%.nc}.

# 4) calculate climatological average (20 years average for each 8 days timestep)
start_i=0
end_i=3

for i in $(seq $start_i $end_i) ; do

	cdo ydaymean ${in_dir}${in_file%.nc}.0000${i}.nc ${out_ydaymean_dir}${in_file%.nc}_ydaymean.0000${i}.nc ;

done

# 5) merge ydaymean
cdo collgrid  ${out_ydaymean_dir}${in_file%.nc}_ydaymean.00000.nc  ${out_ydaymean_dir}${in_file%.nc}_ydaymean.00001.nc  ${out_ydaymean_dir}${in_file%.nc}_ydaymean.00002.nc  ${out_ydaymean_dir}${in_file%.nc}_ydaymean.00003.nc  ${out_ydaymean_dir}${in_file%.nc}_ydaymean.nc

# 6) subtract climatological average at each time-step to calculate anomalies
cdo ydaysub ${in_dir}${in_file} ${out_ydaymean_dir}${in_file%.nc}_ydaymean.nc ${out_dir}${in_file%.nc}_deseasonalised.nc




