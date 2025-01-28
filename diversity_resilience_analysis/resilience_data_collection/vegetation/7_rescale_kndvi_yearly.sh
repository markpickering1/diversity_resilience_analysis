#!/bin/bash

# callf or initialisation script
source main/initialise_bash.sh

##############################################
## rescale a netCDF file to lower resolution                                                                        
##############################################
# algorithm description
# 1) set working directories, selecting between deseasonalised or raw time-series
# 2) check if ouput directories exist if not create them
# 3) rescale data
# 4) merge yearly files

# 1) set working directories
#in_dir=${ROOT_DATA_INPUT}vegetation/8day/kNDVI/kNDVI_nc_merged_rescaled10_masked_deseasonalised/
in_dir=${ROOT_DATA_INPUT}vegetation/8day/kNDVI/kNDVI_nc_merged_rescaled10_masked/
in_grid_dir=vegetation/targetgrid/
#out_dir=${ROOT_DATA_INPUT}vegetation/8day/kNDVI/kNDVI_nc_merged_rescaled10_masked_deseasonalised_005/
#out_dir_merged=${ROOT_DATA_INPUT}vegetation/8day/kNDVI/kNDVI_nc_merged_rescaled10_masked_deseasonalised_005/merged/
out_dir=${ROOT_DATA_INPUT}vegetation/8day/kNDVI/kNDVI_nc_merged_rescaled10_masked_005/
out_dir_merged=${ROOT_DATA_INPUT}vegetation/8day/kNDVI/kNDVI_nc_merged_rescaled10_masked_005/merged/
#in_file="merged_kndvi_2003_2021_rescaled10_masked50_deseasonalised.nc"
in_file="merged_kndvi_2003_2021_rescaled10_masked50.nc"
in_grid="targetgrid_005.txt"

# 2) check if ouput directory exists if not create it
if [ ! -d ${out_dir} ]; then
  mkdir ${out_dir}
  fi

if [ ! -d ${out_dir_merged} ]; then
  mkdir ${out_dir_merged}
  fi

# 3) rescale data
start_y=2020
end_y=2020

for year in $(seq $start_y $end_y) ; do
	echo $year
	cdo -remapcon,${in_grid_dir}${in_grid} -selyear,${year} ${in_dir}${in_file} ${out_dir}${year}_${in_file%.nc}_005.nc
done

# 4) merge yearly files
cd ${out_dir}
cdo mergetime *.nc ${out_dir_merged}${in_file%.nc}_005.nc