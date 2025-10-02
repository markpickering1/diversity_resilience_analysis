#!/bin/bash

# callf or initialisation script
source main/initialise_bash.sh

##############################################
## rescale a netCDF file to lower resolution                                                                        
##############################################
# algorithm description
# 1) set working directories
# 2) check if ouput directories exist if not create them
# 3) rescale data

# 1) set working directories
in_dir=${ROOT_DATA_INPUT}static_variables/hansen/hansen_forest_cover_nc_mask/
in_grid_dir=static_variables/hansen/targetgrid/
out_dir=${ROOT_DATA_INPUT}static_variables/hansen/hansen_forest_cover_nc_mask_005/
in_file="hansenForestCoverNoLoss2000AtModisMean_mask50.nc"
in_grid="targetgrid_005.txt"

# 2) check if ouput directory exists if not create it
if [ ! -d ${out_dir} ]; then
  mkdir ${out_dir}
  fi

# 2) rescale data
cdo remapsum,${in_grid_dir}${in_grid} ${in_dir}${in_file} ${out_dir}${in_file%.nc}_005.nc


