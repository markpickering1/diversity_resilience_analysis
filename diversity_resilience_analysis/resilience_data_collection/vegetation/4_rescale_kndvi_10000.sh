#!/bin/bash

# callf or initialisation script
source main/initialise_bash.sh

##############################################
## bring back kndvi to native scale dividing by 10000                                                                       
##############################################
# algorithm description
# 1) set working directories
# 2) check if ouput directory exists if not create it
# 3) rescale kndvi values

# 1) set working directories
in_dir=${ROOT_DATA_INPUT}vegetation/8day/kNDVI/kNDVI_nc_merged/
out_dir=${ROOT_DATA_INPUT}vegetation/8day/kNDVI/kNDVI_nc_merged_rescaled10/
in_file="merged_kndvi_2003_2021.nc"
out_file="merged_kndvi_2003_2021_rescaled10.nc"

# 2) check if ouput directory exists if not create it
if [ ! -d ${out_dir} ]; then
  mkdir ${out_dir}
  fi

# 3) rescale kndvi values
cdo divc,10000  ${in_dir}${in_file} ${out_dir}${out_file}