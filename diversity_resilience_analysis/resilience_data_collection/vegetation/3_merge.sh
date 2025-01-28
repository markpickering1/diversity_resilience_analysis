#!/bin/bash

# callf or initialisation script
source main/initialise_bash.sh

##############################################
## merge a list of nc files                                                                       
##############################################
# algorithm description
# 1) set working directories
# 2) check if ouput directory exists if not create it
# 3) merge all yearly nc files from 2003 to 2021

# 1) set working directories and file names
in_dir=${ROOT_DATA_INPUT}vegetation/8day/kNDVI/kNDVI_nc/
out_dir=${ROOT_DATA_INPUT}vegetation/8day/kNDVI/kNDVI_nc_merged/
out_file="merged_kndvi_2003_2021.nc"

# 2) check if ouput directory exists if not create it
if [ ! -d ${out_dir} ]; then
	mkdir ${out_dir}
	fi

# 3) merge all yearly nc files from 2003 to 2021
cd ${in_dir}
cdo mergetime *.nc ${out_dir}${out_file}
	
	