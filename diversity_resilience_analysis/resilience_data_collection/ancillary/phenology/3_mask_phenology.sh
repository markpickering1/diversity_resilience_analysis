#!/bin/bash

# callf or initialisation script
source main/initialise_bash.sh

####################################################
## mask the phenology                                                                   
####################################################
# algorithm description
# 1) set working directories, select which product to mask (greenup, dormancy or length)
# 2) check if ouput directories exist if not create them
# 3) mask the file

# 1) set working directories
in_dir=${ROOT_DATA_INPUT}ancillary/phenology/modis_phenology_nc/
in_mask_dir=${ROOT_DATA_INPUT}static_variables/hansen/hansen_forest_cover_nc_mask/
out_dir=${ROOT_DATA_INPUT}ancillary/phenology/modis_phenology_nc_masked/
#in_file="modis_lcd_greenup_1_circular_mean.nc"
#in_file="modis_lcd_dormancy_1_circular_mean.nc"
in_file="modis_lcd_length_1_mean.nc"
in_mask="hansenForestCoverNoLoss2000AtModisMean_mask50.nc"

# 2) check if ouput directories exist if not create them
if [ ! -d ${out_dir} ]; then
  mkdir ${out_dir}
  fi

# 3) mask the file
cdo ifthen ${in_mask_dir}${in_mask} ${in_dir}${in_file} ${out_dir}${in_file%.nc}_masked50.nc


