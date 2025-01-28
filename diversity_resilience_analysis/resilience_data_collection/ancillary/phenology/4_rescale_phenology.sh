#!/bin/bash

# callf or initialisation script
source main/initialise_bash.sh

##############################################
## rescale a netCDF file to lower resolution                                                                        
##############################################
# algorithm description
# 1) set working directories, select which product to rescale (greenup, dormancy or length)
# 2) check if ouput directories exist if not create them
# 3) rescale data

# 1) set working directories
in_dir=${ROOT_DATA_INPUT}ancillary/phenology/modis_phenology_nc_masked/
in_grid_dir=ancillary/phenology/targetgrid/
out_dir=${ROOT_DATA_INPUT}ancillary/phenology/modis_phenology_nc_masked_005/
#in_file="modis_lcd_greenup_1_circular_mean_masked50.nc"
#in_file="modis_lcd_dormancy_1_circular_mean_masked50.nc"
in_file="modis_lcd_length_1_mean_masked50.nc"
in_grid="targetgrid_005.txt"

# 2) check if ouput directory exists if not create it
if [ ! -d ${out_dir} ]; then
  mkdir ${out_dir}
  fi

# 2) rescale data
cdo remapcon,${in_grid_dir}${in_grid} ${in_dir}${in_file} ${out_dir}${in_file%.nc}_005.nc





