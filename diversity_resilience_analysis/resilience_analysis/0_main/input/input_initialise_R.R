# ########################################################
# Title         : input_initialise_R.R
# Description   : This text script acts as a user input to initialise_R.R
#                 By setting variables in this file, the user should not need to edit the main code
#                 This file should not change once initialised, except when running full separate
#                 analysis after major changes
# Date          : 19/02/25
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################

###################################################
######     SET ROOT DIRECTORIES               #####
###################################################

# analysis version info
# this sets a version on the code used to produce the main analysis dataframes - output data/figs will be stored in subfolder of version
# can set subdirs but do not need to add final '/'
analysis_version <- 'version_3_Aug23/2023-11-08_alignment' # 

# dir root of analysis data
root_data         <- "forest_resilience/data/"  # local directory

# dir root of analysis code
# this is manually set to code/ directory of git - currently set by default to code/
root_project <- paste0(getwd(), '/')

###################################################
######     SET INDIVIDUAL INPUT FILE DATASETS #####
###################################################
# set the extensions of the individual input files of kndvi, climate etc datasets
# datasets should be in the root_data/data directory

# land shapefiles
input_land_shapefile <- '/ancillary/world_vectors/50m_coastline/ne_50m_coastline.shp'
