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

# dir root of analysis code
# this is manually set to code/ directory of git - currently set by default to code/
# used to create backups of configuration setups copied to the output directories
root_project <- paste0(getwd(), '/')

# dir root of analysis data
# this should be set the root name of the directory for data input/outputs
# root_data         <- paste0(root_project, "resilience_data_collection/")  # local directory
root_data <- normalizePath(file.path(getwd(), "..", "resilience_data_collection/")) # local directory

# dir input data
root_data_input   <- paste0(root_data, "/")

# analysis version info
# this sets a version on the code used to produce the main analysis dataframes
# output data/figs will be stored in subfolder of this version 
analysis_version <- 'version_1' 

###################################################
######     SET INDIVIDUAL INPUT FILE DATASETS #####
###################################################
# set the extensions of the individual input files of kndvi, climate etc datasets
# datasets must be in the previously set root_data/data directory

# land shapefiles
input_land_shapefile <- paste0(root_data_input, 'ancillary/world_vectors/50m_coastline/ne_50m_coastline.shp')
