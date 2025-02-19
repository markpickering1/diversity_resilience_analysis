# ########################################################
# Title         : initialise_R.R
# Description   : This script initialises certain environmental variables in order to run the analysis R code. Script should be sourced at the start of the R files in order to set the working directory and standardise the location of the I/O area
# Date          : 26/01/23
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes         : 
# ########################################################

###################################################
######     COMMON FILE STRUCTURE              #####
###################################################

# dir root of analysis
root_project <- "FOREST-RESILIENCE/"

# dir of input data
root_data_input <- paste0(root_project, "data/")

###################################################
######     COMMON CODE                        #####
###################################################

# extract time and date
start_time <- Sys.time() ; print(start_time)      # initialise time
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ")
full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]

