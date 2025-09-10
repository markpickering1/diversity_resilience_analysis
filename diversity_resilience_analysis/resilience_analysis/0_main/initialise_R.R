# ########################################################
# Title         : initialise_R.R
# Description   : This script initialises certain environmental variables in order to run the analysis R code. 
#                 Script should be sourced at the start of the R files in order to
#                 standardise the location of the I/O area and other variables
#                 
# Date          : 19/2/25
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################

print('initialise')

###################################################
######     INITIALISE INPUT INFO              #####
###################################################

# set working directory to GIT code area - currently set to
# setwd(root_project)

# load user input variables for setting program structure
source('0_main/input/input_initialise_R.R')

# set path of figures initialisation script
path_figures_init <- '0_main/input/initialise_figs.R'


###################################################
######     CREATE COMMON FILE STRUCTURE       #####
###################################################

# dir input data
root_data_input   <- paste0(root_data, "data/")
# create input_data if not present
if(! dir.exists(root_data_input)) {dir.create(paste0(root_data_input),recursive=T) ; 
  print( paste0( 'creating data input dir : ', root_data_input ) ) }

# dir processed data output
root_data_proce   <- paste0(root_data, "data_processing/", analysis_version, '/')
# create data_processing directory for output data if not present
if(! dir.exists(root_data_proce)) {dir.create(paste0(root_data_proce),recursive=T) ; 
  print( paste0( 'creating data_processing ouput dir : ', root_data_proce ) )}

# dir figures output
root_data_figs   <- paste0(root_data, "figures/", analysis_version, '/')
# create data_processing directory for output data if not present
if(! dir.exists(root_data_proce)) {dir.create(paste0(root_data_proce),recursive=T) ; 
  print( paste0( 'creating data_processing ouput dir : ', root_data_proce ) )}

###################################################
######     COMMON CODE                        #####
###################################################

# extract time and date of initialisation step
t_start_time <- Sys.time() ; print(t_start_time)      # initialise time
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ")
full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]

###################################################
######     COMMON FUNCTIONS                   #####
###################################################

# print out the time period between current time and the reference_time
f_time_update <- function(reference_time){
  t_end_time <- Sys.time() #; print(t_end_time)     
  t_duration <- t_end_time - reference_time 
  print(t_end_time)
  print(t_duration)
}

# short function to add a string (string_2) into another string (string_1) at the final position
# that a third string (e.g. a period [default]) is found
f_inst_str_b4_expr <- function(string_item, string_insert, expr_find = "\\.") {
  last_period_index <- max(gregexpr(expr_find, string_item)[[1]])
  if (last_period_index > 0) {
    modified_string <- paste0( substr(string_item, 1, last_period_index - 1), string_insert, substr(string_item, last_period_index, nchar(string_item)))
    return(modified_string)
  } else {
    return(string_item)
  }
}

