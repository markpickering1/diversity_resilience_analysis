# ########################################################
# Title         : plotRF_initialise.R
# Description   : Common plotRF initialisation and loading of relevant dataframes and models
# Aims          : initialise plotting of RF
# Inputs	      : plot_RF_X.R scripts
# Outputs	      : None
# Options	      : 
# Date          : 2025-02-19
# Version       : 1
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : 
# ########################################################


###################################################
######     INITIALISE                         #####
###################################################

# set initialisation script names
main_initialisation_file   <- '0_main/initialise_R.R'
script_config_dir          <- '3_create_figures/input/'    ;   script_config_file <- 'input_plotRF.R'

# initialise code setup and repo building
source(main_initialisation_file)
# initialise user inputs to script
source( paste0( script_config_dir, script_config_file) )
# initialise figure common formatting for code base
source(path_figures_init)
# load common plotting functions
# source('0_main/functions/plotting_functions.R')
# load RF plotting functions
source('3_create_figures/functions/f_plotRF.R')

######     SET LIBRARIES                      #####
library(dplyr)        # use %>%
library(sf)           # utilise shapefiles etc
require(ggplot2)      # for plotting
library(scales)       # oob
library(cowplot)      # draw plots together
library(gridExtra)    # draw plots together
library(grid) # load the grid package to access nullGrob function
library(stringr)      # string and string padding

# RF model
library(Metrics)      # perf metrics commonly used in supervised ML e.g. rmse
library(randomForest) # for random forest regressions and ML

library(caTools)      # for test/training split
library(ICEbox)       # Individual conditional expectation https://arxiv.org/pdf/1309.6392.pdf https://cran.r-project.org/web/packages/ICEbox/ICEbox.pdf
# https://arxiv.org/pdf/1309.6392.pdf
library(pdp)

# parallelisation
library(doParallel) # run in parallel
library(foreach)    # run in parallel


###################################################
######       I/O                              #####
###################################################

# f_plotRF_IO <- function(script_subtitle){ 
# output location
# set/create output directory
output_path <- paste0(root_data_figs, script_output_ext, '_', full_date, '_', script_subtitle,  '/')
print(paste0('output_path is : ', output_path ))

# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for figures of RF : ', output_path ) ) }

# copy configuration input variables to output for storage
if( ! file.exists( paste0(output_path, script_config_file ) ) ) { print('copy config file') 
  file.copy(from= paste0( script_config_dir, script_config_file) , 
            to= paste0(output_path,  f_inst_str_b4_expr(script_config_file, paste0('_', script_subtitle) ) ), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
} else{ print('could not copy config file') }


