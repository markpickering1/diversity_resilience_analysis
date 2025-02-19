# ########################################################
# Title         : plot_residuals.R
# Description   : from a series of rf models and selected predictor variables
#                 calculate, save and plot the residuals of the kNDVI TAC from the 
#                 rf model (plot both as a map and in climate space)
# Aims          : plot RF kndvi TAC residuals
# Inputs	      : dataframes and models
# Outputs	      : plots
# Date          : 2025-02-19
# Version       : 1
# Authors       : Mark Pickering & Agata Elia
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())            

# set initialisation script names
main_initialisation_file   <- '0_main/initialise_R.R'
script_config_dir          <- '3_create_figures/input/'    ;   script_config_file <- 'input_plot_residuals.R'

# initialise code setup and repo building
source(main_initialisation_file)
# initialise figure common formatting for code base
source('0_main/initialise_figs.R')
# load common plotting functions
source('0_main/functions/plotting_functions.R')
# initialise user inputs (config) to script
source( paste0( script_config_dir, script_config_file) )

######     SET LIBRARIES                      #####
# library(chron)   # useful for converting time values
library(dplyr)   # use %>%
# RF model
library(randomForest) # for random forest regressions and ML
library(dplyr)        # use %>%
# library(reshape)      # reshaping dataframes
require(ggplot2)      # for plotting
require(scales)       # for ggplot2 functions eg oob & trans
# library(ggpubr)       # for arranging ggplots together (ggarrange)
# library(ggExtra)      # more complex figures, marginal hists
# library(grid)         # for making multi-gird plots and tables
# library(gridExtra)    # for making multi-gird plots and tables
# library(lattice)      # for making multi-gird plots and tables
library(RColorBrewer) # colour palettes
library(sf)           # utilise shapefiles etc
library(cowplot)      # for ggdraw

###################################################
######       I/O                              #####
###################################################

# output location
# set/create output directory
output_path <- paste0(root_data_figs, script_output_ext, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for figures of dataframe cols : ', output_path ) ) }

# copy configuration input variables to output for storage
if( ! file.exists( paste0(output_path, script_config_file ) ) ) { print('copy config file') 
  file.copy(from= paste0( script_config_dir, script_config_file) , to= paste0(output_path, script_config_file ), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
} else{ print('could not copy config file') }

# load all data (since they are now all in one df)
load(paste0(input_dir_rf, 'df_all.RData') )

# initialize train/test df from the all data
df_comb.train_i <- subset(df_comb, train_sample == T) # head(df_comb.train_i)
df_comb.test_i  <- subset(df_comb, train_sample == F) # head(df_comb.test_i)

# create the pdps using the training, testing or all data
if(s_train_test_all == 'train'){ df_comb <-  df_comb.train_i[complete.cases(df_comb.train_i), ] ; print('using train data') }
if(s_train_test_all == 'test') { df_comb <-  df_comb.test_i[complete.cases(df_comb.test_i), ]   ; print('using test data')  }
if(s_train_test_all == 'all')  { df_comb <-  df_comb_i[complete.cases(df_comb_i), ]             ; print('using all data')   }

#################################################################
######    CALCULATE PREDICTIONS AND PLOT RESIDUALS          #####
#################################################################

# loop over 'diversity metrics' to load specific df and rf model
for (i in 1:length(v_optional_predictors)){  #i <- 2
  var_name_i <- v_optional_predictors[i] ; print(var_name_i)
  v_all_vars <- c(v_target, var_name_i, v_predictors)  ; print(v_all_vars)
  
  # select only relevant predictors without the identifiers to run in model
  df_comb <- df_comb[, v_all_vars]
  
  # select respective rf model
  input_rf_file  <- paste0(input_dir_rf, 'list_of_rfs/list_rf_model_pdp_results_boot_parallel_nIter-20_div-', var_name_i, '_seed-98', '_targ-', v_target, '.RData')
  
  # load the rf models for the specific seed
  load(input_rf_file) 
  l_rf_boot <- results$rf.models
  rf.model <- l_rf_boot[[1]]
  
  # run predictions
  df_comb$predicted_resilience <- predict(rf.model, df_comb)
  df_comb <- df_comb %>% mutate(residuals_resilience = (!!sym(v_target) - predicted_resilience))
  
  ####### PLOT IN CLIMATE SPACE ALL RESIDUALS               #######
  
  # call the function to plot a variable in climate space
  g_clim_space <- make_plot_clim_space(df_comb, var_x, var_y, var_z, x_label, y_label, z_label, var_z_lim)
  # save plot
  ggsave(plot = g_clim_space, filename = paste0(output_path, 'g_clim_space_', var_name_i, '_', v_target, '_residuals_test.png' ), width = fig_width_wide+0.5, height = fig_width_wide)

} # end loop over variables
