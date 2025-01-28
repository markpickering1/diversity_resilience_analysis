# ########################################################
# Title         : plot_residuals.R
# Description   : from a series of rf models and selected predictor variables
#                 calculate, save and plot the residuals of the kNDVI TAC from the 
#                 rf model (plot both as a map and in climate space)
# Aims          : plot RF kndvi TAC residuals
# Inputs	      : dataframes and models
# Outputs	      : plots
# Date          : 10/7/23
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

# load background climate (bc) file
load(paste0(input_dir_bc, input_file_bc)) # head(df_comb)  ;  summary(df_comb)

# create df of background climate
df_bc <- df_comb[, c("x", "y", "tp_mean", "t2m_mean")]

# load input file and 
load(paste0(input_dir, input_file)) # head(df_comb)  ;  summary(df_comb)

# keep only complete cases
if(b_completeCases_for_fullDF) df_comb <- df_comb[complete.cases(df_comb), ]

# add a column identifier of row identifier
df_comb$row_id <- as.numeric(rownames(df_comb))

#################################################################
######    CALCULATE PREDICTIONS AND PLOT RESIDUALS          #####
#################################################################

# loop over 'diversity metrics' to load specific df and rf model
for (i in 1:length(v_optional_predictors)){  #i <- 2
  var_name_i <- v_optional_predictors[i] ; print(var_name_i)
  
  # load train data
  load(paste0(input_dir_rf, 'df_comb.train_div-', var_name_i, '.RData') )
  # load test data
  load(paste0(input_dir_rf, 'df_comb.test_div-', var_name_i, '.RData') )
  
  # add a column identifier for train and test and a numeric of row identifier
  df_comb.train_i$split <- 'train'
  df_comb.train_i$row_id <- as.numeric(rownames(df_comb.train_i))
  df_comb.test_i$split  <- 'test'
  df_comb.test_i$row_id <- as.numeric(rownames(df_comb.test_i))
  
  # rejoin train and test
  df_comb_i <- rbind(df_comb.train_i, df_comb.test_i)
  
  # sort by row_id
  df_comb_i <- df_comb_i %>% arrange(row_id)
  
  # select respective rf model
  load(paste0(input_dir_rf, 'rf_model_div-', var_name_i, '.RData'  ) )
  
  # run predictions
  df_comb_i$predicted_kndvi_TAC <- predict(rf.model, df_comb_i)
  df_comb_i <- df_comb_i %>% mutate(residuals_kndvi_TAC = (kndvi_TAC - predicted_kndvi_TAC))
  
  # add the x,y
  df_comb_i <- cbind(df_comb[,c(1,2)], df_comb_i)
  
  # save new df with residuals
  save(df_comb_i, file=paste0(output_path, 'df_all_div-', var_name_i, '_residuals.RData'))
  
  # create df with residuals and bc
  df_comb_i_bc <- left_join(df_comb_i[,c("x", "y", "split", "residuals_kndvi_TAC")], df_bc)
  
  # select only the testing part
  df_comb_i_bc_test <- df_comb_i_bc[df_comb_i_bc$split=="test",]
  
  ####### PLOT IN CLIMATE SPACE ALL RESIDUALS               #######
  
  # call the function to plot a variable in climate space
  g_clim_space <- make_plot_clim_space(df_comb_i_bc, x, y, var_x, var_y, var_z, x_label, y_label, paste0(z_label, " using ", var_name_i), low_limit, up_limit)
  # save plot
  ggsave(plot = g_clim_space, filename = paste0(output_path, 'g_clim_space_', var_name_i,  '_residuals_all.png' ), width = 10, height = 8)

  ####### SPATIALLY MAP ALL RESIDUALS                       ####### 
  # make a map of the residuals
  g_input <- make_map(df_comb_i_bc, var_z, "Residuals" , paste0(z_label, " using ",  var_name_i), c(low_limit, up_limit))
  
  # save plot
  ggsave(plot = g_input, filename = paste0(output_path, 'g_map_', var_name_i,  '_residuals_all.png' ), width = 10, height = 10)
  
  # add histogram to map
  lims_h_i <-  c(low_limit_hist, up_limit_hist  )
  h_dist <- make_hist(df_comb_i_bc, var_z, "Residuals" , lims_h_i)
  g_draw <- f_combine_map_hist (g_input, h_dist)
  ggsave(plot = g_draw, filename = paste0(output_path, 'g_comb_', var_name_i,  '_residuals_all.png' ) , width = fig_width, height = fig_height ) # , width = wid, height = hei)
  
  ####### SPATIALLY MAP TEST RESIDUALS                     ####### 
  
  # call the function to plot a variable in climate space only for the test data
  g_clim_space <- make_plot_clim_space(df_comb_i_bc_test, x, y, var_x, var_y, var_z, x_label, y_label, paste0(z_label, " using ", var_name_i), low_limit, up_limit)
  # save plot
  ggsave(plot = g_clim_space, filename = paste0(output_path, 'g_clim_space_', var_name_i,  '_residuals_test.png' ), width = 10, height = 8)
  
  ####### SPATIALLY MAP ALL RESIDUALS                       ####### 
  
  # make a map of the residuals only for the test data
  g_input <- make_map(df_comb_i_bc_test, var_z, "Residuals" , paste0(z_label, " using ",  var_name_i), c(low_limit, up_limit))
  # save plot
  ggsave(plot = g_input, filename = paste0(output_path, 'g_map_', var_name_i,  '_residuals_test.png' ), width = 10, height = 10)  

  # and the combined figures
  h_dist <- make_hist(df_comb_i_bc_test, var_z, "Residuals (test)" , lims_h_i)
  g_draw <- f_combine_map_hist (g_input, h_dist)
  ggsave(plot = g_draw, filename = paste0(output_path, 'g_comb_', var_name_i,  '_residuals_test.png' ) , width = fig_width, height = fig_height ) # , width = wid, height = hei)
  
} # end loop over variables

#######################################
##### END                         #####
#######################################

# # loop over 'diversity metrics' to load specific df and rf model
# for (i in 1:length(v_optional_predictors)){ 
#   var_name_i <- v_optional_predictors[i] ; print(var_name_i)
#   
#   # select only the relevant predictors
#   if ( var_name_i == "no_diversity") { v_all_vars <- c( v_identifiers, v_target, v_predictors)
#   }else{ v_all_vars <- c( v_identifiers, v_target, var_name_i, v_predictors) }  # v_identifiers
# 
#   # select only those columns and complete rows
#   df_comb_i <- df_comb[, v_all_vars]
#   df_comb_i <- df_comb_i[complete.cases(df_comb_i), ]
# 
#   # save selected df
#   save(df_comb_i, file=paste0(output_path, 'df_all_div-', var_name_i, '.RData'))
# 
#   # select only target and predictor
#   df_comb_residuals <- df_comb_i[, 3:length(df_comb_i)]
# 
#   # run predictions
#   df_comb_residuals$predicted_kndvi_TAC <- predict(rf.model, df_comb_residuals)
#   df_comb_residuals <- cbind(df_comb_i[,c(1,2)], df_comb_residuals)
#   df_comb_residuals <- df_comb_residuals %>% mutate(residuals_kndvi_TAC = (kndvi_TAC - predicted_kndvi_TAC))
#   
#   # save new df with residuals
#   save(df_comb_residuals, file=paste0(output_path, 'df_all_div-', var_name_i, '_residuals.RData'))
#   
#   # create df with residuals and bc
#   df_comb_residuals_bc <- left_join(df_comb_residuals[,c("x", "y", "residuals_kndvi_TAC")], df_bc)
#   
#   # call the function to plot a variable in climate space (specifying all three)
#   g_clim_space <- make_plot_clim_space(df_comb_residuals_bc, x, y, var_x, var_y, var_z, x_label, y_label, paste0(z_label, " using ", var_name_i), low_limit, up_limit)
#   
#   # save plot
#   ggsave(plot = g_clim_space, filename = paste0(output_path, 'g_clim_space_', var_name_i,  '_residuals.png' ), width = 10, height = 10)
#   
#   # make a map of the residuals
#   g_input <- make_map(df_comb_residuals_bc, var_z, "Residuals" , paste0(z_label, " using ",  var_name_i), c(low_limit, up_limit))
#   
#   # save plot
#   ggsave(plot = g_input, filename = paste0(output_path, 'g_map_', var_name_i,  '_residuals.png' ), width = 10, height = 10)
#   
# } # end loop over variables
