# ########################################################
# Title         : input_plot_climate_space.R
# Description   : This text script acts as a user input to plot_climate_space.R
#                 By setting variables in this file, the user should not need to edit the main code
#                 this script does not contain plotting or styles themes: see initialse_figs.R
# Date          : 5/7/23
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################

###################################################
######     I/O                                #####
###################################################

# set output path name - with date this will link output (variables) to next script
script_output_ext <- 'combineDF_plots_climate_space'           # time-series stats     

# input dataset containing the combined dataframes to plot
# date_production <- 'sv1_2023-05-26'   # first production - 1st v on new system 
# date_production <- 'sv1_2023-06-28'   # second production - add topo, update forest cover
# date_production <- 'sv1_2023-07-08_rerunTPSSR' # revised tp and ssr annual climate with gs kndvi tac
# input_dir <- paste0(root_data_proce, '2_ts_statistics_', date_production, '/') 
# input_file <- 'df_all_GSonly.RData' # df_comb
date_production_rf <- 'bootDiv_metrics_preselectTestTrain_2024-03-03_noXYTS' # bootstrapped rf models with same train test for all corrected for x and y in the training df
input_dir_rf <- paste0('/eos/jeodpp/data/projects/FOREST-RESILIENCE/data_processing/version_3_Aug23/2023-11-08_alignment/', 'createRF_', date_production_rf, '/') # for the bs rf models

# # add file with background climate
# date_production_bc <- 'sv1_2023-07-08_rerunTPSSR_fullAnnualVersion'
# input_dir_bc <-  paste0(root_data_proce, '2_ts_statistics_', date_production_bc, '/') 
# input_file_bc <- 'df_all_fullAnnual.RData'

#######################################
##### SELECT VARIABLES TO BIN     #####
#######################################
# here chose which variables to use to create a climate space and which variable to bin in it
x <- "x"
y <- "y"
var_x <- "tp_mean"   # the first climatic variable
var_y <- "t2m_mean"  # the second climatic variable
# var_z <- "kndvi_TAC" # the variable to be binned in climate space
var_z <- "kndvi_lambda_xt" # the variable to be binned in climate space

# define name of plot labels
x_label <- "Precip. mean [mm/day]"
y_label <- "2m temp mean [°C]"
# z_label <- "Long-term kNDVI TAC over gs (mean in bins)"
z_label <- "Rest. Rate AC1"

# define limits
# var_z_lim <- c(0.0, 0.8)
var_z_lim <- c(-2.5, 0.0)
#low_limit <- 0.0
#up_limit <- 0.8

#  END

