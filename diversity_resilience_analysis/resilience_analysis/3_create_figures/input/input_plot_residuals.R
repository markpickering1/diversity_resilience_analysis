# ########################################################
# Title         : input_plot_residuals.R
# Description   : This text script acts as a user input to plot_residualsl.R
#                 By setting variables in this file, the user should not need to edit the main code
#                 This file should not change once initialised, except when running full separate
#                 analysis after major changes
# Date          : 10/7/23
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################


###################################################
######     I/O                                #####
###################################################
# set output path name - with date this will link output (variables) to next script
script_output_ext <- 'combineDF_plots_residuals_climate_space'           # time-series stats     

# input dataset containing the dataframes used for training the rf models and the respective rf models
# date_production <- 'sv1_2023-07-08_rerunTPSSR' # revised tp and ssr
# date_production_rf <- '2023-07-08_mntry7_ntree500' 
# date_production_bc <- 'sv1_2023-07-08_rerunTPSSR_fullAnnualVersion'
# input_dir <- paste0(root_data_proce, '2_ts_statistics_', date_production, '/') 
# input_dir_rf <- paste0(root_data_proce, 'createRF_', date_production_rf, '/')
# input_dir_bc <-  paste0(root_data_proce, '2_ts_statistics_', date_production_bc, '/') 
# input_file <- 'df_all_GSonly.RData' 
# input_file_bc <- 'df_all_fullAnnual.RData'

date_production_rf <- 'bootDiv_metrics_preselectTestTrain_2024-03-03_noXYTS' # bootstrapped rf models with same train test for all corrected for x and y in the training df
input_dir_rf <- paste0('/eos/jeodpp/data/projects/FOREST-RESILIENCE/data_processing/version_3_Aug23/2023-11-08_alignment/', 'createRF_', date_production_rf, '/') # for the bs rf models


#####################################################
###### SELECT VARIABLES OF INTEREST IN ANALYSIS #####
#####################################################
# choose identifier, target and predictor variables
# some predictor variables are used in all RF models, other 'optional' variables
# are cycled over an included in only one RF model each time (this applies to the 
# diversity metrics, including 'no_diversity' metric)

# target variable to predict 
# v_target <- 'kndvi_TAC'  # kndvi_CV
v_target <- 'kndvi_lambda_xt'
v_identifiers <- c('x', 'y')

# variables to go as predictor in every model
v_predictors <- c( 'kndvi_mean', 
                   'socc30cm', # soil carbon content
                   'forestcover', # previously: 'mu_treecover',
                   'topology_elevation_std', # topology metric # topology_elevation_mean topology_slope_mean topology_slope_std
                   'ssr_mean', 'ssr_CV', 'ssr_TAC',
                   't2m_mean', 't2m_CV', 't2m_TAC', 
                   'tp_mean', 'tp_CV', 'tp_TAC',
                   'VPD_mean', 'VPD_CV', 'VPD_TAC'
                   # 'mu_spei', 'cv_spei', 'tac_resid_spei' # shouldn't really use this as predictor - use VPD instead
)

# add biodiversity variables to loop over and add to separate models
v_optional_predictors <- c("shannon_entropy")

###################################################
######     SET RF PARAMETERS                  #####
###################################################
# need to do the test train split by each diversity group and removing NAs

# only use the full dataframe for all variables
b_completeCases_for_fullDF <- T

# choose which dataset to use
s_train_test_all <- 'test'

#######################################
##### SELECT VARIABLES TO BIN     #####
#######################################
# here chose which variables to use to create a climate space and which variable to bin in it
x <- "x"
y <- "y"
var_x <- "tp_mean"
var_y <- "t2m_mean"
# var_z <- "residuals_kndvi_TAC"
var_z <- "residuals_resilience"

# define name of plot labels
x_label <- "Precip. mean [mm/day]"
y_label <- "2m temp mean [°C]"
# z_label <- "Residuals of kNDVI TAC from RF"
z_label <- "AC1 residuals"

# define limits
# var_z_lim <- c(-0.1, 0.1)
var_z_lim <- c(-0.08, 0.08)
#  END