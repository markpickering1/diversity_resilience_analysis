# ########################################################
# Title         : input_plot_predictions.R
# Description   : This text script acts as a user input to plot_residualsl.R
#                 By setting variables in this file, the user should not need to edit the main code
#                 This file should not change once initialised, except when running full separate
#                 analysis after major changes
# Date          : 2025-02-19
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################


###################################################
######     I/O                                #####
###################################################
# set output path name - with date this will link output (variables) to next script
script_output_ext <- 'combineDF_plots_predictions'           # time-series stats     

# input dataset containing the dataframes used for training the rf models and the respective rf models
# and a df to merge x,y
date_production <- '2025-02-19_rerunTPSSR' # revised tp and ssr
date_production_rf <- '2025-02-19_mntry7_ntree500' 
input_dir <- paste0(root_data_proce, '2_ts_statistics_', date_production, '/') 
input_dir_rf <- paste0(root_data_proce, 'createRF_', date_production_rf, '/')
input_file <- 'df_all_GSonly.RData' 

#####################################################
###### SELECT VARIABLES OF INTEREST IN ANALYSIS #####
#####################################################
# choose identifier, target and predictor variables
# some predictor variables are used in all RF models, other 'optional' variables
# are cycled over an included in only one RF model each time (this applies to the 
# diversity metrics, including 'no_diversity' metric)

# target variable to predict 
v_target <- 'kndvi_TAC'  # kndvi_CV
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
v_optional_predictors <- c( "no_diversity",
                            "shannon_entropy", "simpson_index", "rao_quadratic_entropy",
                            "euclidean_distances_mean", "convex_hull_volume",      
                            "rh50_mean", "rh98_mean", "fhd_mean", 
                            "skew_mean", "kurt_mean"
)

###################################################
######     SET RF PARAMETERS                  #####
###################################################
# need to do the test train split by each diversity group and removing NAs

# only use the full dataframe for all variables
b_completeCases_for_fullDF <- T

######################################################
##### SELECT INCREMENT AND VARIABLES TO PLOT     #####
######################################################
# define the increment to apply to the predictor
increment <- 0.1

# variables names and full names to plot
var_pre <- "predicted_kndvi_TAC_inc"
var_pre_full_name <- "kNDVI TAC"
var_pre_diff <- "difference_kndvi_TAC"
var_pre_diff_full_name <- "Difference kNDVI TAC"
var_pre_perc <- "perc_change_kndvi_TAC"
var_pre_perc_full_name <- "Percentage change kNDVI TAC"

# define plot labels
var_pre_label <- paste0("Predicted kNDVI TAC with ", increment*100, "% increase")
var_pre_diff_label <- paste0("Difference in kNDVI TAC predictions with ", increment*100, "% increase")
var_pre_perc_label <- paste0("Percentage change in kNDVI TAC with ", increment*100, "% increase")

# define limits of maps and histograms
limits_map_pre <- c(0.0, 0.6)
limits_hist_pre <- c(-0.2, 0.8)
limits_map_pre_diff <- c(-0.009, 0.009)
limits_hist_pre_diff <- c(-0.02, 0.02)
limits_map_pre_perc <- c(-2.5, 2.5)
limits_hist_pre_perc <- c(-20, 20)

#  END