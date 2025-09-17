# ########################################################
# Title         : input_createPDP_fromModel.R
# Description   : This text script acts as a user input to createPDP_fromModel.R
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
# the loading process assumes that there are one or several subdirectories of RDATA objects
# containing 'results' which incorporate rf models and indices used to create them
# the subdirectories are generally separate runs of the rf production that use different random seeds
# in the case where no subdirectories are used, then input_subdir <- c('') & the dir is the 

# input dataset containing dataframe used in model
input_dataframe <-  '4_selections'
input_df <- paste0(root_data_proce, input_dataframe, '/')

# the main directory used for the oreviously output bootstrapped results models
input_dir <- 'createRF_model/' # '4_selections/'

# list of the subdirectories (if stored in subdirs, e.g. if 5 runs stored in 5 subdirs, see below)
# input_subdir           <- c('createRF_98/', 'createRF_99/', 'createRF_100/', 'createRF_101/',  'createRF_102/') 
input_subdir           <- c('') # ,'','','','')
# list of the seeds used in each subdir production (should correspond to the dir order)
l_seed                 <- c(99) # e.g c(98,99,100,101,102)
# list of the niterations used in each subdir productions (should correspond to the dir order)
l_iter                 <- c(20) # e.g. c(20,20,20,20,20)

# prefix of name of results object to load
f_results_name <- 'list_rf_model_pdp_results_boot_parallel'                       

#####################################################
###### SELECT VARIABLES OF INTEREST IN ANALYSIS #####
#####################################################
# choose predictor variables to turn into pdp

# v_target <- c( 'kndvi_TAC', 'kndvi_lambda_xt', 'kndvi_lambda_variance')
v_target <- c(  'kndvi_lambda_xt', 'kndvi_lambda_variance')

# v_optional_predictor - the diversity predictor used in the model
# these pdps should have been produced in previous step
v_optional_predictor <-  "Kurtosis" # "Shannon" "Canopy_heights_sd"
  # 'sd_rh98' 'mu_kurt' "shannon_entropy" "euclidean_distances_mean",
  # "rh50_mean", "rh98_mean", "fhd_mean" "skew_mean", "kurt_mean",
  # 'mu_rh98' 'no_diversity' "simpson_index" "rao_quadratic_entropy" "convex_hull_volume"


# variables to test partial dependence in every model 
# these pdps have not yet been produced and can be producde if (b_run_basic == F)
v_predictors <- c( "Kurtosis" )
  # "Shannon" "Canopy_heights_sd"
  # 'kndvi_mean' # 'socc30cm' # 'forestcover', 
  # 'topology_elevation_std', # topology_elevation_mean topology_slope_mean topology_slope_std
  # 't2m_mean', #'t2m_CV', 't2m_TAC',  'ssr_mean', 'ssr_CV', 'ssr_TAC',
  # 'tp_mean'  'tp_CV', 'tp_TAC' 'VPD_mean', #'VPD_CV', 'VPD_TAC'


###################################################
######     PDP RELEVANT PARAMS                 ####
###################################################

# Some pdps were by default created when creating the RF. These are for the diversity variables (and forestcover for no_diversity) if you only want to run over these (faster) then select T 
# F: may not be applicable for for simplified published code
b_run_basic  <- T

# toggle whether to run from the saved 'results' objects (recommended and tested) or use separately produced pdps
run_from_results <- F
# currently I have left in a hardcoded line that switches this from F to T for the fourth df. This is because I changed mid-way
# in general from now on, I will likely save the output of the createRF_parallely to T

# select the number of points to put into the pdp (i.e. how many marks on the x-axis of the pdp plots)
n_pdp_points <- 200

