# ########################################################
# Title         : input_createRF_model.R
# Description   : This text script acts as a user input to createRF_model.R
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
script_output_ext <- 'createRF_bootDiv_2025-02-19'           # directory name (may include timestamp)

# input dataset containing the combined dataframes to create RF
date_production <- '4_selections_2025-02-19' # as above - updated shannon
input_dir <- paste0(root_data_proce, date_production, '/') 
input_file <- 'df_all.RData' # new div add GEDI_entries kndvi_n_ts_entries selections


#####################################################
###### SELECT VARIABLES OF INTEREST IN ANALYSIS #####
#####################################################
# choose identifier, target and predictor variables
# some predictor variables are used in all RF models, other 'optional' variables
# are cycled over an included in only one RF model each time (this applies to the 
# diversity metrics, including 'no_diversity' metric)

# target variable to predict - either single string for simple model or vector for running over multiple
v_target <- c('kndvi_lambda_xt') #,'kndvi_lambda_variance') # , 'kndvi_TAC') # 'kndvi_lambda_xt') # , 'kndvi_lambda_variance')       #  ,'kndvi_TAC', , if this doesn't work in some scripts put only one until script updated

# v_identifiers <- c('x', 'y')
v_identifiers <- c('x', 'y', 'train_sample')

# variables to split and run RF over 
# v_split_RF <- c('KG5')

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
# v_optional_predictors <- c( "no_diversity")
v_optional_predictors <- c( # "no_diversity" , 
   # "mu_rh98"                  , # "mu_rh50"                 , "mu_rh75"                 , # "mu_rh25",
  # "mu_kurt"            #     ,  #
  # "mu_skew"                  ,  #"mu_sd"                   ,
  # "sd_rh98"                  #, # "sd_rh75"                 , "sd_rh50"                 , # "sd_rh25"                  ,
  "shannon_entropy"          #,         #   , "rao_quadratic_entropy"   , "euclidean_distances_mean",
  # "simpson_index"
  # "mu_fhd_normal"              # "mu_pai"                  , "mu_cover"  ,
  # "mu_mean"                   , "mu_cv"       
  # "mu_skew_negativevalues"    "mu_kurt_negativevalues"    "mu_sd_negativevalues"      "mu_mu_negativevalues"     
  # "mu_cv_negativevalues"      
  # "sd_fhd_normal"             "sd_pai"                   
  # "sd_cover"                  "sd_skew_negativevalues"    "sd_kurt_negativevalues"    "sd_sd_negativevalues"     
  # "sd_mu_negativevalues"      "sd_cv_negativevalues"      "sd_skew"                   "sd_kurt"                  
  # "sd_sd"                     "sd_mean"                   "sd_cv"                       "euclidean_distances_stdev"
)



###################################################
######     SET RF PARAMETERS                  #####
###################################################
# need to do the test train split by each diversity group and removing NAs

# option on whether to do the test-train split within the createRF_model script, or load directly a dataframe
# which the selection script has already determined a consistent test-train split
b_do_testTrainSplit <- F

# only use the full dataframe for all variables
b_completeCases_for_fullDF <- F            # require no NAs in the full input df for all variables (inc all div metrics)
b_completeCases_for_nonDiv_metrics <- T    # require no NAs in the variables selected for random forest (and only the diversity metric used)

# SIMPLE Train-Test Split - 30% split to start
n_setseed_trainTest <- 102 # for set.seed(n_setseed_trainTest)  # set seed in order to run the same sample each time
f_train_frac <- 0.7

# parameters to optimise
ntree_1 <- 500 # 500                          # to optimise
mtry_1  <- 7  # ceiling( (length(v_predictors) + 1 ) /3) # use default and optimise floor(sqrt(length(v_predictors) + 1 )) 

###################################################
######     TUNE RF PARAMETERS                  ####
###################################################
# if tuning is run, then the tuned parameters are printed (with figures) and replace those above

# Optional tuning
b_run_tuning <- F

# select different mtry parameters - number of variables in each tree
tunegrid <- expand.grid(.mtry=c(5:9))
k_fold_splits  <- 5          # number of k-fold CV splits
k_fold_repeats <- 1          # repeats of tuning
metric         <- "Rsquared" # metric to tune for

###################################################
######     BOOSTRAPPING MODELS                 ####
###################################################
# Instead of outputting a single random forest, this will output a series of random forest models
# These can then be bootstrapped to produce uncertainty estimates

# optional_bootrapping_models - if set false, it will run the simple, single RF model, as first created
b_run_boot <- T

# set the bootstrapping seed
# When sampling with a set seed in an R session, 
# the sampling will be independent within that R session e.g. 
# set.seed(101) ; for(i in 1:10){print(sample (1:10))} 
# However when you open a new session and reset the seed to that same seed, the same sample series will 
# be reproduced in that order (so should set new seed)
# therefore you cannot produce results with the same seed in different sessions and bootstrap them (they will be replications)
# it is advisable to either change seed (but this will affect the test/train sampling) or run over different i in X:Y
# n_setseed_boot <- 102 # for set.seed(n_setseed_trainTest)  # set seed in order to run the same sample each time
n_setseed_boot <- 99 # for set.seed(n_setseed_trainTest)  # set seed in order to run the same sample each time

# number of bootstrap iterations
n_iterations <- 20 # 100?

# for paralellisations
# number of cores to use
n_cores <- 20

# select the number of points to put into the pdp (i.e. how many marks on the x-axis of the pdp plots)
n_pdp_points <- 200

# random resample of model to test e.g. p-vals
b_resample <- T
s_resample_col <- 'shannon_entropy'

