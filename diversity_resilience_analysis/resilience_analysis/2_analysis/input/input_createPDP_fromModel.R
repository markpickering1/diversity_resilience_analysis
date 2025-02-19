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

# here is info for the no diversity dataset

# the main directory used
input_dir <- '4_selections_2025-02-19/'

# list of the subdirectories
# input_subdir           <- c('createRF_2023-08-24_101/',  'createRF_2023-08-24_102/',  'createRF_2023-08-24_103/', 'createRF_2023-08-26_104_parallel/')
input_subdir           <- c('','','','','')
# list of the seeds used in each subdir production (should correspond to the dir order)
l_seed                 <- c(98,99,100,101,102)
# list of the niterations used in each subdir productions (should correspond to the dir order)
l_iter                 <- c(20,20,20,20,20)

# prefix of name of results object to load
f_results_name <- 'list_rf_model_boot'                        # this is the earliest produced results ones
f_results_name_2 <- 'list_rf_model_pdp_results_boot_parallel' # used for the ones over diversity metrics




#####################################################
###### SELECT VARIABLES OF INTEREST IN ANALYSIS #####
#####################################################
# choose predictor variables to turn into pdp

# v_target <- c( 'kndvi_TAC', 'kndvi_lambda_xt', 'kndvi_lambda_variance')
v_target <- c(  'kndvi_lambda_xt', 'kndvi_lambda_variance')

# v_optional_predictor - the diversity predictor used in the model
v_optional_predictor <- 'shannon_entropy' # 'sd_rh98'  #'shannon_entropy' #'sd_rh98' #'simpson_index' # 'mu_rh98' ## 'simpson_index' #'mu_rh98'
   #  # 'no_diversity'
    # 'mu_kurt' #
# "no_diversity"
# "rh50_mean", "rh98_mean",
# "fhd_mean"
# "skew_mean", "kurt_mean",
# "shannon_entropy", "simpson_index"
# "rao_quadratic_entropy", "euclidean_distances_mean", "convex_hull_volume"


# variables to test partial dependence in every model
v_predictors <- c( 'shannon_entropy' # 'sd_rh98' # 
  # 'mu_kurt'
  # 'shannon_entropy' #'simpson_index' #'mu_rh98' # 'sd_rh98' # 'simpson_index' #'mu_rh98'
  #'kndvi_mean', 
  # 'socc30cm' # soil carbon content
                   # 'forestcover', # previously: 'mu_treecover',
                    # 'topology_elevation_std', # topology metric # topology_elevation_mean topology_slope_mean topology_slope_std
                   # 't2m_mean', #'t2m_CV', 't2m_TAC', 
  # 'tp_mean' 
  # 'tp_CV', 'tp_TAC'
  # 'VPD_mean', #'VPD_CV', 'VPD_TAC'
  # 'ssr_mean', 'ssr_CV', 'ssr_TAC',
  # "rh50_mean", "rh98_mean",
  # 'mu_kurt'
  # 'shannon_entropy' # 
  # "fhd_mean"
  # "skew_mean", "kurt_mean",
  # "shannon_entropy", "simpson_index"
  # "rao_quadratic_entropy", "euclidean_distances_mean", "convex_hull_volume"
)


###################################################
######     PDP RELEVANT PARAMS                 ####
###################################################

# Some pdps were by default created when creating the RF. These are for the diversity variables (and forestcover for no_diversity)
# if you only want to run over these (faster) then select T
b_run_basic  <- T

# toggle whether to run from the saved 'results' objects (recommended and tested) or use separately produced pdps
run_from_results <- F
# currently I have left in a hardcoded line that switches this from F to T for the fourth df. This is because I changed mid-way
# in general from now on, I will likely save the output of the createRF_parallely to T

# select the number of points to put into the pdp (i.e. how many marks on the x-axis of the pdp plots)
n_pdp_points <- 200

