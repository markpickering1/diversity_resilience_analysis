# ########################################################
# Title         : input_createDf_selections.R
# Description   : This text script acts as a user input to createDf_combine.R
#                 By setting variables in this file, the user should not need to edit the main code.
#                 createDf_combine.R should not change once initialised, except when running full separate
#                 analysis after major changes
# Date          : 2025-02-19
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################


###################################################
######     I/O                                #####
###################################################

# set output path name - with date this will link output (variables) to next script
# leave blank to output to input
script_output_ext <- '4_selections'           # time-series stats

# set output  prefix for cross check dataframes (df_all.RData is output name)
f_name_output_comb <- 'df_all_long_reduce'

# input dataset containing the time-series statistic dataframes to combine
input_dir <- '3_combDFselect'           
# set input name
f_name_input <- 'df_all_long_base'


#####################################################
###### SELECTIONS TO DF                         #####
#####################################################

# create extra resilience comparison metrics
b_resComparison <- F

# number of Gedi points 
b_GEDI_count_filter <- F
n_GEDI_count <- 40

# number of kndvi consecutive points
b_kndvi_count_filter <- T
n_kndvi_count <- 200

#####################################################
###### SELECTIONS TO COLUMNS THAT SHOULD BE NA FREE #####
#####################################################
# all the following columns will be free of NAs 

# target variable to predict - either single string for simple model or vector for running over multiple
v_target <- c('kndvi_lambda_xt', 'kndvi_lambda_variance') # 'kndvi_TAC',
              # 'kndvi_rob_lambda_xt', 'kndvi_rob_lambda_variance') 

v_identifiers <- c('x', 'y')

# variables to go as predictor in every model
v_predictors <- c( 'kndvi_mean', 
                   'socc30cm', # soil carbon content
                   'Ndep',     # Nitrogen deposition
                   'forestcover',
                   'topology_elevation_std', # topology metric # topology_elevation_mean topology_slope_mean topology_slope_std
                   'ssr_mean', 'ssr_CV', 'ssr_TAC',
                   't2m_mean', 't2m_CV', 't2m_TAC', 
                   'tp_mean', 'tp_CV', 'tp_TAC',
                   'VPD_mean', 'VPD_CV', 'VPD_TAC'
)

# v_optional_predictors <- c( "no_diversity")
v_optional_predictors <- c( 
  "Kurtosis", "Shannon", "Canopy_heights_sd"     
)
  # other alternative resilience metrics:
  # "Coefficient_variation", "Skewness"             
  # "Cover_sd"              
  # "Rao"                   "Hull"                 
  # "mu_rh98"                  , # "mu_rh50"                 , "mu_rh75"                 , # "mu_rh25",
  # "mu_kurt"                 ,  #"mu_skew"                  ,  #"mu_sd"                   ,
  # "sd_rh98"                  , # "sd_rh75"                 , "sd_rh50"                 , # "sd_rh25"
  # "shannon_entropy"          #,           # "rao_quadratic_entropy"   , "euclidean_distances_mean",
  # "simpson_index"
  # "mu_fhd_normal"              # "mu_pai"                  , "mu_cover"  ,
  # "mu_mean"                   , "mu_cv"       
  # "mu_skew_negativevalues"    "mu_kurt_negativevalues"    "mu_sd_negativevalues"      "mu_mu_negativevalues"     
  # "mu_cv_negativevalues"      
  # "sd_fhd_normal"             "sd_pai"                   
  # "sd_cover"                  "sd_skew_negativevalues"    "sd_kurt_negativevalues"    "sd_sd_negativevalues"     
  # "sd_mu_negativevalues"      "sd_cv_negativevalues"      "sd_skew"                   "sd_kurt"
  # "sd_sd"                     "sd_mean"                   "sd_cv"                       "euclidean_distances_stdev"



###################################################
######     SET TEST-TRAIN SPLIT PARAMETERS    #####
###################################################
# need to do the test train split by each diversity group and removing NAs

#option to use a consistent test/train & general dataset for production of RFs
b_consistent_dataset_andTestTrainSet <- TRUE

# SIMPLE Train-Test Split - 30% split to start
n_setseed_trainTest <- 102 # for set.seed(n_setseed_trainTest)  # set seed in order to run the same sample each time
f_train_frac <- 0.7

