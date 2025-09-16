# ########################################################
# Title         : 4_createDf_selections.R
# Description   : Place selections on the combined analysis dataframe
# Aims          : coordinated selections on datasets
# Inputs	      : df_comb single df containing all columns and all rows (inc NAs)
# Outputs	      : df_comb tailored to particular analysis or plotting
# Options	      : 
# Date          : 2025-02-19
# Version       : 
# Authors       : Mark Pickering & Agata Elia
# Notes		      : 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())      

# set initialisation script names
main_initialisation_file   <- '0_main/initialise_R.R'
script_config_dir          <- '1_create_df/input/'    ;   script_config_file <- 'input_createDf_selections.R'

# initialise code setup and repo building
source(main_initialisation_file)
# initialise user inputs (config) to script
source( paste0( script_config_dir, script_config_file) )

######     SET LIBRARIES                      #####
library(dplyr)   # use %>%
library(stringr)
library(caTools) # for test/training split

###################################################
######       I/O                              #####
###################################################

# set/create output directory

output_path <- paste0(root_data_proce, script_output_ext, '/')
print(paste0('output_path is : ', output_path ))

# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for dataframes of analysis inputs : ', output_path ) ) }

# copy configuration input variables to output for storage
if( ! file.exists( paste0(output_path, script_config_file ) ) ) { print('copy config file') 
  file.copy(from= paste0( script_config_dir, script_config_file) , to= paste0(output_path, f_name_output_comb, '_', script_config_file ), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
} else{ print('could not copy config file') }

#######################################
##### DATAFRAME SELECTIONS        #####
#######################################

# load any dataframe from list (that should contain entire timeseries)
load( paste0(root_data_proce, input_dir, '/', f_name_input , '.RData') )

# checks
print(dim(df_comb)) 
head(df_comb) ; summary(df_comb) ;  names(df_comb)

#######################################
##### ADD COLUMNS FOR RES METRIC DIFF  #####
#######################################
# add some extra metrics for tests/comparison/checks - save separately
# subtract various combinations of kndvi for comparison of metrics
if(b_resComparison){
  print('run kndvi metrics tests/comparisons')
  df_comb <- df_comb %>% mutate(
    # the xt slopes
    diff_TAC_slopext           = kndvi_TAC - kndvi_slope_xt,
    diff_TAC_robslopext        = kndvi_TAC - kndvi_rob_slope_xt,
    diff_robslopext_slopext    = kndvi_rob_slope_xt - kndvi_slope_xt,
    # the kappa slopes
    diff_TAC_slopekappa           = kndvi_TAC - kndvi_slope_kappa,
    diff_robslopekappa_slopekappa = kndvi_rob_slope_kappa - kndvi_slope_kappa,
    # the xt vs kappa
    diff_robslopext_robslopekappa = kndvi_rob_slope_kappa - kndvi_rob_slope_kappa,
    diff_slopext_slopekappa       = kndvi_slope_kappa - kndvi_slope_kappa,
    # the lamda
    diff_lamxt_lamkappa           = kndvi_lambda_xt    - kndvi_lambda_kappa,
    diff_lamxt_lamvar             = kndvi_lambda_xt    - kndvi_lambda_variance,
    diff_kappa_lamvar             = kndvi_lambda_kappa - kndvi_lambda_variance,
    # the lamda rob
    diff_roblamxt_roblamkappa     = kndvi_rob_lambda_xt    - kndvi_rob_lambda_kappa,
    diff_roblamxt_roblamvar       = kndvi_rob_lambda_xt    - kndvi_rob_lambda_variance,
    diff_robkappa_roblamvar       = kndvi_rob_lambda_kappa - kndvi_rob_lambda_variance,
    # the rob lamda vs lambda
    diff_roblamxt_lamxt           = kndvi_rob_lambda_xt       - kndvi_lambda_xt,
    diff_roblamkappa_lamkappa     = kndvi_rob_lambda_kappa    - kndvi_lambda_kappa,
    diff_roblamvar_lamvar         = kndvi_rob_lambda_variance - kndvi_lambda_variance
    
  )
  
  # select columns
  b_cols <- grepl("diff_|kndvi_", names(df_comb))
  b_cols[1:2] <- T # include also x,y, columns for plotting
  df_comb <- df_comb[, b_cols]
  # head(df_comb) ; names(df_comb)
  
  # Save merged output - avoid overwriting by assigning random number if already present
  if( ! file.exists( paste0(output_path, f_name_output_comb, '_resilienceMetricComparison' , ".RData")) ){ 
    print(paste0('saving file: ', output_path, f_name_output_comb, '_resilienceMetricComparison' , ".RData")) 
    save(df_comb, file=paste0(output_path, f_name_output_comb, '_resilienceMetricComparison' , ".RData")) 
    }

  # reload the dataframe for the other unrelated selections
  load( paste0(root_data_proce, input_dir, '/', f_name_input , '.RData') )
  
}

#######################################
##### GEDI ENTRIES REQUIREMENT    #####
#######################################

# select on number of GEDI entries per pixel
if(b_GEDI_count_filter) {
  print('Apply filter on GEDI entries/sample count per pixel')
  # Check those below cutoff
  df_comb <- df_comb %>% filter( div_count < n_GEDI_count )
  save(df_comb, file=paste0(output_path, f_name_output_comb, '_lt', n_GEDI_count, 'GEDIcount' , ".RData")) 
  # reload the dataframe for the other unrelated selections
  load( paste0(root_data_proce, input_dir, '/', f_name_input , '.RData') )

  # Apply cutoff 
  df_comb <- df_comb %>% filter( div_count > n_GEDI_count )
  save(df_comb, file=paste0(output_path, f_name_output_comb, '_gt', n_GEDI_count, 'GEDIcount' , ".RData"))
}
  
#######################################
##### KNDVI ENTRIES REQUIREMENT   #####
#######################################  

# select on number of ts entries for calculation
if( b_kndvi_count_filter ) {
  print('Apply filter on number of kndvi values in per-pixel timeseries')
  # Check those below cutoff
  df_comb <- df_comb %>% filter( kndvi_n_ts_entries > n_kndvi_count )
  save(df_comb, file=paste0(output_path, f_name_output_comb, '_gt', n_kndvi_count, 'kndvicount' , ".RData"))
}



#######################################
##### SELECTS DIV METRICS, T/T    #####
#######################################
# To ensure a consistent comparable dataset between different models split test/train dataset 
# Select certain diversity metrics and then remove the NAs that are not present for all cols
# Also split the test train data so that it is common to all variables (simple random selection here)

if(b_consistent_dataset_andTestTrainSet) {
  dim(df_comb) 
  # summary(df_comb)
  
  # select columns which will contain no NAs
  v_all_vars       <- c( v_identifiers, v_target, v_optional_predictors, v_predictors) 
  
  cat(paste0("cols to check: \n", paste(v_all_vars, collapse = ", ")))
  
  df_comb <- df_comb %>%
    filter(!if_any(all_of(v_all_vars), is.na))
  
  dim(df_comb) 
  # summary(df_comb) ;  names(df_comb)
  
  # optional - drop all cols except v_all_vars
  df_comb <- df_comb[v_all_vars]
  
  # create test train split
  set.seed(n_setseed_trainTest) ; print( paste0('train-test seed: ', n_setseed_trainTest))   # set the test/train split seed
  
  train_sample <- sample.split(1:dim(df_comb)[1] , SplitRatio = f_train_frac)
  df_comb.train <- subset(df_comb, train_sample == TRUE)
  df_comb.test  <- subset(df_comb, train_sample == FALSE)
  
  # cross-checks
  # summary(df_comb.train) ; summary(df_comb.test) 
  # dim(df_comb.train) ; dim(df_comb.test) ; print(dim(df_comb.train)[1] / (dim(df_comb.test)[1] + dim(df_comb.train)[1]))
  
  # add test-train split data to the full dataframe
  df_comb <- cbind(df_comb, train_sample)

  # save train and test set for later analysis as well as full df
  # in theory do not need to save train and test - only the overal comb
  save(df_comb.train, file=paste0(output_path, 'df_comb.train.RData' )    )
  save(df_comb.test , file=paste0(output_path, 'df_comb.test.RData' )    )
  save(df_comb      , file=paste0(output_path, 'df_all.RData' )    )
  
}

# # Check to remove negative TAC values and low lambda values
# df_comb <- df_comb %>% filter(kndvi_TAC > 0.03)
# summary(df_comb$kndvi_lambda_variance)
# save(df_comb      , file=paste0(output_path, 'df_all_TACgt003.RData' )    )

