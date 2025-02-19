# ########################################################
# Title         : create_RF_model.R
# Description   : create a series of non-bootstrapped rf models from selected predictor variables
#                 including substituting a different predictor from list (i.e.
#                 diversity metrics as) predictor variables each time
#                 Script differs from others as each RF model diversity metric runs in parallel to the others
#                 Additional loops over resilience predicted metric
# Aims          : create RF model for predictors
# Inputs	      : input_create_RF_model.R for model settings (e.g. predictors)
#                 functions script and input dataset - df of combined predictors and response
# Outputs	      : model
# Options	      : can run cross validation on tree depth
# Date          : 2025-02-19
# Version       : 3
# Authors       : Mark Pickering & Agata Elia
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())            

# set initialisation script names
main_initialisation_file   <- '0_main/initialise_R.R'
script_config_dir          <- '2_analysis/input/'    ;   script_config_file <- 'input_createRF_model.R'

# initialise code setup and repo building
source(main_initialisation_file)
# initialise user inputs to script
source( paste0( script_config_dir, script_config_file) )
# initialise functions
source('2_analysis/functions/f_createRF_functions.R')

######     SET LIBRARIES                      #####
# library(chron)   # useful for converting time values
library(dplyr)   # use %>%
# RF model
library(caTools) # for test/training split
library(randomForest) # for random forest regressions and ML
library(caret)        # for RF tuning
# library(mlbench)      # ML benchmark problems
library(pdp)

library(doParallel) # run in parallel
library(foreach)    # run in parallel


###################################################
######       I/O                              #####
###################################################

# set/create output directory
output_path <- paste0(root_data_proce, script_output_ext, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))

# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for dataframes of analysis inputs : ', output_path ) ) }

# copy configuration input variables to output for storage
if( ! file.exists( paste0(output_path, script_config_file ) ) ) {
  file.copy(from= paste0( script_config_dir, script_config_file) , to= paste0(output_path, script_config_file ), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
} else{ print('could not copy config file') }

load( paste0(input_dir, input_file) )
dim(df_comb) # summary(df_comb) ;  names(df_comb)

# use only complete data points - either for full df or for the subset of static+climate data
# this ensures comman dataset is used across the div metrics (with the exception of the points not common between div or resilience metrics)
if(b_completeCases_for_fullDF) {df_comb <- df_comb[complete.cases(df_comb), ]
} else if(b_completeCases_for_nonDiv_metrics){ subset_cols <- c(v_target, v_predictors) ;
                              df_comb <- df_comb[complete.cases(df_comb[subset_cols]), ] }
dim(df_comb) ;# head(df_comb)

###################################################
######     RUN                                #####
###################################################
rf_start_time <- Sys.time() ; print(rf_start_time)      # initialise time

# loop over 'resilience metrics' adding each one in turn and running the RF
for (j in 1:length(v_target)){  # j  <- 1
  target_name_j <- v_target[j] ; print(target_name_j)
  n_divMetrics <- length(v_optional_predictors)
  
  
  # Register parallel backend
  cl <- makeCluster(n_cores,  outfile=paste0(output_path, 'log.txt'))
  registerDoParallel(cl)
  
  # Run foreach loop with parallel processing for each metric of optional predictors
  results <- foreach(var_i = v_optional_predictors, .packages = c("randomForest", "pdp", "caTools"), .combine = combine_function_parallelDiv, .multicombine = TRUE) %dopar% {
    parallelDiv_function( var_i )
  }
  
  print('stop cluster)')
  # Stop cluster
  stopCluster(cl)
  
  f_time_update(t_start_time) # time check
  
  print('save results')
  # save first output of the results, along with the seed in case of replication    
  save(results     , file=paste0(output_path, 'list_rf_model_results_parallelDiv_nDivMet-', n_divMetrics, '_targ-', target_name_j, '_seed-', n_setseed_boot,  '.RData' )    )
  
} # end loop over target variables


# output timings
print('end of script')
# output timings
f_time_update(t_start_time)


