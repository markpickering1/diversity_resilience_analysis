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
# Date          : 23/11/23
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



# # I messed up the first organisation so I'd like to put the rf with the metric name
# # load the different results
# load( paste0(output_path,'list_rf_model_only/list_rf_model_results_parallelDiv_nDivMet-', n_divMetrics, '_targ-', target_name_j, '_seed-', n_setseed_boot,  '.RData' ) ) 
# # output_list <- list() # vector("list", length = length(results$rf.models)) # list()
# # loop over the entries and group the rf with the extracted diversity metric
# for (k in 1:length(results$rf.models) ){ # k <- 1
#   rf.model <- results$rf.models[[k]]
#   div_metric_k <- names(attr(results$rf.models[[k]]$terms[1], "dataClasses")[2])
#   # output_list[[k]] <- list( rf.model = rf.model_k, div_metric = div_metric_k )
#   print(paste0('saving : ', div_metric_k))
#   save(rf.model     , file=paste0(output_path, 'list_rf_model_only/rf_model_div-', div_metric_k  ,  '_targ-', target_name_j, '_seed-', n_setseed_boot,  '.RData' )    ) 
# }
# # resave the results as before
# # results$rf.models <- output_list
# # save(results     , file=paste0(output_path, 'list_rf_model_results_parallelDiv_nDivMet-', n_divMetrics, '_targ-', target_name_j, '_seed-', n_setseed_boot,  '.RData' )    ) 


# # loop over 'diversity metrics' adding each one in turn and running the RF
# for (i in 1:length(v_target)){  # i  <- 3
#   var_name_i <- v_target[i] ; print(var_name_i)
#   
#   # select only relevant predictors and the identifiers
#   if( var_name_i == "no_diversity") { 
#     v_all_vars       <- c( v_identifiers, v_target, v_predictors)  
#     v_all_vars_train <- c( v_target, v_predictors)  
#   } else{ v_all_vars       <- c( v_identifiers, v_target, var_name_i, v_predictors) 
#   v_all_vars_train <- c( v_target, var_name_i, v_predictors) }
#   
#   # select the ordered
#   
#   # select only those columns and complete rows
#   df_comb_i <- df_comb[, v_all_vars]
#   df_comb_i <- df_comb_i[complete.cases(df_comb_i), ]
#   
#   # create test train split
#   set.seed(n_setseed_trainTest)   # set the test/train split seed
#   train_sample <- sample.split(df_comb_i[[v_target]] , SplitRatio = f_train_frac)
#   df_comb.train_i <- subset(df_comb_i, train_sample == TRUE)
#   df_comb.test_i  <- subset(df_comb_i, train_sample == FALSE)
#   # summary(df_comb.train_i) ; summary(df_comb.test_i) 
#   # dim(df_comb.train_i) ; dim(df_comb.test_i) ; print(dim(df_comb.train_i)[1] / (dim(df_comb.test_i)[1] + dim(df_comb.train_i)[1]))
#   
#   # add split data to the full dataframe
#   df_comb_i <- cbind(df_comb_i, train_sample)
#   
#   # save train and test set for later analysis as well as full df
#   # in theory do not need to save train and test - only the overal comb_i
#   save(df_comb.train_i, file=paste0(output_path, 'df_comb.train_div-',var_name_i, '_targ-', v_target, '.RData' )    )
#   save(df_comb.test_i , file=paste0(output_path, 'df_comb.test_div-' ,var_name_i, '_targ-', v_target, '.RData' )    )
#   save(df_comb_i      , file=paste0(output_path, 'df_all_div-'       ,var_name_i, '_targ-', v_target, '.RData' )    )
#   
#   # select only relevant predictors without the identifiers to run in model
#   df_comb.train_i <- df_comb.train_i[, v_all_vars_train]
#   
# 
#   # Run over bootstraped iterations of the rf model
#   # if(!b_run_boot) n_iterations <- 1 # if we want to run over same RF code lines could just put this in and remove sampling
#   
#   if(b_run_boot){
#     # set sampling seed (do not reuse seed when combining bootstrapped rfs between R sessions)
#     set.seed(n_setseed_boot)
#     
#     # this is the variable for making the pdps within the function in theory can probably make the pdps out of the rf and later remove this
#     # for now set as forestcover (for the no_diversity), and set as the diversity metric otherwise
#     v_pdp <- ifelse(var_name_i == 'no_diversity' ,"forestcover" , var_name_i)
#     print(paste0('investigating partial variable (v_pdp):', v_pdp))
#     
#     # LATER REMOVE - this is for testing only for forestcover - in order to test if should produce pdps now or later
#     # set the values of the partial variable to predict on:
#     pred_grid <- data.frame(v_pdp = seq(min(df_comb.train_i[[v_pdp]]), max(df_comb.train_i[[v_pdp]]), length.out = n_pdp_points))
#     names(pred_grid) <- v_pdp   # head(pred_grid)
#     
#     # Register parallel backend
#     cl <- makeCluster(n_cores,  outfile=paste0(output_path, 'log.txt'))
#     registerDoParallel(cl)
#     
#     # Run foreach loop with parallel processing
#     results <- foreach(i = 1:n_iterations, .packages = c("randomForest", "pdp"), .combine = combine_function, .multicombine = TRUE) %dopar% {
#       boot_function(i)
#     }
#     
#     print('stop cluster)')
#     # Stop cluster
#     stopCluster(cl)
#     
#     f_time_update(t_start_time) # time check
#     
#     print('save results')
#     # save first output of the results, along with the seed in case of replication    
#     save(results     , file=paste0(output_path, 'list_rf_model_pdp_results_boot_parallel_nIter-', n_iterations, '_div-',var_name_i, '_seed-', n_setseed_boot, '_targ-', v_target, '.RData' )    ) # , '_mntry-', mtry_1, '_ntree-', ntree_1
#     
#   } # end of bootstrapping
#   
#   # # Run over a single non-bootstraped iteration of the rf model
#   
#   if(!b_run_boot) {
#     
#     # Create formula as a character string
#     formula_str <- paste(v_target, "~ .")
#     # Convert to formula object
#     formula_obj <- as.formula(formula_str)
#     
#     # rf.model <- randomForest(!!as.symbol(v_target) ~ . , data = df_comb.train,  importance = TRUE)
#     rf.model <- randomForest( formula = formula_obj , data = df_comb.train_i, 
#                               mtry = mtry_1, ntree = ntree_1, importance = TRUE,
#                               na.action=na.omit) # previously cleaned NAs manually
#     
#     # only save the model if it is 
#     save(rf.model, file=paste0(output_path, 'rf_model_div-',var_name_i, '_targ-', v_target, '.RData' )    ) # , '_mntry-', mtry_1, '_ntree-', ntree_1
#     
#   } # end non-bootstrapped iteration
#   
#   rf_end_time <- Sys.time() ; print(rf_end_time)      # initialise time
#   f_time_update(t_start_time)
#   
# } # end loop over variables

# output timings
print('end of script')
# output timings
f_time_update(t_start_time)


