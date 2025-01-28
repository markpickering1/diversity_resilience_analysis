# ########################################################
# Title         : create_RF_model.R
# Description   : create a series of bootstrapped rf models from selected predictor variables
#                 including substituting a different predictor from list (i.e.
#                 diversity metrics as) predictor variables each time
#                 
# Aims          : create RF model for predictors
# Inputs	      : input_create_RF_model.R for model settings (e.g. predictors)
#                 functions script and input dataset - df of combined predictors and response
# Outputs	      : model
# Options	      : can run cross validation on tree depth
# Date          : 15/6/23
# Version       : 3 (updated 2/2/24)
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
# initialise functions
# source('3_create_figures/functions/f_plotRF.R')

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
output_path <- paste0(root_data_proce, script_output_ext, '_', full_date, '/')
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
dim(df_comb) ; #head(df_comb)

###################################################
######     RUN                                #####
###################################################
rf_start_time <- Sys.time() ; print(rf_start_time)      # initialise time

# loop over resilience metrics - probably easier to run only one res metric and do in separate terminals
for (j in 1:length(v_target)){  # j  <- 1
target_name_j <- v_target[j] ; print(target_name_j)
# loop over 'diversity metrics' adding each one in turn and running the RF
for (i in 1:length(v_optional_predictors)){  # i  <- 1
  var_name_i <- v_optional_predictors[i] ; print(var_name_i)
  
  # select only relevant predictors and the identifiers
  if( var_name_i == "no_diversity") { 
          v_all_vars       <- c( v_identifiers, target_name_j, v_predictors)  
          v_all_vars_train <- c( target_name_j, v_predictors)  
  } else{ v_all_vars       <- c( v_identifiers, target_name_j, var_name_i, v_predictors) 
          v_all_vars_train <- c( target_name_j, var_name_i, v_predictors) }
  
  # select only those columns and complete rows
  df_comb_i <- df_comb[, v_all_vars]
  df_comb_i <- df_comb_i[complete.cases(df_comb_i), ]
  
  # resample column to add randomness for p-value testing
  if(b_resample){
    print(paste0('random resample col: ', s_resample_col))
    df_comb_i[[s_resample_col]] <- sample(df_comb_i[[s_resample_col]]) # sample should be without replacement
  }
  
  if(b_do_testTrainSplit){ 
    # create test train split
    set.seed(n_setseed_trainTest) ; print( paste0('train-test seed: ', n_setseed_trainTest))   # set the test/train split seed
    train_sample <- sample.split(df_comb_i[[target_name_j]] , SplitRatio = f_train_frac)
    df_comb.train_i <- subset(df_comb_i, train_sample == TRUE)
    df_comb.test_i  <- subset(df_comb_i, train_sample == FALSE)
    # summary(df_comb.train_i) ; summary(df_comb.test_i) 
    # dim(df_comb.train_i) ; dim(df_comb.test_i) ; print(dim(df_comb.train_i)[1] / (dim(df_comb.test_i)[1] + dim(df_comb.train_i)[1]))
    
    # add split data to the full dataframe
    df_comb_i <- cbind(df_comb_i, train_sample)
    
    # save train and test set for later analysis as well as full df
    # in theory do not need to save train and test - only the overal comb_i
    save(df_comb.train_i, file=paste0(output_path, 'df_comb.train_div-',var_name_i, '_targ-', target_name_j, '_seedTrainTest', n_setseed_trainTest, '_seedboot', n_setseed_boot, '.RData' )    )
    save(df_comb.test_i , file=paste0(output_path, 'df_comb.test_div-' ,var_name_i, '_targ-', target_name_j, '_seedTrainTest', n_setseed_trainTest, '_seedboot', n_setseed_boot, '.RData' )    )
    save(df_comb_i      , file=paste0(output_path, 'df_all_div-'       ,var_name_i, '_targ-', target_name_j, '_seedTrainTest', n_setseed_trainTest, '_seedboot', n_setseed_boot, '.RData' )    )
  
    # select only relevant predictors without the identifiers to run in model
    df_comb.train_i <- df_comb.train_i[, v_all_vars_train]
    
  } else {
    df_comb.train_i <- subset(df_comb_i, train_sample == T) # head(df_comb.train_i)
    # df_comb.train_i <- f_plotRF_load_RFDF(df_comb_i, s_use_pdp_dataset) ; # head(df_pdp) ; # dim(df_pdp)
    # select only relevant predictors without the identifiers to run in model
    df_comb.train_i <- df_comb.train_i[, v_all_vars_train]
  }
  # Run over bootstraped iterations of the rf model
  # if(!b_run_boot) n_iterations <- 1 # if we want to run over same RF code lines could just put this in and remove sampling
  
  if(b_run_boot){
    # set sampling seed (do not reuse seed when combining bootstrapped rfs between R sessions)
    set.seed(n_setseed_boot) ; print( paste0('bootstrap seed: ', n_setseed_boot) )
    
    # this is the variable for making the pdps within the function in theory can probably make the pdps out of the rf and later remove this
    # for now set as forestcover (for the no_diversity), and set as the diversity metric otherwise
    v_pdp <- ifelse(var_name_i == 'no_diversity' ,"forestcover" , var_name_i)
    print(paste0('investigating partial variable (v_pdp):', v_pdp))
    
    # LATER REMOVE - this is for testing only for forestcover - in order to test if should produce pdps now or later
    # set the values of the partial variable to predict on:
    pred_grid <- data.frame(v_pdp = seq(min(df_comb.train_i[[v_pdp]]), max(df_comb.train_i[[v_pdp]]), length.out = n_pdp_points))
    names(pred_grid) <- v_pdp   # head(pred_grid)
    
    # Register parallel backend
    cl <- makeCluster(n_cores,  outfile=paste0(output_path, 'log.txt'))
    registerDoParallel(cl)
    
    # Run foreach loop with parallel processing
    results <- foreach(i = 1:n_iterations, .packages = c("randomForest", "pdp"), .combine = combine_function, .multicombine = TRUE) %dopar% {
      boot_function(i)
    }
    
    print('stop cluster)')
    # Stop cluster
    stopCluster(cl)
    
    f_time_update(t_start_time) # time check
    
    print('save results')
    # save first output of the results, along with the seed in case of replication    
    save(results     , file=paste0(output_path, 'list_rf_model_pdp_results_boot_parallel_nIter-', n_iterations, '_div-',var_name_i, '_seed-', n_setseed_boot, '_targ-', target_name_j, '.RData' )    ) # , '_mntry-', mtry_1, '_ntree-', ntree_1
    
  } # end of bootstrapping
  
  # # Run over a single non-bootstraped iteration of the rf model
  # dropped - see simple model version
  
  rf_end_time <- Sys.time() ; print(rf_end_time)      # initialise time
  f_time_update(t_start_time)
  
} # end loop over variables
} # end loop over v_target

# output timings
print('end of script')
# output timings
f_time_update(t_start_time)



#############
### EXTRA INFO FOR BOOTSTRAPPING
#############
### this code produces the pdps for set variables in line
### insert after 'save results'


# print('extract results')
# Extract results
# l_rf_boot <- results$rf.models
# the following do.call commands rejoin the separate results into one dataframe
# # pdp_list_df <- do.call(rbind, lapply(results$pdp_i, function(pdp_i) {
# #   pdp_i$iteration <- i
# #   return(pdp_i)
# # }))
# pdp_list_df <- do.call(rbind, lapply(1:length(results$pdp_i), function(i) {
#   pdp_i <- results$pdp_i[[i]]
#   pdp_i$post_iteration <- i
#   return(pdp_i)
# }))
# indices_list_df <- do.call(rbind, lapply(1:length(results$indices), function(i) {
#   indices <- results$indices[[i]]
#   indices$post_iteration <- i
#   return(indices)
# }))

# save(l_rf_boot      , file=paste0(output_path, 'list_rf_model_boot_nIter-', n_iterations, '_div-',var_name_i, '_seed-', n_setseed_boot, '_targ-', v_target, '.RData' )    ) # , '_mntry-', mtry_1, '_ntree-', ntree_1
# save(indices_list_df, file=paste0(output_path, 'list_indices_boot_nIter-' , n_iterations, '_div-',var_name_i, '_seed-', n_setseed_boot, '_targ-', v_target, '.RData' )    ) #
# save(pdp_list_df    , file=paste0(output_path, 'list_pdp-', v_pdp, '_fc_boot_nIter-'  , n_iterations, '_div-',var_name_i, '_seed-', n_setseed_boot, '_targ-', v_target, '.RData' )    ) #


# # initialise list to store RF object and the pdps
# l_rf_boot <- list()
# l_indices_boot <- list()
# l_pdp_boot <- list()
# 
# 
# # for(j in 1:n_iterations) { # j <- 1
#   # print(paste0('bootstrap iteration j = ', j) )
#   
#   
#   # Sample with replacement
#   # indices_j <- sample(1:nrow(df_comb.train_i), replace = TRUE) # length(unique(indices_j))/dim(df_comb.train_i)[1]
#   # boot_data_j <- df_comb.train_i[indices_j, ] # length(unique(indices))
#   
#   # rf.model_j <- randomForest( kndvi_TAC ~ . , data = boot_data_j, 
#   #                           mtry = mtry_1, ntree = ntree_1, importance = TRUE,
#   #                           na.action=na.omit) # superfluous: previously cleaned NAs manually
#   
#   # pp_j <- partialPlot(rf.model_j, boot_data, 'forestcover' )
#   l_rf_boot[[j]] <- rf.model_j
#   l_indices_boot[[j]] <- indices_j
#   
#   # LATER REMOVE - this is for testing only for forestcover - in order to test if should produce pdps now or later
#   # already create the pdps for each variable - this allows us to run on the bootstrapped data
#   # for variables to plot:
#   pdp_j <- partial(rf.model_j, train = boot_data_j,  pred.var = 'forestcover', pred.grid = pred_grid ) # train = df_comb.train_i
#   l_pdp_boot[[j]] <- pdp_j
#   
#   f_time_update(t_start_time) # time check
# } # end of loop over boot iteration j
# 
# # save the files, along with the seed in case of replication    
# save(l_rf_boot     , file=paste0(output_path, 'list_rf_model_boot_nIter-', n_iterations, '_div-',var_name_i, '_seed-', n_setseed_boot, '_targ-', v_target, '.RData' )    ) # , '_mntry-', mtry_1, '_ntree-', ntree_1
# save(l_indices_boot, file=paste0(output_path, 'list_indices_boot_nIter-' , n_iterations, '_div-',var_name_i, '_seed-', n_setseed_boot, '_targ-', v_target, '.RData' )    ) # 
# save(l_pdp_boot    , file=paste0(output_path, 'list_pdp_fc_boot_nIter-'  , n_iterations, '_div-',var_name_i, '_seed-', n_setseed_boot, '_targ-', v_target, '.RData' )    ) # 
# 