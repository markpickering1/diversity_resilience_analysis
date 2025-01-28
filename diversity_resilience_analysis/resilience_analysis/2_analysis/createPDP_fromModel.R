# ########################################################
# Title         : createPDP_fromModel.R
# Description   : from a series of rf models created in parallel for a specif diversity metric
#                 create a df containing the pdp
#                 diversity metrics as) predictor variables each time
#                 
# Aims          : create df of pdp values for different predictors using the ensemble of rf models
# Inputs	      : the results from the createRF_model_parallely.R
#                 
# Outputs	      : dataframes of paratial dependence for different variables for a given diveristy metric RF model
# Options	      : 
# Date          : 4/9/23
# Version       : 2
# Authors       : Mark Pickering & Agata Elia
# Notes         : This script is a bit messy at the moment due to a number of different ways the results of the parallel df were saved
#                 In time this version should be saved as a legacy, and a simpler version (e.g. always 'run_from_results' ) put in place
#                 This is more or less the first bootstrapped method, that was done in a non-parallelised way
#                 One method runs over saved pdps, another creates pdps from bootstrapped dfs.
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())            

# set initialisation script names
main_initialisation_file   <- '0_main/initialise_R.R'
script_config_dir          <- '2_analysis/input/'    ;   script_config_file <- 'input_createPDP_fromModel.R'

# initialise code setup and repo building
source(main_initialisation_file)
# initialise user inputs to script
source( paste0( script_config_dir, script_config_file) )
# initialise functions
# source('2_analysis/functions/f_rf_functions.R')

######     SET LIBRARIES                      #####
# library(chron)   # useful for converting time values
library(dplyr)   # use %>%
# RF model
# library(caTools) # for test/training split
library(randomForest) # for random forest regressions and ML
# library(caret)        # for RF tuning
# library(mlbench)      # ML benchmark problems
library(pdp)


###################################################
###### LOAD THE PDPs CREATED IN createRF_model_parallely.R #####
###################################################
# the program already, by default, creates pdps for the diversity variables (forestcover for no_diversity)
# This is a shortcut for certain important variables - may as well use this as it saves time (already created in parallel)

print('run combine pdps')
if(b_run_basic){

# initialise a df of pdp for this pdp var and resilience metric combinations
df_pdp_all <- data.frame()

# loop over resilience metrics - this means we can aggregated
for (j in 1:length(v_target)){  # j  <- 1
  target_name_j <- v_target[j] ; print(target_name_j)
    
  # in this shortcut we only look at the diversity metric (or the forestcover in the case of the no_diversity)
  var_name_i <- v_optional_predictor
  
  
  # loop over the different seeds 
  for( i in 1:length(l_seed) ){ # i <- 1
    n_seed <- l_seed[i] ; print(n_seed)
    
    # create the input path of the results, using the different subdirectories
    input_file  <- paste0( root_data_proce, input_dir, input_subdir[i], f_results_name_2, '_nIter-', l_iter[i], '_div-', v_optional_predictor, '_seed-', n_seed, '_targ-', target_name_j, '.RData')
    
    print(input_file)
    load(input_file) # results 
    
    # extract the pdp results
    pdp_list_df <- do.call(rbind, lapply(1:length(results$pdp_i), function(i) {
      pdp_i <- results$pdp_i[[i]]
      pdp_i$post_iteration <- i    # save the iteration i value
      return(pdp_i)
    }))
    # save the seed
    pdp_list_df$n_seed  <- n_seed
    pdp_list_df$res_metric <- target_name_j
    pdp_list_df$div_metric <- var_name_i
    # add to the df for all seeds
    df_pdp_all <- rbind(df_pdp_all, pdp_list_df)
  } # end loop over seed for list of models
  
  # pdp_list_mean <- df_pdp_all %>%  group_by( !!sym(var_name_i)) %>%
  #   summarise(mean_yhat = mean(yhat), se_yhat = sd(yhat))
  
  # save the pdp variable
  # save(df_pdp_all    , file=paste0(root_data_proce, input_dir, 'df_pdp_div-',v_optional_predictor, '_varPDP-', var_name_i, '.RData' )    ) #
  } # end loop over res metrics 
  
  # testing
  # df_pdp_all_test_2 <- df_pdp_all %>% filter(res_metric == 'kndvi_lambda_variance')

  names(df_pdp_all)[names(df_pdp_all) == var_name_i] <- 'div_val'
  df_pdp_all$res_metric <- as.factor(df_pdp_all$res_metric)
  df_pdp_all$div_metric <- as.factor(df_pdp_all$div_metric)
  summary(df_pdp_all) ; length(unique(df_pdp_all$div_val))
  # save the pdp variable for all res metrics
  save(df_pdp_all    , file=paste0(root_data_proce, input_dir, 'df_pdp_div-', var_name_i, '.RData' )    ) #
  
} # end run basic



###################################################
###### LOOP OVER DIRS CREATING AND ADDING PDP #####
###################################################
# THIS METHOD SEPARATELY CREATES PDPS FOR VARIABLES THAT ARE NOT PREVIOUSLY CREATED IN createRF_model_parallely.R
# this script should be used when you wish to recreate the pdps yourself for non-diversity variables

if(!b_run_basic){
  
  # load dataframes of variables containing test/train split
  # load df containing all test train data
  load( paste0(root_data_proce, input_dir, input_subdir[1], 'df_all_div-', v_optional_predictor, '_targ-', target_name_j , '.RData') )        # df_comb_i      head(df_comb_i)
  
  # # initialise train/test df
  df_comb.train_i <- subset(df_comb_i, train_sample == T) # head(df_comb.train_i)
  df_comb.test_i  <- subset(df_comb_i, train_sample == F) # head(df_comb.test_i)
  
  # loop over the different predictors and create a pdp for each predictor
  df_pdp_all_vars <- data.frame()
  
  for (j in 1:length(v_predictors)){ # j <- 1
    var_name_i <- v_predictors[j] ; print(var_name_i) # extract individual diversity predictor
    
    # set the considered pdp range of points for this predictor
    pred_grid <- data.frame(v_pdp = seq(min(df_comb.train_i[[var_name_i]]), max(df_comb.train_i[[var_name_i]]), length.out = n_pdp_points))
    names(pred_grid) <- var_name_i   # head(pred_grid)
    
    # initialise a df of pdp for this pdp var
    df_pdp_all <- data.frame()
    
    # loop over the different seeds 
    for( i in 1:length(l_seed) ){ # i <- 4
      n_seed <- l_seed[i] ; print(n_seed)
      
      # this line switches between the initial way the data was saved and a 'new' way - it is messy and in later runs can be cleaned
      # if(i==4){run_from_results <- T} # no longer necessary
      
      # now can either run from the results object (containing indices etc) OR run from separately saved objects
      
      if(!run_from_results){
        print('run from results: F')
        # load the list of rf.models for that seed and run pdp from this
        input_file  <- paste0( root_data_proce, input_dir, input_subdir[i], f_results_name, '_nIter-', l_iter[i], '_div-', v_optional_predictor, '_seed-', n_seed, '_targ-', target_name_j, '.RData')
        
        # input_file_rf       <- paste0( input_dir, input_subdir[i], 'list_rf_model_boot_nIter-8_div-no_diversity_seed-', n_seed, '.RData')
        print(input_file)
        load(input_file) # l_rf_boot
        
        # load also the indices
        input_file_ind  <- paste0( root_data_proce, input_dir, input_subdir[i], 'list_indices_boot', '_nIter-', l_iter[i], '_div-', v_optional_predictor, '_seed-', n_seed, '_targ-', target_name_j, '.RData')
        load(input_file_ind)
        # l_indices_boot <- results$rf.models
        
      }
      
      if(run_from_results){
        print('run from results: T')
        # create the input path of the results, using the different subdirectories
        input_file  <- paste0( root_data_proce, input_dir, input_subdir[i], f_results_name_2, '_nIter-', l_iter[i], '_div-', v_optional_predictor, '_seed-', n_seed, '_targ-', target_name_j,  '.RData')
        # input_file  <- paste0( input_dir, input_subdir[i], 'list_rf_model_pdp_results_boot_parallel_nIter-8_div-no_diversity_seed-', n_seed, '.RData')
        print(input_file)
        load(input_file) # results or l_rf_boot
        
        # normally need to use these
        l_rf_boot      <- results$rf.models
        l_indices_boot <- results$indices
        # save(l_rf_boot     , file=paste0( root_data_proce, input_dir, input_subdir[i], 'list_rf_model_boot_nIter-', l_iter[i], '_div-',v_optional_predictor, '_seed-', n_seed, '.RData' )    ) # , '_mntry-', mtry_1, '_ntree-', ntree_1
        # save(l_indices_boot, file=paste0( root_data_proce, input_dir, input_subdir[i], 'list_indices_boot_nIter-' , l_iter[i], '_div-',v_optional_predictor, '_seed-', n_seed, '.RData' )    ) #
        
      }
      
      # now loop over the k different rf.models in the list for that seed
      for( k in 1:length(l_rf_boot) ){ # k <- 1
        print(paste0('model: ', k))
        rf.model_k <- l_rf_boot[[k]]
        
        # if(!run_from_results) pdp_i <- partial(rf.model_k, pred.var = var_name_i , pred.grid = pred_grid) # v_pdp # , train = boot_data
        # if(run_from_results)  {
        indices_k <- l_indices_boot[[k]]
        boot_data_k <- df_comb.train_i[indices_k, ]
        pdp_i <- partial(rf.model_k, pred.var = var_name_i , pred.grid = pred_grid, train = boot_data_k) # v_pdp # , train = boot_data
        # }
        
        pdp_i$n_seed  <- n_seed
        pdp_i$pdp_var <- var_name_i
        df_pdp_all <- rbind(df_pdp_all, pdp_i)
      } # end loop over models in list
    } # end loop over seed for list of models
    
    # save the pdp variables separately
    save(df_pdp_all    , file=paste0(root_data_proce, input_dir, 'df_pdp_test2createpdpfromModel_div-',v_optional_predictor, '_varPDP-', var_name_i, '_targ-', target_name_j, '.RData' )    ) # 
    
    # combine with all the other pdp vars of interest
    df_pdp_all_vars <- rbind(df_pdp_all_vars, df_pdp_all)
    
    
  }
  
  # save the pdp variables all together for each diversity metric
  save(df_pdp_all_vars    , file=paste0(root_data_proce, input_dir, 'df_pdp_div-',v_optional_predictor, '_multiVars.RData' )    ) # 
  
} # end !b_run_basic