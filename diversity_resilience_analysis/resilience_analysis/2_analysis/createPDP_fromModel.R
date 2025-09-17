# ########################################################
# Title         : createPDP_fromModel.R
# Description   : from a series of rf models created in parallel for a specif diversity metric
#                 create a df containing the pdp diversity metrics as predictor
# Aims          : create df of pdp values for different predictors using the ensemble of rf models
# Inputs	      : the results from the createRF_model_boot
# Outputs	      : dataframes of partial dependence for different variables for a given diveristy metric RF model
# Date          : 2025-02-19
# Authors       : Mark Pickering & Agata Elia
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

######     SET LIBRARIES                      #####
library(dplyr)   # use %>%
# RF model
library(randomForest)   # for random forest regressions and ML
# library(caret)        # for RF tuning
# library(mlbench)      # ML benchmark problems
library(pdp)


###################################################
###### LOAD THE PDPs CREATED IN createRF_model_boot.R #####
###################################################
# the previous program (createRF_model_boot.R) already, by default, creates pdps for the diversity variables (forestcover for no_diversity)
# This is a shortcut for certain important variables and these are aggregated running: b_run_basic
# if not already created (e.g. other predictor variables, then jump to !b_run_basic, below)

print('run combine pdps')
if(b_run_basic){

# initialise a df of pdp for this pdp var and resilience metric combinations
df_pdp_all <- data.frame()

# loop over resilience metrics - this means we can aggregated
for (j in 1:length(v_target)){  
  target_name_j <- v_target[j] ; print(target_name_j)
    
  # in this shortcut we only look at the diversity metric (or the forestcover in the case of the no_diversity)
  # this pdp has already been produced in createRF_model_boot.R, and is simply loaded
  var_name_i <- v_optional_predictor
  
  # loop over the different seeds and collect the premade pdps
  for( i in 1:length(l_seed) ){ # i <- 1
    n_seed <- l_seed[i] ; print(n_seed)
    
    # create the input path of the results, using the different subdirectories
    input_file  <- paste0( root_data_proce, input_dir, input_subdir[i], f_results_name, '_nIter-', l_iter[i], '_div-', v_optional_predictor, '_seed-', n_seed, '_targ-', target_name_j, '.RData')
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
  
  } # end loop over res metrics 
  
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
# this script should be used when you wish to recreate the pdps yourself, e.g. for non-diversity variables

if(!b_run_basic){
  
  # load dataframes of variables containing test/train split
  load( paste0(input_df, 'df_all.RData') )
  # # initialise train/test df
  df_comb.train_i <- subset(df_comb, train_sample == T) # head(df_comb.train_i)
  df_comb.test_i  <- subset(df_comb, train_sample == F) # head(df_comb.test_i)
  
  # loop over the different predictors and create a pdp for each predictor
  df_pdp_all_vars <- data.frame()
  
  for (j in 1:length(v_predictors)){ 
    var_name_i <- v_predictors[j] ; print(var_name_i) # extract individual diversity predictor
    
    # set the considered pdp range of points for this predictor
    pred_grid <- data.frame(v_pdp = seq(min(df_comb.train_i[[var_name_i]]), max(df_comb.train_i[[var_name_i]]), length.out = n_pdp_points))
    names(pred_grid) <- var_name_i  
    
    # initialise a df of pdp for this pdp var
    df_pdp_all <- data.frame()
    
    # loop over the different seeds 
    for( i in 1:length(l_seed) ){ 
      n_seed <- l_seed[i] ; print(n_seed)
      
      print('run from results: T')
      # create the input path of the results, using the different subdirectories
      input_file  <- paste0( root_data_proce, input_dir, input_subdir[i], f_results_name, '_nIter-', l_iter[i], '_div-', v_optional_predictor, '_seed-', n_seed, '_targ-', target_name_j,  '.RData')
      print(input_file)
      load(input_file) # results or l_rf_boot
      
      # normally need to use these
      l_rf_boot      <- results$rf.models
      l_indices_boot <- results$indices
      
      # now loop over the k different rf.models in the list for that seed
      # then create the pdp for each index
      for( k in 1:length(l_rf_boot) ){ 
        print(paste0('model: ', k))
        rf.model_k <- l_rf_boot[[k]]
        
        indices_k <- l_indices_boot[[k]]
        boot_data_k <- df_comb.train_i[indices_k, ]
        pdp_i <- partial(rf.model_k, pred.var = var_name_i , pred.grid = pred_grid, train = boot_data_k) 
        
        pdp_i$n_seed  <- n_seed
        pdp_i$pdp_var <- var_name_i
        df_pdp_all <- rbind(df_pdp_all, pdp_i)
      } # end loop over models in list
    } # end loop over seed for list of models
    
    # save the pdp variables separately
    save(df_pdp_all    , file=paste0(root_data_proce, input_dir, 'df_pdpFromModel_div-',v_optional_predictor, '_varPDP-', var_name_i, '_targ-', target_name_j, '.RData' )    ) # 
    
    # combine with all the other pdp vars of interest
    df_pdp_all_vars <- rbind(df_pdp_all_vars, df_pdp_all)
  }

  # save the pdp variables all together for each diversity metric
  save(df_pdp_all_vars    , file=paste0(root_data_proce, input_dir, 'df_pdpFromModelAll_div-',v_optional_predictor, '.RData' )    ) # 
  
} # end !b_run_basic