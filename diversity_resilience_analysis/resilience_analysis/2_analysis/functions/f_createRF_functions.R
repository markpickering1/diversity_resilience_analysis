# ########################################################
# Title         : f_createRF_model.R
# Description   : statistical functions to apply to RF modelling
#                 
#                 
# Date          : 23/11/23
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################


###################################################
######     FUNCTIONS  - BOOTSTRAPPED MODEL    #####
###################################################

# Function to fit random forest and compute PDP for one bootstrap sample
# NOTE: added v_target - needs testing and maybe adding to function input!
boot_function <- function(index) {
  # Sample with replacement
  indices <- sample(1:nrow(df_comb.train_i), replace = TRUE)
  boot_data <- df_comb.train_i[indices, ]
  print('run rf.model')
  
  # old method - set metric
  # rf.model <- randomForest( kndvi_TAC ~ . , data = boot_data, 
  #                           mtry = mtry_1, ntree = ntree_1, importance = TRUE,
  #                           na.action=na.omit) # superfluous: previously cleaned NAs manually
  
  # new method - user input resilience metric 
  # Create formula as a character string
  formula_str <- paste(target_name_j, "~ .")
  # Convert to formula object
  formula_obj <- as.formula(formula_str)
  
  # rf.model <- randomForest(!!as.symbol(target_name_j) ~ . , data = df_comb.train,  importance = TRUE)
  rf.model <- randomForest( formula = formula_obj , data = df_comb.train_i, 
                            mtry = mtry_1, ntree = ntree_1, importance = TRUE,
                            na.action=na.omit) # previously cleaned NAs manually
  
  
  # rf.model <- randomForest(Species ~ ., data = boot_data, ntree = 1000)
  print('completed rf.model, now partial')
  pdp_i <- partial(rf.model, pred.var = v_pdp , pred.grid = pred_grid, train = boot_data) # v_pdp
  pdp_i$para_iteration <- index
  print('finish boot function for iteration')
  return(list(rf.model = rf.model, pdp_i = pdp_i, indices = indices))
}

# Custom combine function
combine_function <- function(...) {
  list(rf.models = lapply(list(...), `[[`, "rf.model"), pdp_i = lapply(list(...), `[[`, "pdp_i"), indices = lapply(list(...), `[[`, "indices"))
}



###################################################
######     FUNCTIONS  - PARA DIV MODELS       #####
###################################################

# Function to fit random forest and compute PDP for one diversity metric: var_name_i
# NOTE: to add v_target - needs testing and maybe adding to function input!
parallelDiv_function <- function( var_name_i) {
  
  # select only relevant predictors and the identifiers
  if( var_name_i == "no_diversity") { 
    v_all_vars       <- c( v_identifiers, target_name_j, v_predictors)  
    v_all_vars_train <- c( target_name_j, v_predictors)  
  } else{ v_all_vars       <- c( v_identifiers, target_name_j, var_name_i, v_predictors) 
  v_all_vars_train <- c( target_name_j, var_name_i, v_predictors) }
  
  print('train on vars: ') ; print(v_all_vars_train)
  # select the ordered
  
  # select only those columns and complete rows
  df_comb_i <- df_comb[, v_all_vars]
  df_comb_i <- df_comb_i[complete.cases(df_comb_i), ]
  # apply a test-train split if not already applied
  if(b_do_testTrainSplit){
    # create test train split
    set.seed(n_setseed_trainTest)   # set the test/train split seed
    train_sample <- sample.split(df_comb_i[[target_name_j]] , SplitRatio = f_train_frac)
    df_comb.train_i <- subset(df_comb_i, train_sample == TRUE)
    df_comb.test_i  <- subset(df_comb_i, train_sample == FALSE)
    
    # add split data to the full dataframe
    df_comb_i <- cbind(df_comb_i, train_sample)
    
    # save train and test set for later analysis as well as full df
    # in theory do not need to save train and test - only the overal comb_i
    save(df_comb.train_i, file=paste0(output_path, 'df_comb.train_div-',var_name_i, '_targ-', target_name_j, '.RData' )    )
    save(df_comb.test_i , file=paste0(output_path, 'df_comb.test_div-' ,var_name_i, '_targ-', target_name_j, '.RData' )    )
    save(df_comb_i      , file=paste0(output_path, 'df_all_div-'       ,var_name_i, '_targ-', target_name_j, '.RData' )    )
  } else{
    df_comb.train_i <- subset(df_comb_i, train_sample == TRUE)
    df_comb.test_i  <- subset(df_comb_i, train_sample == FALSE)
  }
  
  # select only relevant predictors without the identifiers to run in model
  df_comb.train_i <- df_comb.train_i[, v_all_vars_train]
  
  # # Sample with replacement - DON'T DO AS IS NOT BOOSTRAP
  # set sampling seed (do not reuse seed when combining bootstrapped rfs between R sessions)
  # I think we don't need this as we are not bootstrapping - i.e. don't need diff seeds for results
  # set.seed(n_setseed_boot)
  # indices <- sample(1:nrow(df_comb.train_i), replace = TRUE)
  # boot_data <- df_comb.train_i[indices, ]
  # print('run rf.model')
  
  # old method - set metric
  # rf.model <- randomForest( kndvi_TAC ~ . , data = boot_data, 
  #                           mtry = mtry_1, ntree = ntree_1, importance = TRUE,
  #                           na.action=na.omit) # superfluous: previously cleaned NAs manually
  
  # new method - user input resilience metric 
  # Create formula as a character string
  formula_str <- paste(target_name_j, "~ .")
  # Convert to formula object
  formula_obj <- as.formula(formula_str)
  
  # rf.model <- randomForest(!!as.symbol(target_name_j) ~ . , data = df_comb.train,  importance = TRUE)
  rf.model <- randomForest( formula = formula_obj , data = df_comb.train_i, 
                            mtry = mtry_1, ntree = ntree_1, importance = TRUE,
                            na.action=na.omit) # previously cleaned NAs manually

  # save the individual rf.models - prob don't need to save seed here
  save(rf.model     , file=paste0(output_path, 'list_rf_model_results_parallelDiv_div-', var_name_i  , '_targ-', target_name_j,   '.RData' )    ) 
  
  # also return the model for combining and saving
  # return( list( rf.model = rf.model)  ) # first version
  return( list( rf.model = rf.model, div_metric = var_name_i)  )
  
  # # rf.model <- randomForest(Species ~ ., data = boot_data, ntree = 1000)
  # print('completed rf.model, now partial')
  # pdp_i <- partial(rf.model, pred.var = v_pdp , pred.grid = pred_grid, train = boot_data) # v_pdp
  # pdp_i$para_iteration <- index
  # print('finish boot function for iteration')
  # return(list(rf.model = rf.model, pdp_i = pdp_i, indices = indices))
}

# Custom combine function
combine_function_parallelDiv <- function(...) {
  list(rf.models = lapply(list(...), `[[`, "rf.model") )
}
# list(rf.models = lapply(list(...), `[[`, "rf.model"), pdp_i = lapply(list(...), `[[`, "pdp_i"), indices = lapply(list(...), `[[`, "indices"))
