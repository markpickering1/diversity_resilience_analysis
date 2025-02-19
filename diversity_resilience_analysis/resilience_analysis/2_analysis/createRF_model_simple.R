# ########################################################
# Title         : create_RF_model.R
# Description   : create a series of rf models from selected predictor variables
#                 including substituting a different predictor from list (i.e.
#                 diversity metrics as) predictor variables each time
#                 
# Aims          : create RF model for predictors
# Inputs	      : input_create_RF_model.R for model settings (e.g. predictors)
#                 functions script and input dataset - df of combined predictors and response
# Outputs	      : model
# Options	      : can run cross validation on tree depth
# Date          : 2025-02-19
# Version       : 3
# Authors       : Mark Pickering & Agata Elia
# Notes         : Can probably set aside this version as a legacy version. Do not need the bootstrapping as an updated parallel version does it better
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
# source('2_analysis/functions/f_createRF_functions.R')

######     SET LIBRARIES                      #####
# library(chron)   # useful for converting time values
library(dplyr)   # use %>%
# RF model
library(caTools) # for test/training split
library(randomForest) # for random forest regressions and ML
library(caret)        # for RF tuning
# library(mlbench)      # ML benchmark problems
library(pdp)


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

# loop over 'diversity metrics' adding each one in turn and running the RF
for (i in 1:length(v_optional_predictors)){  # i  <- 2
  var_name_i <- v_optional_predictors[i] ; print(var_name_i)
  
  # select only relevant predictors and the identifiers
  if( var_name_i == "no_diversity") { 
          v_all_vars       <- c( v_identifiers, v_target, v_predictors)  
          v_all_vars_train <- c( v_target, v_predictors)  
  } else{ v_all_vars       <- c( v_identifiers, v_target, var_name_i, v_predictors) 
          v_all_vars_train <- c( v_target, var_name_i, v_predictors) }
  
  # select the ordered
  
  # select only those columns and complete rows
  df_comb_i <- df_comb[, v_all_vars]
  df_comb_i <- df_comb_i[complete.cases(df_comb_i), ]
  dim(df_comb_i)
  
  # apply a test-train split if not already applied
  if(b_do_testTrainSplit){
    # create test train split
    set.seed(n_setseed_trainTest)   # set the test/train split seed
    train_sample <- sample.split(df_comb_i[[v_target]] , SplitRatio = f_train_frac)
    df_comb.train_i <- subset(df_comb_i, train_sample == TRUE)
    df_comb.test_i  <- subset(df_comb_i, train_sample == FALSE)
    # summary(df_comb.train_i) ; summary(df_comb.test_i) 
    # dim(df_comb.train_i) ; dim(df_comb.test_i) ; print(dim(df_comb.train_i)[1] / (dim(df_comb.test_i)[1] + dim(df_comb.train_i)[1]))
    
    # add split data to the full dataframe
    df_comb_i <- cbind(df_comb_i, train_sample)
    
    # save train and test set for later analysis as well as full df
    # in theory do not need to save train and test - only the overal comb_i
    save(df_comb.train_i, file=paste0(output_path, 'df_comb.train_div-',var_name_i, '_targ-', v_target, '.RData' )    )
    save(df_comb.test_i , file=paste0(output_path, 'df_comb.test_div-' ,var_name_i, '_targ-', v_target, '.RData' )    )
    save(df_comb_i      , file=paste0(output_path, 'df_all_div-'       ,var_name_i, '_targ-', v_target, '.RData' )    )
  } else{
    df_comb.train_i <- subset(df_comb_i, train_sample == TRUE)
    df_comb.test_i  <- subset(df_comb_i, train_sample == FALSE)
  }
  
  # select only relevant predictors without the identifiers to run in model
  df_comb.train_i <- df_comb.train_i[, v_all_vars_train]
  
  # run a cross validation tuning on the dataset in order to optimise the mtry parameter
  if(b_run_tuning){
    # create CV parameters
    control <- trainControl(method="repeatedcv", number=k_fold_splits, repeats=k_fold_repeats, search="grid")
    # run CV 
    rf_gridsearch <- train(kndvi_TAC~., data=df_comb.train_i, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
    # save output
    save(rf_gridsearch, file=paste0(output_path, 'rf_tune_grid-',var_name_i, '_targ-', v_target, '.RData' )    )
    f_time_update(t_start_time)
  }
  
  # # Run over a single non-bootstraped iteration of the rf model
  
  if(!b_run_boot) {
    
    # Create formula as a character string
    formula_str <- paste(v_target, "~ .")
    # Convert to formula object
    formula_obj <- as.formula(formula_str)
    
    # rf.model <- randomForest(!!as.symbol(v_target) ~ . , data = df_comb.train,  importance = TRUE)
    rf.model <- randomForest( formula = formula_obj , data = df_comb.train_i, 
                              mtry = mtry_1, ntree = ntree_1, importance = TRUE,
                              na.action=na.omit) # previously cleaned NAs manually
  
    # only save the model if it is 
    save(rf.model, file=paste0(output_path, 'rf_model_div-', var_name_i, '_targ-', v_target, '.RData' )    ) # , '_mntry-', mtry_1, '_ntree-', ntree_1
  
  } # end non-bootstrapped iteration
  
  rf_end_time <- Sys.time() ; print(rf_end_time)      # initialise time
  f_time_update(t_start_time)
  
} # end loop over variables

# output timings
print('end of script')
# output timings
f_time_update(t_start_time)

