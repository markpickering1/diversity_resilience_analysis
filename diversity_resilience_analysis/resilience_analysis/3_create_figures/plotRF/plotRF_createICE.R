# ########################################################
# Title         : plotRF_createICE.R
# Description   : In this script we create the Individual Conditional Expectation dataset for plotting
# Aims          : Create ICE dataset
# Inputs	      : rf models with different diversity metrics in each 
# Outputs	      : dice dataframe 
# Date          : 2025-02-19
# Authors       : Mark Pickering & Agata Elia
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())            

script_subtitle <- 'createICE'
# load RF plotting functions
source('3_create_figures/functions/f_plotRF_initialise.R')


###################################################
######     LOOP OVER RESILIENCE METRICS       #####
###################################################


if(b_run_dice){ 
  
  # loop over 'resilience target metrics' producing importance figure and adding each to an div importance dataframe
  for (k in 1:length(v_target)){ 
    target_name_k <- v_target[k] ; print(target_name_k)
    
    # loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
    for (i in 1:length(v_optional_predictors)){ 
      var_name_i <- v_optional_predictors[i] ; print(var_name_i)
    
      # time count
      f_time_update(t_start_time)
      
      ###################################################
      ####### LOAD RF AND DF                   ##########
      ###################################################
      
      # load basic rf model
      load( paste0(input_dir, 'rf_model_div-', var_name_i, '_targ-', target_name_k, '.RData' ) )
      
      # load dataframes of variables containing test/train split
      load( paste0(input_df, 'df_all', '.RData') )        # summary( df_comb )      head(df_comb)
      
      # # initialise train/test df and df to use in analysis - select only those needed
      df_ice <- f_plotRF_load_RFDF(df_comb, s_train_test_all = 'all') # ; head(df_pdp)
      summary(df_ice) ; dim(df_ice)

      ###################################################
      ######     ICE                                #####
      ###################################################
      # it takes ~ hours to run per variable so be sure you want to run this
      # therefore it is not recommended to run looping over all diversity/resilience metrics at once
    
      # create df of dice values
      # as the computation takes a while and loads a lot of data into RAM, 
      # the data can be broken up into (n_splits) chunks for processing
      # each chunk is run (and can be saved, b_saveChunks) separately and then recombined at the end
      # splitting into chunks means that not all the same chunks contain the same ICE ranges of values
      df_dice <- f_run_ICE(df_ice, rf.model, var_name_i, predicted_metric = target_name_k, n_splits = 5, b_saveChunks = F) 
      print('end f_run_ICE') ; f_time_update(t_start_time)
      
      # save the overall dice object
      save(df_dice, file=paste0(output_path, 'df_dice-', var_name_i, '_targ-', target_name_k, '.RData' )    ) 
      # summary(df_dice) ; dim(df_dice)
      print('dice object saved') ; f_time_update(t_start_time)
      
      # # # rejoin with the original variables to get x,y
 
      
    } # end loop over div metrics
  } # end loop over res metrics
  
} # end run ICE