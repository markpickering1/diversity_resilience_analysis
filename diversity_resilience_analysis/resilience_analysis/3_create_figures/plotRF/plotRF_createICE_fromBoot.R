# ########################################################
# Title         : plotRF_createICE.R
# Description   : plot RF output to analyse performance and, in particular, variability of the 
#                 model to different diversity metrics
#                 In this script we create the Individual Conditional Expectation
# Aims          : analyse performance and diversity metrics
# Inputs	      : rf models with different diversity metrics in each 
# Outputs	      : figures for each diversity: 1) performance 2) Importance 3) dice 
#                 summary figures: 1) partial plot of diversity 2) ranked average Importance 3) Importance of diversity metrics
# Options	      : 
# Date          : 2025-02-19
# Version       : 1
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : This version is slightly different as it runs on bootstrapped rfs to get a range of ICE plots
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())            

script_subtitle <- 'createICE'
# load RF plotting functions
source('3_create_figures/functions/f_plotRF_initialise.R')

# iterate [20] over the different rfs produced for each seed to create the ICE (for now run on one seed 98)
i_iter <- 20


output_path <- paste0(root_data_figs, script_output_ext, '_', full_date, '_', script_subtitle,  '/i_iter_',i_iter, '/')
print(paste0('output_path is : ', output_path ))

# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for figures of RF : ', output_path ) ) }

###################################################
######     LOOP OVER RESILIENCE METRICS       #####
###################################################


if(b_run_dice){ 
  
  # loop over 'resilience target metrics' producing importance figure and adding each to an div importance dataframe
  for (k in 1:length(v_target)){ # k <- 1
    target_name_k <- v_target[k] ; print(target_name_k)
    
    # loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
    for (i in 1:length(v_optional_predictors)){ # i <- 1
      var_name_i <- v_optional_predictors[i] ; print(var_name_i)# extract individual diversity predictor
    
      # time count
      f_time_update(t_start_time)
      
      ###################################################
      ####### LOAD RF AND DF                   ##########
      ###################################################
      
      # load rf model
      s_name_rf_model  <- paste0( root_data_proce, input_dir_boot_imp, #, '_nIter-', l_iter[i], 
                             'list_rf_model_pdp_results_boot_parallel_nIter-20', 
                             '_div-', var_name_i, '_seed-', '98', '_targ-', target_name_k, '.RData') #input_subdir[i],
      
      print(s_name_rf_model)
      load(s_name_rf_model) # results[[x]] - load the (20) rf models for each div variable and each seed
      rf.model <- results$rf.models[[i_iter]]
      print(i_iter)
      
      if(!b_do_common_testTrainSplit){ 
        # load dataframes of variables containing test/train split
        s_name_df_comb <-  paste0(input_dir, 'df_all_div-',var_name_i, '_targ-', target_name_k, '.RData')
        load( s_name_df_comb )        # df_comb_i      head(df_comb_i) ; dim(df_comb_i)
      } else{
        load( paste0(input_dir, 'df_all.RData' ) ) # head(df_comb)
        df_comb_i <- df_comb ; rm(df_comb)
      }
      
      
      # # initialise train/test df and df to use in analysis - select only those needed
      df_ice <- f_plotRF_load_RFDF(df_comb_i, s_train_test_all = 'all') # ; head(df_pdp)
      # df_pdp <- result_temp[[1]] ; # df_comb.train_i <- result_temp[[2]] ; df_comb.test_i <- result_temp[[3]] ; rm(result_temp)
      summary(df_ice) ; dim(df_ice)

      
      
      ###################################################
      ######     ICE                                #####
      ###################################################
      # it takes ~ 10 hours to run per variable so be sure you want to run this
      # maybe if 13034 lines to run over (i.e splits ~10) then it takes 7 hours
      # also might need to step through - not run through all variables yet
    
      # create df of dice values
      df_dice <- f_run_ICE(df_ice, rf.model, var_name_i, predicted_metric = target_name_k, n_splits = 20) 
      
      print('end f_run_ICE') ; f_time_update(t_start_time)
      
      
      # save the overall dice object
      save(df_dice, file=paste0(output_path, 'df_dice-', var_name_i, '_targ-', target_name_k, 'iter-', i_iter,  '.RData' )    ) 
      # summary(df_dice) ; dim(df_dice)
      print('dice object saved') ; f_time_update(t_start_time)
      
  
      
    } # end loop over div metrics
  } # end loop over res metrics
  
} # end run ICE