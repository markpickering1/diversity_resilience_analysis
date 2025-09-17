# ########################################################
# Title         : plot_rf_performance.R
# Description   : plot RF output to analyse performance and, in particular, variability of the 
#                 model to different diversity metrics 
#                 Non-bootstrapped version
# Aims          : analyse performance and diversity metrics
# Inputs	      : rf models with different diversity metrics in each 
# Outputs	      : figures for each diversity: performance 
# Date          : 2025-02-19
# Authors       : Mark Pickering & Agata Elia
# ########################################################


###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())            

script_subtitle <- 'performance'
# load RF plotting functions
source('3_create_figures/functions/f_plotRF_initialise.R')
library(car)    # check multicollinearity

###################################################
####### LOAD RF AND DF                   ##########
###################################################

# load dataframes of variables containing test/train split
load( paste0(input_df, 'df_all', '.RData') )        # df_comb_i      head(df_comb_i)

# # initialise train/test df
df_comb.train_i <- subset(df_comb, train_sample == T) # head(df_comb.train_i)
df_comb.test_i  <- subset(df_comb, train_sample == F) # head(df_comb.test_i)

###################################################
######     LOOP OVER RF DIVERSITY MODELS      #####
###################################################

# initialise a df of all obs vs mod
df_comb_predictVobs_all <- data.frame()

# loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
for (i in 1:length(v_optional_predictors)){ 
  var_name_i <- v_optional_predictors[i] # extract individual diversity predictor
  print(var_name_i)
  # time count
  f_time_update(t_start_time)

  # loop over resilience metrics
  for (j in 1:length(v_target)){  
    target_name_j <- v_target[j] ; print(target_name_j)
  
    # ###################################################
    # ####### LOAD RF AND DF                   ##########
    # ###################################################

    # load basic rf model
    load( paste0(input_dir, 'rf_model_div-', var_name_i, '_targ-', target_name_j, '.RData' ) )
    
    # load bootstrapped rf models - for averaging or calculating variability (to do as a prior step)
    # load( paste0(input_dir, 'list_rf_model_pdp_results_boot_parallel_nIter-20_div-',
    #              var_name_i, '_seed-99_targ-', target_name_j, '.RData') )

  
    ###################################################
    ####### RUN PERFORMANCE METRICS          ##########
    ###################################################
    
    # run and plot performance metrics (R2 MSE BIAS g_OBS_vs_PRED) on the test dataset
    print('testing performance')
    
    # choose to run perf on the train dataset (instead of test) - this switches the dfs
    if(b_run_on_train) {df_comb.test_i <- df_comb.train_i}
    
    # get df_test predictions of resilience metric based on rf.model. test df (df_comb.test_i must have target variable removed)
    df_comb.test_i_predictors <- df_comb.test_i[,-which(names(df_comb.test_i) %in% v_target)] # df_comb.test[,-c(1)])
    df_comb.test_i_predict_test <- as.data.frame( predict(rf.model, df_comb.test_i_predictors) )  
    head(df_comb.test_i_predict_test) ; dim(df_comb.test_i_predict_test) ; dim(df_comb.test_i) ;
    
    # combine the observed and the model predicted resilience metrics
    df_comb_predictVobs <- cbind( df_comb.test_i[,which(names(df_comb.test_i) %in% target_name_j)], df_comb.test_i_predict_test )
    names(df_comb_predictVobs) <- c( 'Observed', 'Modelled' )
    # head(df_comb_predictVobs) ; summary(df_comb_predictVobs)
    df_comb_predictVobs <- df_comb_predictVobs[complete.cases(df_comb_predictVobs), ]
    
    x_lab_i <- l_lables_metrics[[var_name_i]]
    y_lab_i <- l_lables_metrics[[target_name_j]]
    c_lims  <- y_lims_pdp_perf[[target_name_j]]
    
    # invert the restoration rate
    if( b_useAbs_RestRate & ( target_name_j == "kndvi_lambda_xt"  | target_name_j == "kndvi_lambda_variance") ){
      print('invert rest rate')
      y_lab_i <- paste0( '|', l_lables_metrics[[target_name_j]] , '|' )
      df_comb_predictVobs$Observed  <- df_comb_predictVobs$Observed *-1
      df_comb_predictVobs$Modelled  <- df_comb_predictVobs$Modelled *-1
      c_lims  <- -1* c_lims[c(2, 1)]   # invert the range

    }
    
    # plot the dataframe with options to add cor, mse, rmse, mae, pbias, coefDet
    g_perf <- f_obs_vs_mod_density(df_comb_predictVobs, s_title = paste0(y_lab_i, ' for ', x_lab_i), 
                                   b_cor = F, b_mse = T, b_rmse = F,  b_mae = F, b_pbias = T, b_coefDet = T,
                                   lims_x = c_lims, lims_y = c_lims)
    
    # remove the legend (optional)
    legend_grob <- cowplot::get_legend(g_perf)
    g_perf <- g_perf + theme(legend.position = "none")
    
    # create the plots without the legend
    ggsave(filename = paste0( 'g_obsVSmod_div-', var_name_i, '_targ-', target_name_j, 
                              '_absRR-', b_useAbs_RestRate , '_runOnTrain-', b_run_on_train,  '_R_MSE_PB.png' ) , 
           plot = g_perf, path = output_path, width = fig_width_wide+2, height = fig_height_wide+2)

    # add this div x res combination to the dataframe
    df_comb_predictVobs$div_metric <- var_name_i
    df_comb_predictVobs$res_metric <- target_name_j
    df_comb_predictVobs_all <- rbind(df_comb_predictVobs_all,  df_comb_predictVobs)
    
  } # end loop over resilience metrics
} # end loop over variables

# save also a dataframe of all the Observed-Modelled values by res/div metric
df_comb_predictVobs_all$div_metric <- as.factor(df_comb_predictVobs_all$div_metric)
df_comb_predictVobs_all$res_metric <- as.factor(df_comb_predictVobs_all$res_metric)
summary(df_comb_predictVobs_all)
save( df_comb_predictVobs_all, file = paste0(output_path, 'df_ObsvsMod', '_runOnTrain-', b_run_on_train, '.RData' ) )

