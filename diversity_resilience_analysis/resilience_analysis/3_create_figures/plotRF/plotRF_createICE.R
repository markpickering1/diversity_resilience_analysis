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
# Date          : 29/11/23
# Version       : 1
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : 
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
  for (k in 1:length(v_target)){ # k <- 1
    target_name_k <- v_target[k] ; print(target_name_k)
    
    # loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
    for (i in 1:length(v_optional_predictors)){ # i <- 3
      var_name_i <- v_optional_predictors[i] ; print(var_name_i)# extract individual diversity predictor
    
      # time count
      f_time_update(t_start_time)
      
      ###################################################
      ####### LOAD RF AND DF                   ##########
      ###################################################
      
      # load rf model
      # s_name_rf_model <-  paste0(input_dir, 'rf_model_div-',var_name_i, '_targ-', target_name_k, '_seed-102.RData' ) 
      s_name_rf_model   <-  paste0(input_dir, 'list_rf_model_results_parallelDiv_div-', var_name_i, '_targ-', target_name_k, '.RData')
      load(s_name_rf_model) # rf.model - load the rf model for each div variable
      
      
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
      save(df_dice, file=paste0(output_path, 'df_dice-', var_name_i, '_targ-', target_name_k, '.RData' )    ) # load(/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-03-21_plot/df_ice_test-fhd_avg_diversity.RData)
      # summary(df_dice) ; dim(df_dice)
      print('dice object saved') ; f_time_update(t_start_time)
      
      # # # rejoin with the original variables to get x,y
      # # orig_df_all <- "/eos/jeodpp/data/projects/FOREST-RESILIENCE/data_processing/version_2_postEGU/2023-06-27_allVars_waitingFinalEdit/2_ts_statistics_sv1_2023-07-08_rerunTPSSR/df_all_GSonly.RData"
      # # 
      # # load(orig_df_all)
      # # dim(df_comb)
      # # df_comb <- df_comb[complete.cases(df_comb), ]
      # # df_ice_i_join_orig <- full_join( df_comb, df_ice_i) # this is a really basic way - needs improvement
      # # head(df_ice_i_join_orig) ; dim(df_ice_i_join_orig) ; # summary(df_ice_i_join_orig)
      # 
      # # load diff df_dice directly 
      # # target_name_k <- 'kndvi_lambda_xt'  ; var_name_i <- 'mu_kurt' ; df_dice_direct_load <- '/mnt/eos_rw/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_selectionsCommonTestTrain_2024-02-27_createICE/df_dice-mu_kurt_targ-kndvi_lambda_xt.RData'
      # # target_name_k <- 'kndvi_lambda_xt'  ; var_name_i <- 'shannon_entropy' ; df_dice_direct_load <- '/mnt/eos_rw/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_selectionsCommonTestTrain_2024-02-27_createICE/df_dice-shannon_entropy_targ-kndvi_lambda_xt.RData'
      # # load(df_dice_direct_load)
      # 
      # l_label_res <- paste0( l_lables_metrics[[target_name_k]] )
      # l_label_div <- paste0( l_lables_metrics[[var_name_i]]    )
      # # should use the absolute value of restoration rate?
      # if( b_useAbs_RestRate & ( target_name_k == 'kndvi_lambda_xt' | target_name_k == 'kndvi_lambda_variance' ) ){
      #   df_dice$actual_deriv <- -1 * df_dice$actual_deriv 
      #   l_label_res <- paste0( '|', l_lables_metrics[[target_name_k]], '|' )
      #   print('invert rest rate')
      # }
      # # invert diversity metrics?
      # if(b_invert_mu_kurt & var_name_i== 'mu_kurt'){
      #   df_dice$actual_deriv <- -1 * df_dice$actual_deriv 
      #   var_name_i <- paste0(var_name_i, '_inv')
      #   print('invert mu_kurt')
      # }
      #    
      # # now can make a plot
      # # first select the limits (can probably assume 0.2 and 0.1 for hist and map respectively, tested: fhd, kurtosis)
      # # hist(df_ice_i$actual_deriv) ; summary(df_ice_i$actual_deriv)
      # # lims_h_in <- c(-0.05, 0.05) ; lims_m_in <- c(-0.004, 0.004)
      # # lims_h_in <- c(-0.1, 0.1) ; lims_m_in <- c(-0.02, 0.02)
      # # lims_h_in <- c(-0.3, 0.3) ; lims_m_in <- c(-0.08, 0.08)
      # lims_h_in <- c(-1.5, 1.5) ; lims_m_in <- c(-0.25, 0.25) # mu_kurt/shannon & lambda_xt
      # # lims_h_in <- c(-40, 40) ; lims_m_in <- c(-10, 10)
      # # lims_h_in <- c(-0.02, 0.02) ; lims_m_in <- c(-0.005, 0.005)
      # # if(var_name_i == "fhd_avg_diversity" | var_name_i == "kurt_avg_diversity")  { lims_h_in <- c(-0.2, 0.2) ; lims_h_in <- c(-0.1, 0.1) }
      # # if no limits provided then use quantiles
      # # if(lims_m_i == F)  lims_m_i <- quantile( df_ice_i[['actual_deriv']] , probs = c(0.15, 0.85)) ; if(lims_h_i == F)  lims_h_i <- quantile( df_ice_i[['actual_deriv']] , probs = c(0.05, 0.95))
      # 
      # #paste0('ICEpartial derivative\n', l_label_res ,              ' ~ ', l_lables_metrics[[var_name_i]])
      # 
      # # make hist and map (save)
      # h_dist <- make_hist(df_dice, 'actual_deriv', 'ICE partial derivative' , lims_h_in)
      # # g_input <- make_map(df_dice, 'actual_deriv', 'ICE partial derivative (actual)', var_name_i, lims_m_in)
      # g_input <- make_map(df_dice, 'actual_deriv', '', 
      #                     paste0( l_label_res , ' ~ ', l_label_div ), 
      #                     lims_m_in)
      # ggsave(plot = g_input, filename = paste0(output_path, 'm_', 'ICE' ,'_', var_name_i, '_targ-', target_name_k, '_absRR-', b_useAbs_RestRate, '.png' ) ) # , width = wid, height = hei)
      # 
      # g_draw <- f_combine_map_hist(g_input, h_dist, b_cut_title = F)
      # # combine map and histogram and save
      # # g_draw <- ggdraw( clip = 'off') +
      # #   draw_plot(g_input, x = 0.23, y = 0.23, width = 0.77, height = 0.77, hjust = 0.3) +
      # #   draw_plot(h_dist , x = 0, y = 0,    width = 0.77, height = 0.24, hjust = 0 )
      # ggsave(plot = g_draw, filename = paste0(output_path, 'g_ICE_comb_', var_name_i, '_targ-', target_name_k, '_absRR-', b_useAbs_RestRate, '.png' ) , width = fig_width, height = fig_height ) # , width = wid, height = hei)
      # 
      # 
      # print( paste0('diversity variable : ', var_name_i , ' model plotting complete for res variable : ', target_name_k) )
      # f_time_update(t_start_time)
      # # rf_end_time <- Sys.time() ; print(rf_end_time)      # initialise time
      # # rf_duration <- rf_end_time - rf_start_time ; print(rf_duration)
      
    } # end loop over div metrics
  } # end loop over res metrics
  
} # end run ICE