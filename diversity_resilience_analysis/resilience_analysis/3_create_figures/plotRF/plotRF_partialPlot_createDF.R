# ########################################################
# Title         : plotRF_partialPlot_createDF.R
# Description   : create a dataframe containing data of partial plots from random forest models
#                 model to different diversity metrics 
# Aims          : create partial plot dfs (also basic plotting)
# Inputs	      : rf models with different diversity metrics in each 
# Outputs	      : DF of partial plots (to be plotted by a separate script)
# Date          : 2025-02-19
# Authors       : Mark Pickering & Agata Elia
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())            

script_subtitle <- 'partialPlot'
# load RF plotting functions
source('3_create_figures/functions/f_plotRF_initialise.R')

###################################################
######     LOOP OVER RF DIVERSITY MODELS      #####
###################################################
# This first creates the pdps, In the next section it plots them
# for the diversity metrics, we loop over the 'other' predictors (e.g. climate) and gather the pdps
# we create pdps for the different diversity metrics and group them
# for the diversity metrics we also produce 2d histograms with selected metrics

# initialise df for partial dependence
df_partDep_div    <- data.frame() # data frame containing partial dependence points for each diversity
df_partDep_nondiv <- data.frame() # data frame containing partial dependence points for each non diversity


# loop over 'resilience target metrics' producing importance figure and adding each to an div importance dataframe
for (j in 1:length(v_target)){ 
  target_name_j <- v_target[j] ; print(target_name_j)
  # loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
  for (i in 1:length(v_optional_predictors)){ 
    var_name_i <- v_optional_predictors[i] ; print(var_name_i)# extract individual diversity predictor
    # time count
    f_time_update(t_start_time)
  
    ###################################################
    ####### LOAD RF AND DF                   ##########
    ###################################################
    
    # load basic rf model
    load( paste0(input_dir, 'rf_model_div-', var_name_i, '_targ-', target_name_j, '.RData' ) )
    # load bootstrapped rf models - for averaging or calculating variability (to do as a prior step)
    # load( paste0(input_dir, 'list_rf_model_pdp_results_boot_parallel_nIter-20_div-',
    #              var_name_i, '_seed-99_targ-', target_name_j, '.RData') )
    
    # load dataframes of variables containing test/train split
    load( paste0(input_df, 'df_all', '.RData') )        # df_comb_i      head(df_comb_i)
    
    if( var_name_i == "no_diversity") { v_all_vars <- c( v_identifiers, target_name_j, v_predictors)  
    } else{                 v_all_vars <- c( v_identifiers, target_name_j, var_name_i, v_predictors)}
    df_comb_i <- df_comb[, v_all_vars]   # df_comb_i    names(df_comb_i) ; dim(df_comb)
    
    # # initialise train/test df and df to use in analysis - select only those needed
    df_pdp <- f_plotRF_load_RFDF(df_comb_i, s_use_pdp_dataset) ; # head(df_pdp) ; # dim(df_pdp)

    ###################################################
    ######     PARTIAL DEPENDENCE DIVERSITY       #####
    ###################################################
    # create dataframe showing the biodiv partial dependence - 1D
  
    if(b_run_partialplot){
      print('partial_dependence')
      # select only complete cases
      pdp_i <- partial(rf.model, pred.var = as.character(var_name_i)  , grid.resolution = n_pdp_points, train = df_pdp)
      pdp_i <- as.data.frame(pdp_i) 
      
      # take the variable name and the stat name
      pdp_i[3] <- target_name_j
      pdp_i[4] <- var_name_i         
      # df_partDep_div[[5]] <- s_use_pdp_dataset
      names(pdp_i) <- c( 'div_val'   ,   'yhat' , 'res_metric',    'div_metric')
      if (dim(df_partDep_div)[1] == 0){  df_partDep_div <- pdp_i  ; print('new df') 
      } else{df_partDep_div <- rbind(df_partDep_div, pdp_i)}
      
      # now plot the partial with histogram under
      # set the x/y-limits of the variable
      lims_in <- l_lims_in[[var_name_i]]      
      lims_y  <- y_lims_pdp[[target_name_j]]        
      x_lab_i <- l_lables_metrics[[var_name_i]]
      y_lab_i <- l_lables_metrics[[target_name_j]]
      # if none provided then auto apply the full limits of the diveristy metric
      if ( is.null('lims_in') ) {  lims_in <- find_cutpts_from_percentile(pdp_i, 'value_div', 0.02) }
      
      if( b_useAbs_RestRate & ( target_name_j == 'kndvi_lambda_xt' | target_name_j == 'kndvi_lambda_variance' ) ){
        pdp_i$yhat <- -1 * pdp_i$yhat
        y_lab_i <- paste0( '|', l_lables_metrics[[target_name_j]], '|' )
        print('invert rest rate')
      }
      # invert diversity metrics?
      if(b_invert_mu_kurt & var_name_i== 'Kurtosis'){
        lims_in <- c( -1*lims_in[2], -1*lims_in[1])
        pdp_i$div_val <- -1 * pdp_i$div_val 
        # df_pdp[var_name_i] <- -1*df_pdp[var_name_i]
        x_lab_i <- paste0(x_lab_i, ' (inv)')
        print('invert mu_kurt')
      }
      
      # create a basic figure of the partial dependence
      g_pdp <- f_plot_partial(pdp_i, var_name_x = 'div_val', var_name_y = 'yhat', 
                              lims_x = lims_in,  lims_y = lims_y,
                              var_label_x = x_lab_i, var_label_y = y_lab_i,
                              # add_hist_under = F, add_error_band_even = T, var_name_se = 'se_yhat') 
                              add_hist_under = T)
                              # add_error_band_even = F, add_error_band_uneven = F, var_name_se = 'yhat_')  
      
      
      # add a histogram below
      # g_hist <- f_plot_partial_hist(df_pdp, var_name_i, n_bins_x = n_bins/2, lims_x = lims_in )
      # g_pdp_hist <- grid.arrange(g_pdp, g_hist, ncol = 1, heights = c(3, 1))

      ggsave(filename = paste0('g_partDepHist_divMet-', var_name_i ,  
                               '_targ-', target_name_j, '.png'), plot = g_pdp_hist, 
             path = output_path, width = fig_width_wide, height = fig_height_wide)
    } # end pdp 1d diversity if block
    
    ###################################################
    ######     PARTIAL DEPENDENCE DIVERSITY 2D    #####
    ###################################################
    
    if(b_run_partialplot_2d){
      
      # loop over selected variabels for 2d pdp plots
      for (k in 1:length(pdp_2d_extra_vars)){ 
        pdp_2d_var_k <- pdp_2d_extra_vars[k] # select independent var for 2d pdp
        print(paste0('2d pdp plot between ', var_name_i, ' and ', pdp_2d_var_k) )
        
        pdp_2d <- partial(rf.model, pred.var = c(pdp_2d_var_k, var_name_i), train = df_pdp )
        
        save(pdp_2d, file=paste0(output_path, 'df_pdp_2d-',var_name_i, '-', pdp_2d_var_k,
                                 '_targ-', target_name_j, '.RData' )    ) 
        
        # create partial plot
        g_pdp1 <- plotPartial(pdp_2d) # plot(g_pdp1)
        save(g_pdp1, file=paste0(output_path, 'g_pdp_2d-',var_name_i, '-', pdp_2d_var_k, 
                                 '_targ-', target_name_j, '.RData' )    ) # 
        f_time_update(t_start_time)
        
      } # end loop over 2d pdp vars
    } # end 2d pdp
    
  } # end loop over diversity variables
} # end loop over v_target metrics
  
# save the produced partial plots
if(b_run_partialplot){
  print('save dataframe output')
  # organise the df of pdp values for plotting
  head(df_partDep_div) ; summary(df_partDep_div, 16) ; dim(df_partDep_div)
  
  df_partDep_div$div_metric <- as.factor(df_partDep_div$div_metric)
  df_partDep_div$res_metric <- as.factor(df_partDep_div$res_metric)
  # df_partDep_div$test_train <- as.factor(df_partDep_div$test_train)
  
  save(df_partDep_div, file=paste0(output_path, 'df_rf_model_partDep_diversity', '_data-', s_use_pdp_dataset, '.RData' )    )
}




