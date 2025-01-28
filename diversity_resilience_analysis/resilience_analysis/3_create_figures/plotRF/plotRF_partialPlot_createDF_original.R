# ########################################################
# Title         : plot_rf_partialPlot.R
# Description   : create a dataframe containing data of partial plots from random forest models
#                 model to different diversity metrics 
# Aims          : analyse performance and diversity metrics
# Inputs	      : rf models with different diversity metrics in each 
# Outputs	      : DF of partial plots (to be plotted by a separate script)
#                 figures for each diversity: 1) performance 2) Importance 3) dice 
#                 summary figures: 1) partial plot of diversity 2) ranked average Importance 3) Importance of diversity metrics
# Options	      : 
# Date          : 21/12/23
# Version       : 3
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : 
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


# initialise df for both importance and partial dependence
df_partDep_div    <- data.frame() # data frame containing partial dependence points for each diversity
df_partDep_nondiv <- data.frame() # data frame containing partial dependence points for each non diversity

if(b_run_partialplot_nonDiv | b_run_partialplot | b_run_partialplot_2d){
  
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
    # s_name_rf_model <-  paste0(input_dir, 'rf_model_div-',var_name_i, '_targ-', target_name_k, '_seed-102.RData' ) 
    s_name_rf_model <-  paste0(input_dir, 'list_rf_model_results_parallelDiv_div-',var_name_i, '_targ-', target_name_k, '.RData' ) 
    load(s_name_rf_model) # rf.model - load the rf model for each div variable
    
    # load dataframes of variables containing test/train split
    if(b_do_common_testTrainSplit){ 
      # select only relevant predictors and the identifiers
      if( var_name_i == "no_diversity") { v_all_vars <- c( v_identifiers, target_name_k, v_predictors)  
      } else{                             v_all_vars <- c( v_identifiers, target_name_k, var_name_i, v_predictors) }
        s_name_df_comb <-  paste0(input_dir, 'df_all.RData')
        load( s_name_df_comb )               # df_comb      names(df_comb)   ; dim(df_comb)
        df_comb_i <- df_comb[, v_all_vars]   # df_comb_i    names(df_comb_i) ; dim(df_comb)
        df_comb_i <- df_comb_i[complete.cases(df_comb_i), ] ; print(paste0('common test-train split, dim: ', dim(df_comb_i)))
    } else {
      s_name_df_comb <-  paste0(input_dir, 'df_all_div-',var_name_i, '_targ-', target_name_k, '.RData')
      load( s_name_df_comb )        # df_comb_i      head(df_comb_i)
      
    }
    
    # # initialise train/test df and df to use in analysis - select only those needed
    df_pdp <- f_plotRF_load_RFDF(df_comb_i, s_use_pdp_dataset) ; # head(df_pdp) ; # dim(df_pdp)
    # df_pdp <- result_temp[[1]] ; # df_comb.train_i <- result_temp[[2]] ; df_comb.test_i <- result_temp[[3]] ; rm(result_temp)
    
    ###################################################
    ######     PARTIAL DEPENDENCE NO_DIVERSITY    #####
    ###################################################
    
    # only create pdps for the other parameters for the no _diversity variable
    if(var_name_i == "no_diversity" & b_run_partialplot_nonDiv ){
      # cycle over all non-diversity predictors 
      # for (j in 1:length(v_predictors)){ # j <- 1
      for (j in 1:length(v_predictors)){ # j <- 1
        var_name_j <- v_predictors[j] ; print(var_name_j)# extract individual diversity predictor
        
        # select only complete cases
        df_pdp <- df_pdp[complete.cases(df_pdp), ]
        # pp_j <- partialPlot(rf.model, df_comb.train_i_complete, names(df_pdp)[3+j] ) # second name should be the var_name_j
        pp_j <- partialPlot(rf.model, df_pdp, names(df_pdp)[3+j] ) # second name should be the var_name_j
        # head(pp_j)
        pp_j <- as.data.frame(pp_j) ;  pp_j[1:2] <- pp_j[1:2] %>% round( digits = 5)
        pp_j[3] <- var_name_j
        pp_j[4] <- target_name_k

        # save(pp_j, file=paste0(output_path, 'df_pdp_-',var_name_i, '-', var_name_j ,  '_targ-', target_name_k, '.RData' )    ) # load(/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-03-21_plot/df_ice_test-fhd_avg_diversity.RData)
        
        if (dim(df_partDep_nondiv)[1] == 0){  df_partDep_nondiv <- pp_j  ; print('new df')
        } else{df_partDep_nondiv <- rbind(df_partDep_nondiv, pp_j)}
        
      } # loop over v_predictors (non-diversity)
      
      # organise the df of pdp values for saving and plotting
      head(df_partDep_nondiv) ; summary(df_partDep_nondiv, 16)
      # names(df_partDep_nondiv) <- c( "value" , "TAC", "variable")
      names(pp_i) <- c( "value_div" , "value_res", "metric_div", 'metric_res')
      df_partDep_nondiv$variable <- as.factor(df_partDep_nondiv$variable)
      save(df_partDep_nondiv, file=paste0(output_path, 'df_rf_model_partDep_', var_name_i, '_targ-', target_name_k, '_data-', s_use_pdp_dataset, '.RData' )    )
      
    } # end of no_diversity metric loop over parameters
      
    ###################################################
    ######     PARTIAL DEPENDENCE DIVERSITY       #####
    ###################################################
    # create dataframe showing the biodiv partial dependence
    if(b_run_partialplot){
      print('partial_dependence')
      # select only complete cases
      pdp_i <- partial(rf.model, pred.var = as.character(var_name_i)  , grid.resolution = n_pdp_points, train = df_pdp) # v_pdp
      # pdp_i<- partialPlot(rf.model, df_pdp, as.character(var_name_i) ) # second name should be the names(df_comb.train_i)[2]
      # head(pdp_i)
      pdp_i <- as.data.frame(pdp_i) #;  pdp_i[1:2] <- pdp_i[1:2] %>% round( digits = 7)
      # take the variable name and the stat name
      # pdp_i[3] <- strsplit(names(df_comb.train)[i], '_')[[1]][length(strsplit(names(df_comb.train)[i], '_')[[1]])] # names(df_comb.train)[i] ; # variable = final item in name
      # pdp_i[4] <- paste( strsplit(names(df_comb.train)[i], '_')[[1]][1: (length(strsplit(names(df_comb.train)[i], '_')[[1]]) - 1) ] , collapse = "_" ) # stat =  the 1st and last-1 terms and put together
      pdp_i[3] <- target_name_k
      pdp_i[4] <- var_name_i          #stringr::str_replace(var_name_i , '_diversity', '')
      # names(pdp_i) <- c( "value" , target_name_k, "variable")
      # names(pdp_i) <- c( "value_div" , "value_res", "metric_div", 'metric_res')
      names(pdp_i) <- c( 'div_val'   ,   'yhat' , 'res_metric',    'div_metric')
      if (dim(df_partDep_div)[1] == 0){  df_partDep_div <- pdp_i  ; print('new df') 
      } else{df_partDep_div <- rbind(df_partDep_div, pdp_i)}
      
      
      # now plot the partial with histogram under
      # set the x/y-limits of the variable
      lims_in <- l_lims_in[[var_name_i]]      
      lims_y  <- y_lims_pdp[[target_name_k]]        # 
      x_lab_i <- l_lables_metrics[[var_name_i]]
      y_lab_i <- l_lables_metrics[[target_name_k]]
      # if none provided then auto apply the full limits of the diveristy metric
      if ( is.null('lims_in') ) {  lims_in <- find_cutpts_from_percentile(pdp_i, 'value_div', 0.02) }
      
      if( b_useAbs_RestRate & ( target_name_k == 'kndvi_lambda_xt' | target_name_k == 'kndvi_lambda_variance' ) ){
        pdp_i$yhat <- -1 * pdp_i$yhat
        y_lab_i <- paste0( '|', l_lables_metrics[[target_name_k]], '|' )
        print('invert rest rate')
      }
      # invert diversity metrics?
      if(b_invert_mu_kurt & var_name_i== 'mu_kurt'){
        df_dice$actual_deriv <- -1 * df_dice$actual_deriv 
        x_lab_i <- paste0(x_lab_i, ' (inv)')
        print('invert mu_kurt')
      }
      
      # g_pdp   <- f_plot_partial(pdp_i, var_name_x = 'value_div', var_name_y = 'value_res', #, line_color = 'black',
      #                         var_label_y = target_name_k,
      #                         lims_x = lim_x,  lims_y = lim_y,
      #                         add_hist_under = T, add_error_band = F )
      
      g_pdp <- f_plot_partial(pdp_i, var_name_x = 'div_val', var_name_y = 'yhat', 
                              lims_x = lims_in,  lims_y = lims_y,
                              var_label_x = x_lab_i, var_label_y = y_lab_i,
                              # add_hist_under = F, add_error_band_even = T, var_name_se = 'se_yhat') 
                              add_hist_under = T)
                              # add_error_band_even = F, add_error_band_uneven = F, var_name_se = 'yhat_')  # var_name_se = 'se_yhat'
      
      
      # add a histogram below
      g_hist <- f_plot_partial_hist(df_pdp, var_name_i, n_bins_x = n_bins/2, lims_x = lims_in )
      g_pdp_hist <- grid.arrange(g_pdp, g_hist, ncol = 1, heights = c(3, 1))

      ggsave(filename = paste0('g_partDepHist_divMet-', var_name_i ,  '_targ-', target_name_k, '.png'), plot = g_pdp_hist, path = output_path, width = fig_width_wide, height = fig_height_wide)
    } # end pdp 1d diversity
    
    ###################################################
    ######     PARTIAL DEPENDENCE DIVERSITY 2D    #####
    ###################################################
    
    if(b_run_partialplot_2d){
      
      # loop over selected variabels for 2d pdp plots
      for (j in 1:length(pdp_2d_extra_vars)){ # j <- 1
        pdp_2d_var_j <- pdp_2d_extra_vars[j] # select independent var for 2d pdp
        print(paste0('2d pdp plot between ', var_name_i, ' and ', pdp_2d_var_j) )
        
        # loop over the different metrics for the 2d pdp
        # df_comb.train_i_complete <- df_comb.train_i[complete.cases(df_comb.train_i), ]
        
        pdp_2d <- partial(rf.model, pred.var = c(pdp_2d_var_j, var_name_i) )
        
        save(pdp_2d, file=paste0(output_path, 'df_pdp_2d-',var_name_i, '-', pdp_2d_var_j, '_targ-', target_name_k, '.RData' )    ) # load(/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-03-21_plot/df_ice_test-fhd_avg_diversity.RData)
        
        # create partial plot
        g_pdp1 <- plotPartial(pdp_2d) # plot(g_pdp1)
        save(g_pdp1, file=paste0(output_path, 'g_pdp_2d-',var_name_i, '-', pdp_2d_var_j, '_targ-', target_name_k, '.RData' )    ) # load(/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-03-21_plot/df_ice_test-fhd_avg_diversity.RData)
        f_time_update(t_start_time)
        
        # probably want to build a separate function for plloting by ggplot2 - can add features        
        # # plot the histogram below
        # # Plot instead the normalized histogram for kurtosis
        # x_hist <- make_hist(df_comb_i,  pdp_2d_var_j, pdp_2d_var_j, lims_in)
        #   ggplot(df_comb_i, aes_string(x = Kurtosis) ) +
        #   geom_histogram(binwidth = 0.1) +
        #   xlab("Kurtosis") +
        #   ylab("Frequency")
        
        
      } # end loop over 2d pdp vars
    } # end 2d pdp
    
  } # end loop over diversity variables
} # end loop over v_target metrics
  
  # save the produced partial plots
  if(b_run_partialplot){
    print('save dataframe output')
    # organise the df of pdp values for plotting
    head(df_partDep_div) ; summary(df_partDep_div, 16) ; dim(df_partDep_div)
    # names(df_partDep_div) <- c( "value" , target_name_k, "variable")
    df_partDep_div$div_metric <- as.factor(df_partDep_div$div_metric)
    df_partDep_div$res_metric <- as.factor(df_partDep_div$res_metric)
    # save(df_partDep_div, file=paste0(output_path, 'df_rf_model_partDep_diversity', '_targ-', target_name_k, '_data-', s_use_pdp_dataset, '.RData' )    )
    save(df_partDep_div, file=paste0(output_path, 'df_rf_model_partDep_diversity', '_data-', s_use_pdp_dataset, '.RData' )    )
    # load( paste0(output_path, 'df_rf_model_partDep_diversity.RData' )    ) # give the option to load also
  }

  # } # end loop over v_target metrics
} # end if b_run_partialplot




############################################################################################################################################
############################                   END                  ########################################################################
############################################################################################################################################


# just in case we need to amalgamate the different  df_partDep_div created above
# bind the different outputs and the different training/test sets

input_dir <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_selections_2024-01-25_partialPlot/training_data_input/'
l_files <- c('df_rf_model_partDep_diversity_targ-kndvi_lambda_variance_data-train.RData',   'df_rf_model_partDep_diversity_targ-kndvi_TAC_data-train.RData' , 'df_rf_model_partDep_diversity_targ-kndvi_lambda_xt_data-train.RData')
df_partDep_div_all <- data.frame()
for(f_file in l_files){
  print(f_file)
  load(paste0(input_dir, f_file))
  df_partDep_div_all <- rbind(df_partDep_div_all,  df_partDep_div)
}
df_partDep_div_all$test_train <- 'train'

input_dir <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_selections_2024-01-25_partialPlot/test_data_input/'
l_files <- c('df_rf_model_partDep_diversity_targ-kndvi_lambda_variance_data-test.RData',   'df_rf_model_partDep_diversity_targ-kndvi_TAC_data-test.RData' , 'df_rf_model_partDep_diversity_targ-kndvi_lambda_xt_data-test.RData')
for(f_file in l_files){
  print(f_file)
  load(paste0(input_dir, f_file))
  df_partDep_div$test_train <- 'test'
  df_partDep_div_all <- rbind(df_partDep_div_all,  df_partDep_div)
}
df_partDep_div_all$test_train <- as.factor(df_partDep_div_all$test_train)
summary(df_partDep_div_all)
df_partDep_div <- df_partDep_div_all

save(df_partDep_div, file=paste0(output_path, 'df_rf_model_partDep_diversity', '_data-', 'both', '.RData' )    )
