# ########################################################
# Title         : plotRF_partialPlot_plotDF_original.R
# Description   : plot the partial plot dataframes created by plotRF_partialPlot_createDF..R
#                 plot RF output to analyse performance and, in particular, variability of the 
#                 model to different diversity metrics 
# Aims          : analyse performance and diversity metrics
# Inputs	      : df of partial plot values
# Outputs	      : 
#                 figures for each diversity: 1) performance 2) Importance 3) dice 
#                 summary figures: 1) partial plot of diversity 2) ranked average Importance 3) Importance of diversity metrics
# Options	      : 
# Date          : 21/12/23
# Version       : 1
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : Could extend with other metrics as well as partial plots of other variables
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())            

script_subtitle <- 'partialPlot'
# load RF plotting functions
source('3_create_figures/functions/f_plotRF_initialise.R')

# load the pdp file to plot
load(input_plot_pdp) # summary(df_partDep_div)

###################################################
###### PARTIAL DEPENDENCE GROUP PLOTTING 1D   #####
###################################################

# we have the dataframes of partial dependence 
### Now plot the diversity figs
# load('/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_2023-11-29_partialPlot/df_rf_model_partDep_diversity_data-train.RData')
if(b_run_partialplot_nonDiv) pdp_plot <- df_partDep_nondiv
if(b_run_partialplot       ) pdp_plot <- df_partDep_div


###################################################
###### PLOT ALL THE DIV METRICS - BOTH LAMBDA   #####
###################################################

# select on certain resilience metrics
sel_res_metrics <- c( 'kndvi_lambda_xt' , 'kndvi_lambda_variance')
pdp_plot <- pdp_plot  %>% filter(  metric_res %in% sel_res_metrics  ) #  %>% filter(  !variable %in%  var_group_2 ) # exclude with !    

# select only test or train, or leave both
if(s_plot_pdp_dataset != 'all'){
  pdp_plot <- pdp_plot  %>% filter(  test_train %in% s_plot_pdp_dataset  ) #  %>% filter(  !variable %in%  var_group_2 ) # exclude with !    
}

all_metrics <- unique (pdp_plot$metric_div)


# loop over the different pdp plotting groups 
for (j in 1:length(all_metrics)){ # j <- 10
  plot_var_group <- paste(all_metrics[[j]]) ; print(plot_var_group[[1]])
  
  pdp_plot_group <- pdp_plot  %>% filter(  metric_div %in%  plot_var_group ) #  %>% filter(  !variable %in%  var_group_2 ) # exclude with !    
  # summary(pdp_plot)
  # pdp_plot_group <- pdp_plot_group[complete.cases(pdp_plot_group), ] 
  pdp_plot_group <- data.frame(pdp_plot_group)
  
  lim_x <- l_lims_in[[plot_var_group]] ; print(lim_x)
  
  # # new method - via function
  g_pdp <- f_plot_partial(pdp_plot_group, var_name_x = 'value_div', var_name_y = 'value_res', # 'TAC', #, line_color = 'black',
                          var_label_x = paste0(plot_var_group), var_label_y = 'lambda' ,
                          # b_include_testANDtrain = 
                          lims_x = lim_x,  lims_y = y_lims_lambda,
                          add_hist_under = b_plot_hist, add_error_band = F )
  
  # add groups to the figures
  # g_pdp <- g_pdp + aes(color = metric_res)
  g_pdp <- g_pdp + aes(color = metric_res )    + labs(color = 'Metric') 
  g_pdp <- g_pdp + scale_color_manual(name = "lambda", values = colors_resilience_metrics)
  g_pdp <- g_pdp + aes(linetype = test_train)  + labs(linetype = 'Dataset') 
  
  # Save the legend of only one
  if(j == 1) ggsave(filename = paste0('g_partDep_divMetrics', '_targ-lambda-', plot_var_group , '_data-', s_plot_pdp_dataset , '_legend.png'), plot = g_pdp, path = output_path, width = fig_width_wide, height = fig_height_wide)
  # place legend below/remove
  g_pdp <- g_pdp + theme(legend.position = "none") # "bottom")
  # # Add histogram - not possible if joining variables together
  
  ggsave(filename = paste0('g_partDep_divMetrics', '_targ-lambda-', plot_var_group , '_data-', s_plot_pdp_dataset , '.png'), plot = g_pdp, path = output_path, width = fig_width_wide, height = fig_height_wide)
  
  # add histogram
  if(b_plot_hist){
    # as it doesn't matter for the histogram which metric we use - just put any
    target_name_k <- sel_res_metrics[1]
    # load dataframes of variables containing test/train split
    s_name_df_comb <-  paste0(input_dir, 'df_all_div-', plot_var_group, '_targ-', target_name_k, '.RData')
    load( s_name_df_comb )        # df_comb_i      head(df_comb_i)
    
    # # initialise train/test df and df to use in analysis - select only those needed
    df_pdp <- f_plotRF_load_RFDF(df_comb_i, s_plot_pdp_dataset) #; head(df_pdp)
    
    #### CREATE HIST AND ADD TO FIGURE ###
    
    g_hist <- f_plot_partial_hist(df_pdp, plot_var_group, n_bins_x = n_bins/2, lims_x = lim_x )
    g_pdp_hist <- grid.arrange(g_pdp, g_hist, ncol = 1, heights = c(3, 1))
    
    ggsave(filename = paste0('g_partDepHist_divMetrics', '_targ-lambda-', plot_var_group , '_data-', s_plot_pdp_dataset , '.png'), plot = g_pdp_hist, path = output_path, width = fig_width_wide, height = fig_height_wide)
  }
  
}

###################################################
###### NOT YET ADDED   #####
###################################################



# loop over the different pdp plotting groups 
for (j in 1:length(pdp_groups)){ # j <- 1
  plot_var_group <- pdp_groups[[j]]
  
  pdp_plot_group <- pdp_plot  %>% filter(  metric_div %in%  plot_var_group ) #  %>% filter(  !variable %in%  var_group_2 ) # exclude with !    
  # summary(pdp_plot_group)
  # pdp_plot_group <- pdp_plot_group[complete.cases(pdp_plot_group), ] 
  pdp_plot_group <- data.frame(pdp_plot_group)
  
  # # new method - via function
  g_pdp <- f_plot_partial(pdp_plot_group, var_name_x = 'value_div', var_name_y = 'value_res', # 'TAC', #, line_color = 'black',
                          lims_x = F,  lims_y = y_lims_TAC,
                          add_hist_under = F, add_error_band = F )
  
  # add groups to the figures
  # g_pdp <- g_pdp + aes(color = metric_res)
  g_pdp <- g_pdp + aes(color = metric_div)
  # # Add histogram - not possible if joining variables together
  
  ggsave(filename = paste0('g_partDep_divMetrics', '_targ-', target_name_k, '_group-', j ,'.png'), plot = g_pdp, path = output_path, width = fig_width_wide, height = fig_height_wide)
  
  
} # finish loop of pdp_groupings





############################################################
############################################################ IN THEORY THE BELOW IS SUPERSEEDED
############################################################

# # old method - plot groups together
# ggp <- ggplot( pdp_plot_group , aes(value_div, TAC) ) +#, color = variable)) +    # Draw ggplot2 plot with one axis
#   geom_line( size = 1 ) +
#   xlab(plot_var_group) + #xlab('Diversity metric') + # xlab('Median height') +
#   ylab('Temporal autocorrelation') +
#   ylim( y_lims_pdp ) +
#   # scale_color_manual(values=group.colors) +
#   # theme(legend.position="bottom") + # theme(legend.position="none")
#   basic_graph_theme

###################################################
######     PARTIAL DEPENDENCE NON-DIV VARS    #####
###################################################
# produce partial plots for the non-diversity variables only

df_partDep_nondiv <- data.frame() # data frame containing partial dependence points for each non diversity

# do not run 'no_diversity' partial plots 
if(b_run_partialplot_nonDiv){
  var_name_i <- var_i_nonDiv_partPlot
  
  # load rf model
  load( paste0(input_dir, 'rf_model_div-',var_name_i, '.RData' ) ) # rf.model load the rf model for each div variable
  # load dataframes of variables containing test/train split
  # load df containing all test train data
  load( paste0(input_dir, 'df_all_div-',var_name_i, '.RData') )        # df_comb_i      head(df_comb_i)
  # # initialise train/test df
  df_comb.train_i <- subset(df_comb_i, train_sample == T) # head(df_comb.train_i)
  df_comb.test_i  <- subset(df_comb_i, train_sample == F) # head(df_comb.test_i)
  
  if(s_use_pdp_dataset == 'train') df_pdp <-  df_comb.train_i[complete.cases(df_comb.train_i), ]
  if(s_use_pdp_dataset == 'test')  df_pdp <-  df_comb.test_i[complete.cases(df_comb.test_i), ]
  if(s_use_pdp_dataset == 'all')   df_pdp <-  df_comb_i[complete.cases(df_comb_i), ]
  
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
    
    # save(pp_j, file=paste0(output_path, 'df_pdp_-',var_name_i, '-', var_name_j , '.RData' )    ) # load(/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-03-21_plot/df_ice_test-fhd_avg_diversity.RData)
    
    if (j == 1){df_partDep_nondiv <- pp_j
    } else{df_partDep_nondiv <- rbind(df_partDep_nondiv, pp_j)}
    
  } # loop over v_predictors (non-diversity)
  
  
  # plot the different pdps
  
  # organise the df of pdp values for plotting
  head(df_partDep_nondiv) ; summary(df_partDep_nondiv, 16)
  names(df_partDep_nondiv) <- c( "value_div" , "TAC", "metric_div", "metric_res")
  df_partDep_nondiv$metric_div <- as.factor(df_partDep_nondiv$metric_div)
  save(df_partDep_nondiv, file=paste0(output_path, 'df_rf_model_partDep_nonDiv', '_targ-', target_name_k, '.RData' )    )
  # load( paste0(output_path, 'df_rf_model_partDep.RData' )    ) # give the option to load also
  
  # loop over the different pdp plotting groups 
  for (j in 1:length(pdp_groups_nonDiv)){ # j <- 13
    plot_var_group <- pdp_groups_nonDiv[[j]]
    # plot_var_group <- v_predictors[[j]]
    
    df_partDep_nonDiv_plot <- df_partDep_nondiv  %>% filter(  metric_div %in%  plot_var_group ) #  %>% filter(  !variable %in%  var_group_2 ) # exclude with !    
    # summary(df_partDep_nonDiv_plot)
    
    ggp <- ggplot( df_partDep_nonDiv_plot , aes(value_div, value_res) ) +#, color = variable)) +    # Draw ggplot2 plot with one axis
      geom_line( size = 1) +
      xlab(plot_var_group) + #xlab('Diversity metric') + # xlab('Median height') +
      ylab('Temporal autocorrelation') +
      ylim( y_lims_pdp ) +
      # scale_color_manual(values=group.colors) +
      # theme(legend.position="bottom") + # theme(legend.position="none")
      basic_graph_theme 
    
    
    # g_pdp <- f_plot_partial(pdp_list_mean, var_name_x = var_name_i, var_name_y = 'mean_yhat', 
    #                         lims_x = lims_in,  lims_y = y_lims_pdp,
    #                         add_hist_under = T, add_error_band = T, var_name_se = 'se_yhat')
    # 
    # g_hist <- f_plot_partial_hist(df_comb.train_i, var_name_i, n_bins_x = n_bins/2, lims_x = lims_in )
    # 
    # # the difficulty (yet to fully get right) is to match the axes of the x-axes of histogram and partial. 
    # # The only way I've managed to match them so far is by having fixed text_size at 8 for the histogram of frequency. It's all quite messy
    # 
    # # x-axis hist only
    # g_pdp_hist <- grid.arrange(g_pdp, g_hist, ncol = 1, heights = c(3, 1))
    
    # ggp
    ggsave(filename = paste0('g_partDep_nonDivMetrics-', plot_var_group , '_targ-', target_name_k, '.png'), plot = ggp, path = output_path, width = fig_width_wide, height = fig_height_wide)
    
    
  } # finish loop of pdp_groupings
  
} # end cycle non-div partplots
