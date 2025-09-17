# ########################################################
# Title         : plotRF_partialPlot_plotDF.R
# Description   : plot the partial plot dataframes created by plotRF_partialPlot_createDF.R
# Aims          : create partial plots
# Inputs	      : df of partial plot values
# Outputs	      : partial plot of diversity (1D)
# Options	      : 
# Date          : 2025-02-19
# Version       : 1
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

# load the pdp file to plot
load(input_plot_pdp) # summary(df_partDep_div)

###################################################
###### PARTIAL DEPENDENCE GROUP PLOTTING 1D   #####
###################################################

# we have the dataframes of partial dependence - select whether diversity metrics or 'no_diversity' metric used
if(b_run_partialplot_nonDiv) pdp_plot <- df_partDep_nondiv # may not be fully functional in this code version
if(b_run_partialplot       ) pdp_plot <- df_partDep_div

###################################################
###### PLOT ALL THE DIV METRICS - BOTH LAMBDA   #####
###################################################

# select on certain resilience metrics
pdp_plot <- pdp_plot  %>% filter(  res_metric %in% v_target  ) 

# select only test or train, or leave both
# if(s_plot_pdp_dataset != 'all'){
#   pdp_plot <- pdp_plot  %>% filter(  test_train %in% s_plot_pdp_dataset  )  }

all_metrics <- unique(pdp_plot$div_metric)

# loop over the different pdp plotting groups 
for (j in 1:length(all_metrics)){ 
  plot_var_group <- paste(all_metrics[[j]]) ; print(plot_var_group[[1]])
  
  pdp_plot_group <- pdp_plot  %>% filter(  div_metric %in%  plot_var_group ) 
  # summary(pdp_plot)
  # pdp_plot_group <- pdp_plot_group[complete.cases(pdp_plot_group), ] 
  pdp_plot_group <- data.frame(pdp_plot_group)
  
  # extract x-axis lims from diversity metric
  lim_x <- l_lims_in[[plot_var_group]] ; print(lim_x)
  
  # invert directions if want abs rest rate or Kurtosis
  y_lab_i <- 'Rest Rate.'
  if( b_useAbs_RestRate  ){
    pdp_plot_group$yhat <- -1 * pdp_plot_group$yhat
    y_lab_i <- paste0( '|', y_lab_i , '|' )
    print('invert rest rate')
  }
  if(b_invert_mu_kurt & plot_var_group== 'Kurtosis'){
    pdp_plot_group$div_val <- -1 * pdp_plot_group$div_val 
    lim_x <- - lim_x[c(2, 1)]
    print('invert Kurtosis')
  }
  
  
  # create single metric 
  g_pdp <- f_plot_partial(pdp_plot_group, var_name_x = 'div_val', var_name_y = 'yhat', 
                          # var_name_x = 'value_div', var_name_y = 'value_res'
                          var_label_x = paste0(plot_var_group), 
                          var_label_y = y_lab_i ,
                          lims_x = lim_x,  lims_y = y_lims_lambda,
                          add_hist_under = F) #, add_error_band = F )
  
  # add groups to the figures
  # g_pdp <- g_pdp + aes(color = res_metric)
  g_pdp <- g_pdp + aes(color = res_metric )    + labs(color = 'Metric') 
  g_pdp <- g_pdp + scale_color_manual(name = "lambda", values = colors_resilience_metrics)
  # g_pdp <- g_pdp + aes(linetype = test_train)  + labs(linetype = 'Dataset') # compare test train
  
  # place legend below/remove
  g_pdp <- g_pdp + theme(legend.position = "none") # "bottom")
  
  ggsave(filename = paste0('g_partDepPlot_divMetrics', '_targ-lambda-', plot_var_group , '_data-', s_plot_pdp_dataset , '.png'), plot = g_pdp, path = output_path, width = fig_width_wide, height = fig_height_wide)
  
}
