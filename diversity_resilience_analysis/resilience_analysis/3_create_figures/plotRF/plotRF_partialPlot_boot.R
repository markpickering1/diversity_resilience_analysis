# ########################################################
# Title         : plotRF_partialPlot_boot.R
# Description   : plot bootstrapped RF models showing the mean and unc band
# Aims          : produce bootstrapped partial plots
# Inputs	      : df of already calculated pdps with different diversity metrics in each 
# Outputs	      : partial plots of diversity with uncertainty bands
# Date          : 2025-02-19
# Authors       : Mark Pickering & Agata Elia
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())            

script_subtitle <- 'partialPlot_boot'
# load RF plotting functions
source('3_create_figures/functions/f_plotRF_initialise.R')

###################################################
######     PLOT RES METRICS SEPARATELY        #####
###################################################

# initialise combined df_pdp of all res and div metrics
df_pdp <- data.frame()

print('run boostrapped pdp')

# loop over 'diversity metrics' producing partial figure
for (i in 1:length(v_optional_predictors)){ 
  var_name_i <- v_optional_predictors[i] ; print(var_name_i)
  
  # select diversity dataset and model - default is the diversity metric being plotted
  div_metric_var_name <- var_name_i                    
  
  # if not plotting the diversity metrics, then select which diversity metric to use in the model 
  # normally if we are plotting e.g. climate metrics, we will use the 'no_diversity' data and model
  if(b_run_partialplot_nonDiv){  div_metric_var_name <- var_i_nonDiv_partPlot ; print(paste0('rf model diversity metric: ', div_metric_var_name)) }

  ###################################################
  ####### LOAD PRE-CREATED PDP             ##########
  ###################################################
  
  # load dataframes of variables containing test/train split
  load( paste0(input_df, 'df_all', '.RData') )        # df_comb      head(df_comb)
  
  # # initialise train/test df and df to use in analysis - select only those needed
  df_comb.train_i <- f_plotRF_load_RFDF(df_comb, s_use_pdp_dataset) ; # head(df_pdp) ; # dim(df_pdp)
  
  load( paste0(input_dir_pdp_para, 'df_pdp_div-', div_metric_var_name , '.RData' ) ) 
  # summary(df_pdp_all) ; # length(unique(df_pdp_all$div_val))
  
  # Calculate mean and quantiles for uncertainty bands
  df_pdp_all_metrics <- df_pdp_all %>% group_by( div_val, res_metric, div_metric) %>%  
    summarise(mean_yhat = mean(yhat), 
              # calculate the standard deviation
              se_yhat = sd(yhat), 
              # calculate a 95% CI band instead
              yhat_lower = quantile(yhat, probs = 0.025), yhat_upper = quantile(yhat, probs = 0.975))
  # summary(df_pdp_all_metrics)
  
  # should use the absolute value of restoration rate?
  y_lab_i_lambda <-  l_lables_metrics[['kndvi_lambda']]
  if( b_useAbs_RestRate  ){
    #df_pdp_all_metrics$mean_yhat  <- -1 * df_pdp_all_metrics$mean_yhat 
    df_pdp_all_metrics <- df_pdp_all_metrics %>%
      mutate(
        across(c(mean_yhat, yhat_lower, yhat_upper), 
               ~ if_else(res_metric %in% c("kndvi_lambda_variance", "kndvi_lambda_xt"), . * -1, .)),
        temp       = yhat_upper,
        yhat_upper = if_else(res_metric %in% c("kndvi_lambda_variance", "kndvi_lambda_xt"), 
                             yhat_lower, yhat_upper),
        yhat_lower = if_else(res_metric %in% c("kndvi_lambda_variance", "kndvi_lambda_xt"), 
                             temp,       yhat_lower)
      )
    y_lab_i_lambda <- paste0( '|', l_lables_metrics[['kndvi_lambda']] , '|' )
    df_pdp_all_metrics$temp <- NULL
    print('invert rest rate')
  }
  
  # invert diversity metrics?
  lims_in_x <- l_lims_in[[var_name_i]]   
  x_lab_i <- l_lables_metrics[[var_name_i]]
  var_name_i_filename <- var_name_i
  if(b_invert_mu_kurt & var_name_i== 'Kurtosis'){
    df_pdp_all_metrics$div_val <- -1 * df_pdp_all_metrics$div_val 
    df_comb.train_i$Kurtosis    <- -1 * df_comb.train_i$Kurtosis
    var_name_i_filename <- paste0(var_name_i, '_inv')
    # x_lab_i <- paste0(l_lables_metrics[[var_name_i]], ' (inverse)')
    lims_in_x <- - lims_in_x[c(2, 1)]
    print('invert Kurtosis')
  }
  
  # loop over resilience metrics
  for (j in 1:length(v_target)){  
    target_name_j <- v_target[j] ; print(target_name_j)
  
    lims_y  <- y_lims_pdp[[target_name_j]]        # 
    y_lab_i <- l_lables_metrics[[target_name_j]]
    
    if( b_useAbs_RestRate  & ( target_name_j == 'kndvi_lambda_xt' | 
                               target_name_j == 'kndvi_lambda_variance' ) ){
      y_lab_i <- paste0( '|',y_lab_i, '|' )
    }
    
    ###################################################
    ######     CALC PDP MEAN/UNC                 #####
    ###################################################
    # create dataframe showing the mean partial dependence vals
    # different methods for the unc band - e.g. s.d. 
    
    # calculate the error on the mean from the standard error on the model values
    pdp_list_mean <- df_pdp_all_metrics %>%  filter( # div_metric == var_name_i, 
                                             res_metric == target_name_j ) 
    
    pdp_list_mean <- as.data.frame(pdp_list_mean) # summary(pdp_list_mean)
    # length(unique(pdp_list_mean$div_val))
    # extract the max and min y-limits
    print( max(pdp_list_mean$mean_yhat + pdp_list_mean$se_yhat) )
    print( min(pdp_list_mean$mean_yhat - pdp_list_mean$se_yhat) )

    ###################################################
    ######     PLOT PDP MEAN + UNC BAND           #####
    ###################################################
    
    # set the x-limits of the variable
    
    # g_pdp <- f_plot_partial(pdp_list_mean, var_name_x = var_name_i, var_name_y = 'mean_yhat',
    g_pdp <- f_plot_partial(pdp_list_mean, var_name_x = 'div_val', var_name_y = 'mean_yhat', 
                            lims_x = lims_in_x,  lims_y = lims_y,
                            var_label_x = x_lab_i, var_label_y = y_lab_i,
                          # add_hist_under = F, add_error_band_even = T, var_name_se = 'se_yhat') 
                            add_hist_under = F, add_error_band_even = F, add_error_band_uneven = T, var_name_se = 'yhat_')  # var_name_se = 'se_yhat'
    
    # save
    ggsave(plot = g_pdp, path = output_path, filename = paste0( 'g_partDep_boot_var-', var_name_i_filename ,'_targ-', target_name_j, '_absRR-', b_useAbs_RestRate ,'_betterLims.png'),  width = fig_width_wide  , height = fig_height_wide)
    
    # now replot with hist below
    g_pdp <- f_plot_partial(pdp_list_mean, var_name_x = 'div_val', var_name_y = 'mean_yhat', 
                            lims_x = lims_in_x,  lims_y = lims_y,
                            var_label_x = x_lab_i, var_label_y = y_lab_i,
                            # add_hist_under = F, add_error_band_even = T, var_name_se = 'se_yhat') 
                            add_hist_under = T, 
                            add_error_band_even = F, add_error_band_uneven = T, 
                            var_name_se = 'yhat_')  # var_name_se = 'se_yhat'
    
    
    # create histogram with out of bounds added
    g_hist <- f_plot_partial_hist(df_comb.train_i, var_name_i, var_label_x = x_lab_i, 
                                  n_bins_x = n_bins/2, lims_x = lims_in_x )
    g_pdp_hist <- grid.arrange(g_pdp, g_hist, ncol = 1, heights = c(3, 1))
    
    ggsave(path = output_path, filename = paste0( 'g_partDep_boot_var-', var_name_i_filename ,'_targ-', target_name_j, '_absRR-', b_useAbs_RestRate ,'_hist_betterLims.png'), plot = g_pdp_hist, width =  fig_height_wide, height = fig_width_wide)
 
  } # end loop over resilience metric
  
  # include all the different diversity metrics in the overall dataframe
  df_pdp <- rbind(df_pdp, df_pdp_all_metrics)
  
} # end loop over pdp variables

# save the overall df containing the different pdps with unc band
save(df_pdp , file=paste0(output_path, 'df_pdp_unc.RData' )    ) # the built-in plot

###################################################
######  RESCALE AND PLOT DIV METRICS ON SAME FIG #####
###################################################
# now present the different diversity metrics on the same figure
# load(paste0('figures/plotRF_partialPlot_boot/df_pdp_unc.RData'))
# to present on same figure, rescale each of the diversity metrics to a value 0-1
# This value represents the 'diversity' relative to the global (e.g. European value)

# load df_all
load( paste0(input_df, 'df_all.RData' ) ) # head(df_comb)
df_comb.train_i <- subset(df_comb, train_sample == T) # head(df_comb.train_i)
# invert mu_kurt values if required
if(b_invert_mu_kurt ){
  df_comb.train_i$Kurtosis    <- -1 * df_comb.train_i$Kurtosis
  print('invert Kurtosis') }

# Compute the (e.g.) 5th and 95th percentiles for both columns 
# This is hardcoded for the three main metrics (should be changed to include other metrics)
mu_kurt_percentiles <- quantile(df_comb.train_i$Kurtosis, probs = c(percentile_alpha, 1- percentile_alpha), na.rm = TRUE)
shannon_entropy_percentiles <- quantile(df_comb.train_i$Shannon, probs = c(percentile_alpha, 1- percentile_alpha), na.rm = TRUE)
sd_rh98_percentiles <- quantile(df_comb.train_i$Canopy_heights_sd, probs = c(percentile_alpha, 1- percentile_alpha), na.rm = TRUE)
hist(df_comb.train_i$Canopy_heights_sd)

# rescale the metrics of interest
df_comb.train_i$mu_kurt_rescaled <- rescale(df_comb.train_i$Kurtosis, percentile_alpha, mu_kurt_percentiles[1], mu_kurt_percentiles[2])
df_comb.train_i$shannon_entropy_rescaled <- rescale(df_comb.train_i$Shannon, percentile_alpha, shannon_entropy_percentiles[1], shannon_entropy_percentiles[2])
df_comb.train_i$sd_rh98_rescaled <- rescale(df_comb.train_i$Canopy_heights_sd, percentile_alpha, sd_rh98_percentiles[1], sd_rh98_percentiles[2])

## tests to check the scaling:
# plot(df_comb.train_i$mu_kurt_rescaled, df_comb.train_i$Kurtosis) ; 
# cor(df_comb.train_i$mu_kurt_rescaled, df_comb.train_i$Kurtosis, use = 'pairwise.complete.obs')
# plot(df_comb.train_i$shannon_entropy_rescaled, df_comb.train_i$Shannon) ; 
# cor(df_comb.train_i$shannon_entropy_rescaled, df_comb.train_i$Shannon, use = 'pairwise.complete.obs')
# hist(df_comb.train_i$shannon_entropy_rescaled) ; hist(df_comb.train_i$shannon_entropy)

# now rescale the pdp
# Apply the rescaling based on div_metric
df_pdp_rescale <- df_pdp %>%
  mutate(
    div_val_rescaled = case_when(
      div_metric == "Kurtosis" ~ rescale(div_val, percentile_alpha,  
                                          mu_kurt_percentiles[1], mu_kurt_percentiles[2]),
      div_metric == "Shannon" ~ rescale(div_val, percentile_alpha, 
                                          shannon_entropy_percentiles[1], shannon_entropy_percentiles[2]),
      div_metric == "Canopy_heights_sd" ~ rescale(div_val, percentile_alpha, 
                                          sd_rh98_percentiles[1], sd_rh98_percentiles[2]),
      TRUE ~ NA # If div_metric neither, NA ## mean_yhat  # If div_metric is neither, leave unchanged
    )
  )
df_pdp_rescale <- as.data.frame(df_pdp_rescale)

# now recreate plot but with all metrics shown for each resilience metric
l_labels_metrics_vector <- unlist(l_lables_metrics)
df_pdp_rescale$div_metric <- as.character(df_pdp_rescale$div_metric)
df_pdp_rescale$Metric <- l_labels_metrics_vector[df_pdp_rescale$div_metric]

df_pdp_rescale$div_metric <- factor(df_pdp_rescale$div_metric, levels = c("Canopy_heights_sd", "Kurtosis", "Shannon"))
summary(df_pdp_rescale) ; head(df_pdp_rescale)

# loop over the target variables and make a plot for each
for(jj in 1:length(v_target)){ # jj <- 1
  target_name_jj <- v_target[jj] ; print(target_name_jj)
  df_pdp_rescale_jj <- df_pdp_rescale %>% filter( res_metric == target_name_jj )
  
  y_lab_jj_lambda <-  l_lables_metrics[[target_name_jj]]
  if( b_useAbs_RestRate  & ( target_name_jj == 'kndvi_lambda_xt' | target_name_jj == 'kndvi_lambda_variance' ) ){
    y_lab_jj_lambda <- paste0( '|',y_lab_jj_lambda, '|' )
  }
  

  g_pdp <- f_plot_partial_overlay(df_pdp_rescale_jj, var_name_x = 'div_val_rescaled', var_name_y = 'mean_yhat', # 'TAC', #, line_color = 'black',
                                var_label_x =  'Scaled diversity', var_label_y = y_lab_jj_lambda  ,
                                overlay_column = 'Metric', color_column = 'Metric',  
                                var_label_z <- '', l_colors = colors_diversity_metrics, 
                                lims_x = lims_in_jj,  lims_y = y_lims_lambda_jj,
                                line_width = 1, legend_pos = 'right', 
                                add_hist_under = F,
                                add_error_band_even = F, add_error_band_uneven = T, var_name_se = 'yhat_') 
  
  ggsave(plot = g_pdp, path = output_path, 
         filename = paste0( 'g_partDep_boot_', 'targ-', target_name_jj,
                            '_absRR-', b_useAbs_RestRate, '_scaled-', percentile_alpha, '.png'),  
         width =  fig_width_wide*1.5, height = fig_height_wide)
}




