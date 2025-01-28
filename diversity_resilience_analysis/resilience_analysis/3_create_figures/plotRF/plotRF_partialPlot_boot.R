# ########################################################
# Title         : plotRF_partialPlot_boot.R
# Description   : plot bootstrapped RF models showing the mean and unc band
#                 plot RF output to analyse performance and, in particular, variability of the 
#                 model to different diversity metrics 
# Aims          : analyse performance and diversity metrics
# Inputs	      : df of already calculated pdps with different diversity metrics in each 
# Outputs	      : figures for each diversity: 1) performance 2) Importance 3) dice 
#                 summary figures: 1) partial plot of diversity 2) ranked average Importance 3) Importance of diversity metrics
# Options	      : 
# Date          : 12/09/23
# Version       : 2 - updated to include other res metrics (2/2/24)
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : Could extend with other metrics as well as partial plots of other variables
#                 set.seed(n_setseed_boot) ; print( paste0('bootstrap seed: ', n_setseed_boot) )
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

print('run')
if(b_run_partialplot_boot){

  # loop over 'diversity metrics' producing partial figure
  for (i in 1:length(v_optional_predictors)){ # i <- 1
    var_name_i <- v_optional_predictors[i] ; print(var_name_i) # extract individual predictor - to create pdps with
    
    # select diversity dataset and model - default is the diversity metric being plotted
    div_metric_var_name <- var_name_i                    
    # if not plotting the diversity metrics, then select which diversity metric to use in the model 
    # normally if we are plotting e.g. climate metrics, we will use the 'no_diversity' data and model
    if(b_run_partialplot_nonDiv){  div_metric_var_name <- var_i_nonDiv_partPlot ; print(paste0('rf model diversity metric: ', div_metric_var_name)) }

    ###################################################
    ####### LOAD PRE-CREATED PDP             ##########
    ###################################################
    
    if(!b_do_common_testTrainSplit){ 
      # load full data to add histos
      # load( paste0(input_dir_pdp_para, 
      #              'df_all_div-', div_metric_var_name, '.RData' ) ) # head(df_comb_i)
      # note it doesn't matter which res_metric you use as we only plot diveristy histo
      load( paste0(input_dir_pdp_para,
                   'df_all_div-', div_metric_var_name, '_targ-', v_target[1] , '.RData' ) ) # head(df_comb_i) ; summary(df_comb)
      # # initialise train/test df
      df_comb.train_i <- subset(df_comb_i, train_sample == T) # head(df_comb.train_i)
      df_comb.test_i  <- subset(df_comb_i, train_sample == F) # head(df_comb.test_i)
    } else{
      load( paste0(input_dir_pdp_para, 'df_all.RData' ) ) # head(df_comb)
      df_comb.train_i <- subset(df_comb, train_sample == T) # head(df_comb.train_i)
      df_comb.test_i  <- subset(df_comb, train_sample == F) # head(df_comb.test_i)
    }
    
    load( paste0(input_dir_pdp_para, 'df_pdp_div-', div_metric_var_name , '.RData' ) ) 
    # summary(df_pdp_all) ; # length(unique(df_pdp_all$div_val))
    
    # extract the mean and unc of the different metrics:
    # Calculate mean and quantiles for uncertainty bands
    df_pdp_all_metrics <- df_pdp_all %>% group_by( div_val, res_metric, div_metric) %>%  #group_by( !!sym(var_name_i), !!sym(target_name_j) ) %>%
      summarise(mean_yhat = mean(yhat), 
                # calculate the standard error
                se_yhat = sd(yhat), 
                # calculate a 95% CI band instead
                yhat_lower = quantile(yhat, probs = 0.025), yhat_upper = quantile(yhat, probs = 0.975))
    # summary(df_pdp_all_metrics)
    
    # should use the absolute value of restoration rate?
    y_lab_i_lambda <-  l_lables_metrics[['kndvi_lambda']]
    if( b_useAbs_RestRate  ){
      # df_pdp_all_metrics
      #df_pdp_all_metrics$mean_yhat  <- -1 * df_pdp_all_metrics$mean_yhat 
      df_pdp_all_metrics <- df_pdp_all_metrics %>%
        mutate(
          across(c(mean_yhat, yhat_lower, yhat_upper), ~ if_else(res_metric %in% c("kndvi_lambda_variance", "kndvi_lambda_xt"), . * -1, .)),
          temp       = yhat_upper,
          yhat_upper = if_else(res_metric %in% c("kndvi_lambda_variance", "kndvi_lambda_xt"), yhat_lower, yhat_upper),
          yhat_lower = if_else(res_metric %in% c("kndvi_lambda_variance", "kndvi_lambda_xt"), temp,       yhat_lower)
        )
      y_lab_i_lambda <- paste0( '|', l_lables_metrics[['kndvi_lambda']] , '|' )
      df_pdp_all_metrics$temp <- NULL
      print('invert rest rate')
    }
    
    # invert diversity metrics?
    lims_in_x <- l_lims_in[[var_name_i]]   
    x_lab_i <- l_lables_metrics[[var_name_i]]
    var_name_i_filename <- var_name_i
    if(b_invert_mu_kurt & var_name_i== 'mu_kurt'){
      df_pdp_all_metrics$div_val <- -1 * df_pdp_all_metrics$div_val 
      df_comb.train_i$mu_kurt    <- -1 * df_comb.train_i$mu_kurt 
      var_name_i_filename <- paste0(var_name_i, '_inv')
      # x_lab_i <- paste0(l_lables_metrics[[var_name_i]], ' (inverse)')
      lims_in_x <- - lims_in[c(2, 1)]
      print('invert mu_kurt')
    }
    
    # loop over resilience metrics
    for (j in 1:length(v_target)){  # j  <- 1
      target_name_j <- v_target[j] ; print(target_name_j)
    
      lims_y  <- y_lims_pdp[[target_name_j]]        # 
      y_lab_i <- l_lables_metrics[[target_name_j]]
      
      if( b_useAbs_RestRate  & ( target_name_j == 'kndvi_lambda_xt' | target_name_j == 'kndvi_lambda_variance' ) ){
        y_lab_i <- paste0( '|',y_lab_i, '|' )
      }
      
      ###################################################
      ######     CALC PDP MEAN/UNC                 #####
      ###################################################
      # create dataframe showing the mean partial dependence vals
      # different methods for the unc band - e.g. s.d. (or can we do something with uneven dist)
      
      # calculate the error on the mean from the standard error on the model values
      pdp_list_mean <- df_pdp_all_metrics %>%  filter( # div_metric == var_name_i, 
                                               res_metric == target_name_j ) 
      
      pdp_list_mean <- as.data.frame(pdp_list_mean) # summary(pdp_list_mean)
      # length(unique(pdp_list_mean$div_val))
      # extract the max and min y-limits
      print( max(pdp_list_mean$mean_yhat + pdp_list_mean$se_yhat) )
      print( min(pdp_list_mean$mean_yhat - pdp_list_mean$se_yhat) )
      # df_pdp_all %>% filter(mu_kurt == -1.466482000 )
      
  
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
      # g_pdp
      # ggsave(plot = g_pdp, path = output_path, filename = paste0( 'g_partDep_boot_var-', var_name_i_filename ,'_targ-', target_name_j ,, '_absRR-', b_useAbs_RestRate '.png'),  width = fig_width_wide  , height = fig_height_wide)
      ggsave(plot = g_pdp, path = output_path, filename = paste0( 'g_partDep_boot_var-', var_name_i_filename ,'_targ-', target_name_j, '_absRR-', b_useAbs_RestRate ,'_betterLims.png'),  width = fig_width_wide  , height = fig_height_wide)
      
      # now replot with hist below
      g_pdp <- f_plot_partial(pdp_list_mean, var_name_x = 'div_val', var_name_y = 'mean_yhat', 
                              lims_x = lims_in_x,  lims_y = lims_y,
                              var_label_x = x_lab_i, var_label_y = y_lab_i,
                              # add_hist_under = F, add_error_band_even = T, var_name_se = 'se_yhat') 
                              add_hist_under = T, 
                              add_error_band_even = F, add_error_band_uneven = T, var_name_se = 'yhat_')  # var_name_se = 'se_yhat'
      
      
      # create histogram with out of bounds added
      g_hist <- f_plot_partial_hist(df_comb.train_i, var_name_i, var_label_x = x_lab_i, 
                                    n_bins_x = n_bins/2, lims_x = lims_in_x )
      # summary(df_comb.train_i[[var_name_i]])
      # the difficulty (yet to fully get right) is to match the axes of the x-axes of histogram and partial. 
      # The only way I've managed to match them so far is by having fixed text_size at 8 for the histogram of frequency. It's all quite messy
      g_pdp_hist <- grid.arrange(g_pdp, g_hist, ncol = 1, heights = c(3, 1))
      
      # save(df_pdp_all    , file=paste0(input_dir_pdp_para, 'df_test1_pdp_div-', var_name_i_filename, '_targ-', target_name_j , '.RData' )    ) # 
      # ggsave(path = output_path, filename = paste0( 'g_partDep_boot_var-', var_name_i_filename ,'_targ-', target_name_j ,'_hist.png'), plot = g_pdp_hist, width =  fig_height_wide, height = fig_width_wide)
      ggsave(path = output_path, filename = paste0( 'g_partDep_boot_var-', var_name_i_filename ,'_targ-', target_name_j, '_absRR-', b_useAbs_RestRate ,'_hist_betterLims.png'), plot = g_pdp_hist, width =  fig_height_wide, height = fig_width_wide)
      
      ###################################################
      ######     PLOT ALL RF PDPS                   #####
      ###################################################
      
      if(b_plot_all_RF_PDPs ){
      df_pdp_all_test <- df_pdp_all %>%  filter( div_metric == var_name_i, 
                                                      res_metric == target_name_j ) 
      df_pdp_all_test <- df_pdp_all_test %>% mutate( index = paste0(n_seed, '_', para_iteration, '_', post_iteration))
      df_pdp_all_test$index  <- as.factor(df_pdp_all_test$index)
      # head(df_pdp_all_test)  ; summary(df_pdp_all_test)
      # df_pdp_all_test_101 <- df_pdp_all_test %>% filter(n_seed == 101)
      # div_vals_101 <- unique(df_pdp_all_test_101$div_val)
      # all(div_vals_98 ==div_vals_98)

      # if we want to set the colors of the seeds:
      colors_seeds <- setNames( c('red', 'orange', 'yellow', 'green', 'blue'),  c(98, 99, 100, 101, 102) )
      df_pdp_all_test$n_seed <- as.factor(df_pdp_all_test$n_seed)
      # lims_y <- c(0.28,0.38) ; lims_y <- c(0.25,0.38)
  
      if( b_useAbs_RestRate  ){
        # df_pdp_all_metrics
        df_pdp_all_test$yhat <- df_pdp_all_test$yhat * -1
      }
        
      g_pdp_all <- f_plot_partial_overlay(df_pdp_all_test, var_name_x = 'div_val', var_name_y = 'yhat', 
                                      overlay_column = 'index', #color_column = NA, #
                                      l_colors = colors_seeds, color_column = 'n_seed',      # if we want to color by seed
                                      lims_x = lims_in_x,  lims_y = lims_y,
                                      var_label_x = x_lab_i, var_label_y = y_lab_i,
                                      line_width = 0.1,
                                      add_hist_under = F)
      # g_pdp_all
      # ggsave(plot = g_pdp_all, path = output_path, filename = paste0( 'g_partDep_boot_var-', var_name_i_filename ,'_targ-', target_name_j ,'_allRFs_coloredSeeds.png'),  width = fig_width_wide  , height = fig_height_wide)
      ggsave(plot = g_pdp_all, path = output_path, filename = paste0( 'g_partDep_boot_var-', var_name_i_filename ,'_targ-', target_name_j, '_absRR-', b_useAbs_RestRate ,'_allRFs_coloredSeeds_betterLims.png'),  width = fig_width_wide  , height = fig_height_wide)
      
      } # plot over all RF PDPs
    } # end loop over resilience metric
    
    ###################################################
    ######     PLOT COMBINED LAMBDA               #####
    ###################################################
    
    
    pdp_plot_group <- df_pdp_all_metrics  %>% filter(  res_metric %in%  c('kndvi_lambda_xt', 'kndvi_lambda_variance') ) #  %>% filter(  !variable %in%  var_group_2 ) # exclude with !    
    # summary(pdp_plot_group) ;  length(unique( pdp_plot_group$div_val))
    # pdp_plot_group <- pdp_plot_group[complete.cases(pdp_plot_group), ] 
    pdp_plot_group <- data.frame(pdp_plot_group)
    
    # x_lab_i <- l_lables_metrics[[var_name_i]]
    # lims_in_x <- l_lims_in[[var_name_i]]   ;
    print(lims_in_x)
    print( max(pdp_plot_group$yhat_upper) )
    print( min(pdp_plot_group$yhat_lower) )
  
    # change names of res metrics
    res_metric_name <- 'res_metric'
    # pdp_plot_group <- pdp_plot_group %>%
    #   mutate(res_metric = recode(res_metric, !!!unlist( l_lables_metrics)) )
    # res_metric_name <- 'Resilience metric' # res_metric
    # names(pdp_plot_group)[2] <- res_metric_name

    # 
    g_pdp <- f_plot_partial_overlay(pdp_plot_group, var_name_x = 'div_val', var_name_y = 'mean_yhat', # 'TAC', #, line_color = 'black',
                            var_label_x =  x_lab_i, var_label_y = y_lab_i_lambda  ,
                            overlay_column = res_metric_name, color_column = res_metric_name, 
                            var_label_z <- 'Metric', l_colors = colors_resilience_metrics,   #color_column = 'n_seed',
                            # b_include_testANDtrain = 
                            lims_x = lims_in_x,  lims_y = y_lims_lambda,
                            line_width = 1, legend_pos = 'none', #NA , 
                            add_hist_under = F,
                            add_error_band_even = F, add_error_band_uneven = T, var_name_se = 'yhat_') #, add_error_band_even = F, add_error_band_uneven = T, var_name_se = 'yhat_')  # var_name_se = 'se_yhat'
    
    g_pdp
    # ggsave(plot = g_pdp, path = output_path, filename = paste0( 'g_partDep_boot_var-', var_name_i_filename , '_targ-lambda.png'),  width = fig_width_wide  , height = fig_height_wide)
    ggsave(plot = g_pdp, path = output_path, filename = paste0( 'g_partDep_boot_var-', var_name_i_filename , '_targ-lambda', '_absRR-', b_useAbs_RestRate, '_betterLims.png'),  width = fig_width_wide  , height = fig_height_wide)
    
    ######
    
    # Add histogram underneath
    g_pdp <- f_plot_partial_overlay(pdp_plot_group, var_name_x = 'div_val', var_name_y = 'mean_yhat', # 'TAC', #, line_color = 'black',
                                    var_label_x =  x_lab_i, var_label_y = l_lables_metrics[['kndvi_lambda']] ,
                                    overlay_column = 'res_metric', color_column = 'res_metric', 
                                    var_label_z <- 'Metric', l_colors = colors_resilience_metrics,   #color_column = 'n_seed',
                                    # b_include_testANDtrain = 
                                    lims_x = lims_in_x,  lims_y = y_lims_lambda,
                                    line_width = 1, legend_pos = 'none', #NA , 
                                    add_hist_under = T,
                                    add_error_band_even = F, add_error_band_uneven = T, var_name_se = 'yhat_') #, add_error_band_even = F, add_error_band_uneven = T, var_name_se = 'yhat_')  # var_name_se = 'se_yhat'
    
    
    g_hist <- f_plot_partial_hist(df_comb.train_i, var_name_i, var_label_x = x_lab_i, 
                                  s_oob = 'censor', # do not add out of bounds values
                                  n_bins_x = n_bins/2, lims_x = lims_in_x )
    # summary(df_comb.train_i[[var_name_i]])
    # the difficulty (yet to fully get right) is to match the axes of the x-axes of histogram and partial. 
    # The only way I've managed to match them so far is by having fixed text_size at 8 for the histogram of frequency. It's all quite messy
    g_pdp_hist <- grid.arrange(g_pdp, g_hist, ncol = 1, heights = c(3, 1))
    
    # ggsave(plot = g_pdp_hist, path = output_path, filename = paste0( 'g_partDep_boot_var-', var_name_i , '_targ-lambda_hist.png'),  width =  fig_height_wide, height = fig_width_wide)
    ggsave(plot = g_pdp_hist, path = output_path, filename = paste0( 'g_partDep_boot_var-', var_name_i , '_targ-lambda', '_absRR-', b_useAbs_RestRate, '_betterLims_hist.png'),  width =  fig_height_wide, height = fig_width_wide)
    
    # include all the different diversity metrics in the overall dataframe
    df_pdp <- rbind(df_pdp, df_pdp_all_metrics)
    
    
  } # end loop over pdp variables
} # end if b_run_partialplot

# save the overall df containing the different pdps with unc band
save(df_pdp , file=paste0(output_path, 'df_pdp_unc.RData' )    ) # the built-in plot

###################################################
######  RESCALE AND PLOT DIV METRICS ON SAME FIG #####
###################################################
load('/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_selectionsCTT_shanUpdate_kurtInvert_2024-09-24_partialPlot_boot_scaled/df_pdp_unc.RData')

# load df_all
load( paste0(input_dir_pdp_para, 'df_all.RData' ) ) # head(df_comb)
df_comb.train_i <- subset(df_comb, train_sample == T) # head(df_comb.train_i)
# invert mu_kurt values if required
if(b_invert_mu_kurt ){
  df_comb.train_i$mu_kurt    <- -1 * df_comb.train_i$mu_kurt 
  print('invert mu_kurt') }

# set up rescale function and percentile 
percentile_alpha <- 0.025 ; 
lims_in_jj <- c(0,1) ; y_lims_lambda_jj <-c(1.1,1.5)
# Compute the 5th and 95th percentiles for both columns
mu_kurt_percentiles <- quantile(df_comb.train_i$mu_kurt, probs = c(percentile_alpha, 1- percentile_alpha), na.rm = TRUE)
shannon_entropy_percentiles <- quantile(df_comb.train_i$shannon_entropy, probs = c(percentile_alpha, 1- percentile_alpha), na.rm = TRUE)
sd_rh98_percentiles <- quantile(df_comb.train_i$sd_rh98, probs = c(percentile_alpha, 1- percentile_alpha), na.rm = TRUE)
hist(df_comb.train_i$sd_rh98)

# Rescale function to map the values to the range [0.05, 0.95]
rescale <- function(x, percentile_alpha, p5, p95) {
  # Scale x to the range [0.05, 0.95] based on the 5th and 95th percentiles
  scaled_x <- percentile_alpha + ((x - p5) / (p95 - p5)) * ( (1-percentile_alpha) - percentile_alpha)
  # Cap values outside the range as NA (or 0.95 0.05)
  scaled_x[scaled_x < percentile_alpha] <- NA
  scaled_x[scaled_x > (1- percentile_alpha)] <- NA
  return(scaled_x)
}

df_comb.train_i$mu_kurt_rescaled <- rescale(df_comb.train_i$mu_kurt, percentile_alpha, mu_kurt_percentiles[1], mu_kurt_percentiles[2])
plot(df_comb.train_i$mu_kurt_rescaled, df_comb.train_i$mu_kurt) ; 
cor(df_comb.train_i$mu_kurt_rescaled, df_comb.train_i$mu_kurt, use = 'pairwise.complete.obs')
df_comb.train_i$shannon_entropy_rescaled <- rescale(df_comb.train_i$shannon_entropy, percentile_alpha, shannon_entropy_percentiles[1], shannon_entropy_percentiles[2])
plot(df_comb.train_i$shannon_entropy_rescaled, df_comb.train_i$shannon_entropy) ; 
cor(df_comb.train_i$shannon_entropy_rescaled, df_comb.train_i$shannon_entropy, use = 'pairwise.complete.obs')
hist(df_comb.train_i$shannon_entropy_rescaled) ; hist(df_comb.train_i$shannon_entropy)
df_comb.train_i$sd_rh98_rescaled <- rescale(df_comb.train_i$sd_rh98, percentile_alpha, sd_rh98_percentiles[1], sd_rh98_percentiles[2])


# now rescale the pdp
summary(df_pdp)
head(df_pdp)


# Apply the rescaling based on div_metric
df_pdp_rescale <- df_pdp %>%
  mutate(
    div_val_rescaled = case_when(
      div_metric == "mu_kurt" ~ rescale(div_val, percentile_alpha,  mu_kurt_percentiles[1], mu_kurt_percentiles[2]),
      div_metric == "shannon_entropy" ~ rescale(div_val, percentile_alpha, shannon_entropy_percentiles[1], shannon_entropy_percentiles[2]),
      div_metric == "sd_rh98" ~ rescale(div_val, percentile_alpha, sd_rh98_percentiles[1], sd_rh98_percentiles[2]),
      TRUE ~ NA # If div_metric neither, NA ## mean_yhat  # If div_metric is neither, leave unchanged
    )
  )
df_pdp_rescale <- as.data.frame(df_pdp_rescale)

# now recreate plot but with all metrics shown for each resilience metric
l_labels_metrics_vector <- unlist(l_lables_metrics)
df_pdp_rescale$div_metric <- as.character(df_pdp_rescale$div_metric)
df_pdp_rescale$Metric <- l_labels_metrics_vector[df_pdp_rescale$div_metric]

df_pdp_rescale$div_metric <- factor(df_pdp_rescale$div_metric, levels = c("sd_rh98", "mu_kurt", "shannon_entropy"))
# names(df_pdp_rescale)[names(df_pdp_rescale) == 'Diversity metric'] <- 'Metric'
summary(df_pdp_rescale) ; head(df_pdp_rescale)

for(jj in 1:length(v_target)){ # jj <- 1
  target_name_jj <- v_target[jj] ; print(target_name_jj)
  df_pdp_rescale_jj <- df_pdp_rescale %>% filter( res_metric == target_name_jj )
  
  y_lab_jj_lambda <-  l_lables_metrics[[target_name_jj]]
  if( b_useAbs_RestRate  & ( target_name_jj == 'kndvi_lambda_xt' | target_name_jj == 'kndvi_lambda_variance' ) ){
    y_lab_jj_lambda <- paste0( '|',y_lab_jj_lambda, '|' )
  }
  

  g_pdp <- f_plot_partial_overlay(df_pdp_rescale_jj, var_name_x = 'div_val_rescaled', var_name_y = 'mean_yhat', # 'TAC', #, line_color = 'black',
                                var_label_x =  'Scaled diversity', var_label_y = y_lab_jj_lambda  ,
                                overlay_column = 'Metric', color_column = 'Metric',  # overlay_column = res_metric_name, color_column = res_metric_name, 
                                var_label_z <- '', l_colors = colors_diversity_metrics,   #color_column = 'n_seed',
                                # b_include_testANDtrain = 
                                lims_x = lims_in_jj,  lims_y = y_lims_lambda_jj,
                                line_width = 1, legend_pos = 'right', # 'none', #NA , 
                                add_hist_under = F,
                                add_error_band_even = F, add_error_band_uneven = T, var_name_se = 'yhat_') #, add_error_band_even = F, add_error_band_uneven = T, var_name_se = 'yhat_')  # var_name_se = 'se_yhat'

  g_pdp  # + scale_x_continuous(breaks = seq(0, 1, by = 0.5)) # scale_x_continuous(limits = c(0,1.1)) 
  
  ggsave(plot = g_pdp, path = output_path, 
         filename = paste0( 'g_partDep_boot_', 'targ-', target_name_jj,  '_absRR-', b_useAbs_RestRate, '_scaled-', percentile_alpha, '.png'),  
         width =  fig_width_wide*1.5, height = fig_height_wide)
  
  
}


###################################################
######  PLOT COMBINED LAMBDA OVERLAY NON-BOOT #####
###################################################
# A bit hacky - take the last xt + var

# load the df of different non-bootstrapped pdps

load('/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_selectionsCommonTestTrain_2024-03-12_partialPlot/df_rf_model_partDep_diversity_div-kurtShannonSDRH98_data-train.RData')
summary(df_partDep_div)
df_partDep_div <- df_partDep_div  %>% filter(  res_metric %in%  c('kndvi_lambda_xt', 'kndvi_lambda_variance') ) #  %>% filter(  !variable %in%  var_group_2 ) # exclude with !    

s_non_boot_div_metric <- 'shannon_entropy' # 'mu_kurt'  # 'sd_rh98'

df_pdp_simple <- df_partDep_div %>% filter(div_metric == s_non_boot_div_metric)

g_pdp_boot_piu_normal <- g_pdp +
  geom_line(data = df_pdp_simple, aes(x = div_val, y = yhat), linewidth = 1, linetype = "dotted")
  # ggplot(df_pdp_simple, aes(x = div_val, y = yhat) ) +
  # geom_line(linewidth = line_width_base)   # Default line width can be adjusted here if needed
g_pdp_boot_piu_normal
ggsave(plot = g_pdp_boot_piu_normal, path = output_path, filename = paste0( 'g_partDep_bootPlusOriginalDotted_var-', s_non_boot_div_metric , '_targ-lambda', '_absRR-', b_useAbs_RestRate, '.png'),  width =  fig_height_wide, height = fig_width_wide)

###################################################
######     PLOT RES METRICS TOGETHER          #####
###################################################




############################################################################################################################################
############################                   END                  ########################################################################
############################################################################################################################################
# # g_pdp <- g_pdp + aes(color = metric_res)
# g_pdp <- g_pdp + aes(color = res_metric )    + labs(color = 'Metric') 
# g_pdp <- g_pdp + scale_color_manual(name = "lambda", values = colors_resilience_metrics)
# # g_pdp <- g_pdp + aes(linetype = test_train)  + labs(linetype = 'Dataset') 
# g_pdp <- g_pdp + theme(legend.position = "none") # "bottom")


###########################
### create the pdp dataframe from separate lists
###########################


# # loop over the seeds load
# df_pdp_all <- data.frame()
# for( i_seed in l_seeds ){
#   # load pdp dataframe created separately
#   # f_pdp <- paste0(input_dir_pdp_para, 
#   #        'df_pdp_div-', div_metric_var_name, '_varPDP-', var_name_i , '.RData' )
#   f_pdp <- paste0(input_dir_pdp_para, 
#                   'list_rf_model_pdp_results_boot_parallel_nIter-20_div-', 
#                   var_name_i , '_seed-',i_seed, '_targ-' , target_name_j, '.RData')
#   if(! file.exists(f_pdp) ) {next}
#   load( f_pdp ) # head(df_pdp_all)
#   ###################################################
#   ######     bind pdps together                 #####
#   ###################################################
#   for( nn in 1:length(results$pdp_i)){ # nn <- 1
#     df_seed_i <- results$pdp_i[[nn]]
#     df_seed_i$seed <- i_seed
#     df_pdp_all <- rbind(df_pdp_all, df_seed_i)
#   }
#   # length(unique(df_pdp_all$mu_kurt))
#   rm(results)
#   }







#########################################################################

## Manyally creating the partial plots
# g_pdp <- ggplot( pdp_list_mean , aes_string(var_name_i, 'mean_yhat') ) + #, color = variable)) +    # Draw ggplot2 plot with one axis
#   geom_line( size = 1) + # add line of mean
#   xlab("") +
#   ylab("TAC") +
#   scale_x_continuous(limits = lims_in  ) + 
#   theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
#   geom_ribbon( aes(ymin = (mean_yhat - se_yhat), ymax = (mean_yhat + se_yhat) ), alpha=0.2) + # add uncertainty band
#   # basic_graph_theme +
#   scale_y_continuous(limits = y_lims_pdp, labels = fixed_width_labels(12))
# # theme(plot.margin = unit(c(0.5, 0.5, 0.5, y_axis_space), "cm"))    # geom_point(data = pdp_list_df, aes(Petal.Width, yhat, color = iteration ), size = 3 )
# # g_pdp

# # Plot the histogram for the x-axis
# g_hist <- ggplot(df_comb.train_i, aes_string(x = var_name_i) ) +
#   geom_histogram(bins = n_bins_x) +
#   scale_x_continuous(limits = lims_in, oob=squish  ) + 
#   xlab(var_name_i) +
#   theme(axis.text.y = element_text(size = 8)) +
#   ylab("Frequency") +
#   # basic_graph_theme +
#   scale_y_continuous(labels = fixed_width_labels(12))
# # theme(plot.margin = unit(c(0.5, 0.5, 0.5, y_axis_space), "cm"))    # geom_point(data = pdp_list_df, aes(Petal.Width, yhat, color = iteration ), size = 3 )
# 


# # Plot instead the normalized histogram for Petal.Width
# g_dens <- ggplot(df_comb.train_i, aes(x = !!sym(var_name_i), y = ..density..)) +
#   geom_histogram(binwidth = 0.1, aes(y = ..count../max(..count..))) +
#   scale_x_continuous(limits = lims_in, oob=squish  ) + 
#   xlab(var_name_i) +
#   ylab("Density")

# # Plot a TAC histogram for the y-axis
# g_yhist <- ggplot(df_comb.train_i, aes_string(x = 'kndvi_TAC') ) +
#   geom_histogram(bins = n_bins/2) +
#   # scale_x_continuous(limits = y_lims_pdp , oob=squish  ) + 
#   # xlab(var_name_i) +
#   theme(axis.text.y = element_text(size = 8)) +
#   ylab("Frequency") +
#   # basic_graph_theme +
#   scale_y_continuous(labels = fixed_width_labels(12))
# # theme(plot.margin = unit(c(0.5, 0.5, 0.5, y_axis_space), "cm"))    # geom_point(data = pdp_list_df, aes(Petal.Width, yhat, color = iteration ), size = 3 )

# y-axis hist also
# g_pdp_hist <- grid.arrange(g_pdp, g_hist, g_yhist) #ncol = 1, heights = c(3, 1)
