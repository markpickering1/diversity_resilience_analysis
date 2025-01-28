# ########################################################
# Title         : plot_rf_performance.R
# Description   : plot RF output to analyse performance and, in particular, variability of the 
#                 model to different diversity metrics 
# Aims          : analyse performance and diversity metrics
# Inputs	      : rf models with different diversity metrics in each 
# Outputs	      : figures for each diversity: 1) performance 2) Importance 3) dice 
#                 summary figures: 1) partial plot of diversity 2) ranked average Importance 3) Importance of diversity metrics
# Options	      : 
# Date          : 9/07/23
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

script_subtitle <- 'performance'
# load RF plotting functions
source('3_create_figures/functions/f_plotRF_initialise.R')

# # initialise plotRF script
# f_plotRF_initialise(script_subtitle)
# # initialise I/O
# f_plotRF_IO(script_subtitle)

library(car)    # check multicollinearity

###################################################
####### LOAD RF AND DF                   ##########
###################################################

# load dataframes of variables containing test/train split
# load df containing all test train data
load( paste0(input_dir, 'df_all', '.RData') )        # df_comb_i      head(df_comb_i)

# # initialise train/test df
df_comb.train_i <- subset(df_comb, train_sample == T) # head(df_comb.train_i)
df_comb.test_i  <- subset(df_comb, train_sample == F) # head(df_comb.test_i)


###################################################
######     LOOP OVER RF DIVERSITY MODELS      #####
###################################################

# initialise a df of all obs vs mod
df_comb_predictVobs_all <- data.frame()

# loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
for (i in 1:length(v_optional_predictors)){ # i <- 2
  var_name_i <- v_optional_predictors[i] # extract individual diversity predictor
  print(var_name_i)
  # time count
  f_time_update(t_start_time)

  # loop over resilience metrics
  for (j in 1:length(v_target)){  # j  <- 1
    target_name_j <- v_target[j] ; print(target_name_j)
  
    # ###################################################
    # ####### LOAD RF AND DF                   ##########
    # ###################################################
     
    # # load df containing all test train data - moved to common test-train split
    # load( paste0(input_dir, 'df_all_div-',var_name_i, '.RData') )        # df_comb_i      head(df_comb_i)
    # df_comb.train_i <- subset(df_comb_i, train_sample == T) ; df_comb.test_i  <- subset(df_comb_i, train_sample == F) # head(df_comb.test_i)
     
    # load rf model
    # load( paste0(input_dir, 'rf_model_div-',var_name_i, '.RData' ) ) # rf.model load the rf model for each div variable
    load( paste0(input_dir, 'list_rf_model_results_parallelDiv_div-',var_name_i, '_targ-', target_name_j, '.RData' ) ) # rf.model load the rf model for each div variable

  
    ###################################################
    ####### RUN PERFORMANCE METRICS          ##########
    ###################################################
    
    # if(b_run_performance){
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
      
      
      g_perf <- f_obs_vs_mod_density(df_comb_predictVobs, s_title = paste0(y_lab_i, ' for ', x_lab_i), 
                                     b_cor = F, b_mse = T, b_rmse = F,  b_mae = F, b_pbias = T, b_coefDet = T,
                                     lims_x = c_lims, lims_y = c_lims)
      g_perf
      # ggsave(plot = g_perf, filename = paste0(output_path, 'g_obsVSmod_plotLeg_div-', var_name_i, 
      #                                         '_targ-', target_name_j , '_runOnTrain-', b_run_on_train,  '_R_MSE_PB.png' ) , 
      #        width = fig_width_wide + 2.5, height = fig_height_wide+2)
      
      legend_grob <- cowplot::get_legend(g_perf)
      g_perf <- g_perf + theme(legend.position = "none")
      
      # create the plots without the legend
      ggsave(filename = paste0( 'g_obsVSmod_div-', var_name_i, '_targ-', target_name_j, 
                                '_absRR-', b_useAbs_RestRate , '_runOnTrain-', b_run_on_train,  '_R_MSE_PB.png' ) , 
             plot = g_perf, path = output_path, width = fig_width_wide+2, height = fig_height_wide+2)
      # ggsave(filename = paste0( 'g_obsVSmod_legend_div-', var_name_i, '_targ-', target_name_j, 
      #                           '_absRR-', b_useAbs_RestRate , '_runOnTrain-', b_run_on_train, '_R_MSE_PB.png' ) , 
      #        plot = legend_grob, path = output_path, width = fig_width_wide, height = fig_height_wide)
      # 
      
      # add this div x res combination to the dataframe
      df_comb_predictVobs$div_metric <- var_name_i
      df_comb_predictVobs$res_metric <- target_name_j
      df_comb_predictVobs_all <- rbind(df_comb_predictVobs_all,  df_comb_predictVobs)
      
      
      # check multicollinearity via VIF
      # test <-  subset( df_comb.test_i , v_predictors ) 
      df_comb.test_i_subset <- df_comb.test_i[ , c(v_predictors, target_name_j, var_name_i)] #v_optional_predictors, v_target)]
      # missing_values <- sapply(df_comb.test_i_subset, function(x) sum(is.na(x)))
      # print(missing_values)
      dim(df_comb.test_i_subset)
      df_comb.test_i_subset <- na.omit(df_comb.test_i_subset)
      f <- reformulate(c(v_predictors, var_name_i), response = target_name_j)
      lin_model <- lm(f , data = df_comb.test_i_subset )
      print(vif(lin_model))
      
      
    # }  # end model performance
  } # end loop over resilience metrics
} # end loop over variables

df_comb_predictVobs_all$div_metric <- as.factor(df_comb_predictVobs_all$div_metric)
df_comb_predictVobs_all$res_metric <- as.factor(df_comb_predictVobs_all$res_metric)
summary(df_comb_predictVobs_all)
save( df_comb_predictVobs_all, file = paste0(output_path, 'df_ObsvsMod', '_runOnTrain-', b_run_on_train, '.RData' ) )


################################
##### PLOT DIFF IN DENSITY #####
################################
# this takes the kndvi_lambda_variance and kndvi_lambda_xt and subtracts them to see where the differences lie in the density plot

# select the binning 
n_bins_denseDiff <- n_bins/2

get_midpoint <- function(cut_label) {
  mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
}



# loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
for (i in 1:length(v_optional_predictors)){ # i <- 3
  var_name_i <- v_optional_predictors[i] # extract individual diversity predictor
  print(var_name_i)
  
  df_comb_predictVobs_compare  <- df_comb_predictVobs_all %>% filter(div_metric == var_name_i)
  # df_comb_predictVobs_compare  <- df_comb_predictVobs_all %>% filter(res_metric %in% c('kndvi_lambda_variance', 'kndvi_lambda_xt') )
  df_comb_predictVobs_xt   <- df_comb_predictVobs_all %>% filter(res_metric %in% c( 'kndvi_lambda_xt') )
  df_comb_predictVobs_variance  <- df_comb_predictVobs_all %>% filter(res_metric %in% c( 'kndvi_lambda_variance') )
  
  c_lims  <- y_lims_pdp_perf[['kndvi_lambda_xt']]
  #### set lims and binning
  
  seq_bins <- seq( c_lims[1], c_lims[2] , length.out = n_bins_denseDiff)
  
  df_comb_predictVobs_xt <- df_comb_predictVobs_xt %>% mutate(Observed_bin = cut(Observed, breaks=seq_bins))
  df_comb_predictVobs_xt <- df_comb_predictVobs_xt %>% mutate(Modelled_bin = cut(Modelled, breaks=seq_bins))
  df_comb_predictVobs_xt <- na.omit(df_comb_predictVobs_xt)
  df_comb_predictVobs_variance <- df_comb_predictVobs_variance %>% mutate(Observed_bin = cut(Observed, breaks=seq_bins))
  df_comb_predictVobs_variance <- df_comb_predictVobs_variance %>% mutate(Modelled_bin = cut(Modelled, breaks=seq_bins))
  df_comb_predictVobs_variance <- na.omit(df_comb_predictVobs_variance)
  
  summary(df_comb_predictVobs_xt); summary(df_comb_predictVobs_variance)
  dim(df_comb_predictVobs_xt); dim(df_comb_predictVobs_variance)
  
  df_comb_predictVobs_sum_xt       <- df_comb_predictVobs_xt       %>% group_by(Observed_bin, Modelled_bin) %>% summarise(n_xt  = n())
  df_comb_predictVobs_sum_variance <- df_comb_predictVobs_variance %>% group_by(Observed_bin, Modelled_bin) %>% summarise(n_var = n())
  dim(df_comb_predictVobs_sum_xt) ; dim(df_comb_predictVobs_sum_variance)
  head(df_comb_predictVobs_sum_xt) ; head(df_comb_predictVobs_sum_variance)
  
  df_comb_predictVobs_sum_diff <- full_join( df_comb_predictVobs_sum_xt, df_comb_predictVobs_sum_variance)
  df_comb_predictVobs_sum_diff[is.na(df_comb_predictVobs_sum_diff)] <- 0
  df_comb_predictVobs_sum_diff <- df_comb_predictVobs_sum_diff %>% mutate( diff_xtSUBvar = n_xt - n_var)
  # df_comb_predictVobs_sum_diff     <- 
  
  # now need to set the midpoints as the factoras  
  df_comb_predictVobs_sum_diff$Observed <- sapply(df_comb_predictVobs_sum_diff$Observed_bin, get_midpoint)
  df_comb_predictVobs_sum_diff$Modelled <- sapply(df_comb_predictVobs_sum_diff$Modelled_bin, get_midpoint)
  
  # summary(df_comb_predictVobs_sum_diff)
  
  x_lab_i <- l_lables_metrics[[var_name_i]]
  
  g_input <-  ggplot() +
    # geom_tile(data = df_in, aes_string(x = 'x', y = 'y', fill = stat_in)) + # add the raster data
    geom_tile(data = df_comb_predictVobs_sum_diff, aes(x = Observed, y = Modelled, fill = diff_xtSUBvar )) + # add the raster data
    # scale_fill_distiller( direction = 1 ) +
    scale_fill_distiller( limits = c(-40,40), name = 'Relative \ndensity',  palette =  'Spectral', na.value = "white", oob=scales::squish) + # set the colour scheme and palette # direction = 1,
    geom_abline(slope=1, intercept=0, linetype = "dashed" ) +
    # xlim( lims_x ) + ylim( lims_y ) + 
    labs(title = paste0('AC1 - Variance, ', x_lab_i ) ) +
    basic_graph_theme
  g_input
  
  ggsave(plot = g_input, filename = paste0(output_path, 'g_obsVSmod_div-', var_name_i, '_diffDensitybins-', 
                                           n_bins_denseDiff , '_runOnTrain', b_run_on_train,  '.png' ) ,
         width = fig_width_wide + 0.5, height = fig_height_wide)
  
  
    # geom_sf(data = land_shapefile) + # , color = 'black', size =2) + # add the shapefile (place the base layer first)
    # labs( x='', y='', # x= 'latitude', y = 'longitude', 
          # fill = paste0( stat_in_full ),  title = paste0( title_full ) ) + #, '_', gsub('_',' ', stat_j) ) ) + # label axes and title
    # coord_sf() + # align the coordinates to those of shapefile?
    # coord_equal() + # different crs
    # discrete_fill_viridis_c() + # alternative colour system
    # scale_fill_distiller(limits= lims_in , palette =  'Spectral', na.value = "white", oob=scales::squish) + # set the colour scheme and palette # direction = 1,
    # scale_fill_distiller(limits=c(-1, 1), palette =  'YlGn', direction = 1,    
    #                      trans = "log",  breaks = scales::trans_breaks('log10', function(x) round(10 ^ x, digits=1), n=3)    ) + # , guide = guide_colorbar(frame.colour = "black")
    # theme_void() +
    # basic_fig_theme 
}
