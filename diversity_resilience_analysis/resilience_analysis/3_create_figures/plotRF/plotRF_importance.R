# ########################################################
# Title         : plot_rf_importance.R
# Description   : plot RF output to analyse performance and, in particular, variability of the 
#                 model to different diversity metrics 
# Aims          : analyse performance and diversity metrics
# Inputs	      : rf models with different diversity metrics in each 
# Outputs	      : figures for each diversity: 1) performance 2) Importance 3) dice 
#                 summary figures: 1) partial plot of diversity 2) ranked average Importance 3) Importance of diversity metrics
# Options	      : 
# Date          : 2025-02-19
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

script_subtitle <- 'importance'
# load RF plotting functions
source('3_create_figures/functions/f_plotRF_initialise.R')

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

# initialise df for both importance and partial dependence
df_importance  <- data.frame() # data frame containing importance metrics for all variables for each diversity

# loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
for (i in 1:length(v_optional_predictors)){ # i <- 1
  var_name_i <- v_optional_predictors[i] # extract individual diversity predictor
  print(var_name_i)
  # time count
  f_time_update(t_start_time)
  
  # loop over resilience metrics
  for (j in 1:length(v_target)){  # j  <- 1
    target_name_j <- v_target[j] ; print(target_name_j)
  
    ###################################################
    ####### LOAD RF AND DF                   ##########
    ###################################################
    
    # load rf model
    # load( paste0(input_dir, 'rf_model_div-',var_name_i, '.RData' ) ) # rf.model load the rf model for each div variable
    load( paste0(input_dir, 'list_rf_model_results_parallelDiv_div-',var_name_i, '_targ-', target_name_j, '.RData' ) ) # rf.model load the rf model for each div variable
    
    # # load dataframes of variables containing test/train split
    # # load df containing all test train data
    # load( paste0(input_dir, 'df_all_div-',var_name_i, '.RData') )        # df_comb_i      head(df_comb_i)
    # # # initialise train/test df
    # df_comb.train_i <- subset(df_comb_i, train_sample == T) # head(df_comb.train_i)
    # df_comb.test_i  <- subset(df_comb_i, train_sample == F) # head(df_comb.test_i)
    
    ###################################################
    ####### IMPORTANCE OF VARIABLES          ##########
    ###################################################
  
    if(b_run_importance){
      print('importance')
      # extract importance list from model
      df_vari_imp <- as.data.frame(importance(rf.model))
      df_vari_imp <- df_vari_imp[order(df_vari_imp$`%IncMSE`, decreasing = T),] # reorder in terms of importance
      df_vari_imp <- df_vari_imp[1] # extract only INCMSE
      
      # save(varImpPlot(rf.model), file=paste0(output_path, 'rf_importancePlot' , var_name_i , '.RData' )    ) # the built-in plot
      
      # produce df for importance within this diversity type and names of each variable
      df_vari_imp_cols <- cbind(ID = rownames(df_vari_imp), df_vari_imp) 
      rownames(df_vari_imp_cols) <- NULL
      
      # replace diversity metric with diversity and add diversity name as variable in column
      rownames(df_vari_imp)[rownames(df_vari_imp) == var_name_i] <- 'diversity'
      df_vari_imp <- data.frame(t(df_vari_imp))
      rownames(df_vari_imp) <- NULL
      
      # for no diversity, add a diversity column with value zero. Drop the _diversity part of the names
      if (var_name_i == 'no_diversity'){ df_vari_imp$diversity <- 0 ; df_vari_imp$div_metric <- "no_diversity"
      } else{ df_vari_imp$div_metric <- stringr::str_replace(var_name_i, '_diversity', '') }
      
      # df_vari_imp$div_metric <- var_name_i # stringr::str_replace_all( df_vari_imp$div_metric, '_', ' ')
      df_vari_imp$res_metic <- target_name_j
      
      # bind to overall diversity importance df
      if (i == 1){df_importance <- df_vari_imp
      } else{df_importance <- rbind(df_importance, df_vari_imp) }
      
      # print importance figure of individual variables
      # extract meaning of statistical layers
      names(df_vari_imp_cols)[1:2] <- c('Variable', 'Importance')
      
      # categorise each of the RF variables into the different climate types 
      df_vari_imp_cols <- f_add_category(df_vari_imp_cols, 'Variable', div_metrics_list = l_diversity_metrics_list)
      df_vari_imp_cols$Category <- factor(df_vari_imp_cols$Category, levels = c('Diversity', 'T2M',  'TP' , 'SSR',  'VPD' , 'Other' ) )
      # add the true name of each variable
      df_vari_imp_cols <- df_vari_imp_cols %>%
        mutate(Variable_name = sapply(Variable, function(v) l_lables_metrics[[v]]))
      df_vari_imp_cols$Variable_name <- as.character(df_vari_imp_cols$Variable_name)
      
      # create plot ranking the importance of different variables in each RF
      g_importance_i <- f_importance_ranking(df_vari_imp_cols, var_in = 'Variable_name', var_in_full = 'Variable', 
                                             value_in = 'Importance', value_in_full = paste0('% increase MSE for ' , l_lables_metrics[[target_name_j]]) , 
                                             # s_title = 'he', #paste0('Importance in predicting ', l_lables_metrics[[target_name_j]]),
                                             point_color = group.colors_Gio, lims_c = c(0,230) )
      # plot(g_importance_i)
      ggsave(plot = g_importance_i, filename = paste0('g_importance_ranking_', var_name_i ,  '_targ-', target_name_j, '.png'), path = output_path, 
             width = fig_width_wide , height = fig_height_wide)
      
    } # end importance ranking
  } # end loop over res metrics
} # end loop over variables

df_importance$div_metric <- as.factor(df_importance$div_metric)
df_importance$res_metic <- as.factor(df_importance$res_metic)
save(df_importance, file = paste0( output_path, 'df_importance.RData') )

###################################################
######     MEAN IMPORTANCE PLOT               #####
###################################################

##### plot average diversity (and average all variables score) ; Then print the different biodiv importances
if(b_run_importance){
  
  # save overall dataframe
  save(df_importance, file=paste0(output_path, 'df_importance_all.RData' )    )
  
  # create a plot of importance of just different diversity variables
  g_importance_div <- f_importance_ranking(df_importance, 'div_metric', 'Diversity type', 'diversity', '% increase MSE', point_color = group.colors_Gio['Other'], lims_importance )
  plot(g_importance_div)
  ggsave(filename = 'g_diversity_var_ranking.png', plot = g_importance_div, path = output_path, width = fig_width_wide, height = fig_height_wide)
  
  ###### plot average of all variables Importance score #####
  df_importance_avg <- df_importance
  # remove no_diversity from the averaging
  df_importance_avg <- df_importance_avg[!grepl("no diversity", df_importance_avg$div_metric),]  # df_importance_avg[df_importance_avg$div_metric == "no diversity"]
  
  # average the columns
  df_importance_avg$div_metric <- NULL
  df_importance_avg <- colSums(df_importance_avg, na.rm = T)/dim(df_importance_avg)[1]
  df_importance_avg <- as.data.frame(df_importance_avg)
  df_importance_avg <- cbind(ID = rownames(df_importance_avg), df_importance_avg) 
  rownames(df_importance_avg) <- NULL
  names(df_importance_avg) <- c('Variable', 'Importance')
  
  # categorise each of the RF variables into the different climate types 
  df_importance_avg <- f_add_category(df_importance_avg, 'Variable') # add a category 
  # plot the mean importance of each variable across the different models
  g_importance_avg <- f_importance_ranking(df_importance_avg, 'Variable', 'Variable', 'Importance', '% increase MSE', point_color = group.colors_Gio , lims_importance )
  # plot(g_importance_avg)
  ggsave(filename = paste0( 'g_importance_ranking_mean.png'), plot = g_importance_avg, path = output_path, width = fig_width_wide, height = 8)
}




############################################################################################################################################
############################                   END                  ########################################################################
############################################################################################################################################

