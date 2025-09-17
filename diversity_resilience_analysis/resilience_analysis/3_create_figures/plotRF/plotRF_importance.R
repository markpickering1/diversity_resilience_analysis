# ########################################################
# Title         : plot_rf_importance.R
# Description   : plot RF output to analyse performance and, in particular, variability of the 
#                 model to different diversity metrics 
#                 Non-bootstrapped version
# Aims          : analyse performance and diversity metrics
# Inputs	      : rf models with different diversity metrics in each 
# Outputs	      : figures for each diversity:  Importance
# Date          : 2025-02-19
# Authors       : Mark Pickering & Agata Elia
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
load( paste0(input_df, 'df_all', '.RData') )        # df_comb_i      head(df_comb_i)

# # initialise train/test df
df_comb.train_i <- subset(df_comb, train_sample == T) # head(df_comb.train_i)
df_comb.test_i  <- subset(df_comb, train_sample == F) # head(df_comb.test_i)

###################################################
######     LOOP OVER RF DIVERSITY MODELS      #####
###################################################

# initialise df for both importance and partial dependence
df_importance  <- data.frame() # data frame containing importance metrics for all variables for each diversity

# loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
for (i in 1:length(v_optional_predictors)){
  var_name_i <- v_optional_predictors[i] # extract individual diversity predictor
  print(var_name_i)
  # time count
  f_time_update(t_start_time)
  
  # loop over resilience metrics
  for (j in 1:length(v_target)){
    target_name_j <- v_target[j] ; print(target_name_j)
  
    ###################################################
    ####### LOAD RF AND DF                   ##########
    ###################################################
    
    # load basic rf model
    load( paste0(input_dir, 'rf_model_div-', var_name_i, '_targ-', target_name_j, '.RData' ) )
    
    # load bootstrapped rf models - for averaging or calculating variability (to do as a prior step)
    # load( paste0(input_dir, 'list_rf_model_pdp_results_boot_parallel_nIter-20_div-',
    #              var_name_i, '_seed-99_targ-', target_name_j, '.RData') )
    
    ###################################################
    ####### IMPORTANCE OF VARIABLES          ##########
    ###################################################

    print('importance')
    # extract importance list from model
    df_vari_imp <- as.data.frame(importance(rf.model))
    df_vari_imp <- df_vari_imp[order(df_vari_imp$`%IncMSE`, decreasing = T),] # reorder in terms of importance
    df_vari_imp <- df_vari_imp[1] # extract only INCMSE
    
    # optional: simplified built-in plot
    # save(varImpPlot(rf.model), file=paste0(output_path, 'rf_importancePlot' , var_name_i , '.RData' )    ) 
    
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
                                           value_in = 'Importance', value_in_full = paste0('% increase MSE for ' ,
                                           l_lables_metrics[[target_name_j]]) , 
                                           # s_title = 'he', #paste0('Importance in predicting ',
                                           # l_lables_metrics[[target_name_j]]),
                                           point_color = group.colors_Gio, lims_c = c(0,230) )
    # plot(g_importance_i)
    ggsave(plot = g_importance_i, filename = paste0('g_importance_ranking_', var_name_i ,  
                                                    '_targ-', target_name_j, '.png'), path = output_path, 
           width = fig_width_wide , height = fig_height_wide)
      
  } # end loop over res metrics
} # end loop over variables

df_importance$div_metric <- as.factor(df_importance$div_metric)
df_importance$res_metic <- as.factor(df_importance$res_metic)
save(df_importance, file = paste0( output_path, 'df_importance.RData') )


###########################################################################
############################      END      ################################
###########################################################################

