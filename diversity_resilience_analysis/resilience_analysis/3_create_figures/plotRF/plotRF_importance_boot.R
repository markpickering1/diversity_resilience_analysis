# ########################################################
# Title         : plot_rf_importance.R
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

script_subtitle <- 'importance_boot' #_resampleNullHyp'
# load RF plotting functions
source('3_create_figures/functions/f_plotRF_initialise.R')
library(tibble) 
library(tidyr)
library(reshape2)
    

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

# initialise df for importance
df_importance  <- data.frame() # data frame containing importance metrics for all variables for each diversity and for all bootstraps

# loop over bootstrapped dfs and accumulate the importance values
for (i in 1:length(v_optional_predictors)){ # i <- 1
  var_name_i <- v_optional_predictors[i] # extract individual diversity predictor
  print(var_name_i)
  # time count
  f_time_update(t_start_time)
  
  # collect the different seeds, iters and res metric for a given diversity metric
  df_importance_divMetric  <- data.frame() # data frame containing importance metrics for all variables for each diversity
  
  # loop over resilience metrics
  for (j in 1:length(v_target)){  # j  <- 1
    target_name_j <- v_target[j] ; print(target_name_j)
  
    df_importance_divMetric_resMetric  <- data.frame() # data frame containing importance metrics for all variables for each diversity
    
    # loop over seeds
    for (k in 1:length(l_seeds)){  # k  <- 1
    n_seed <- l_seeds[k] ; print(n_seed)
      
    ###################################################
    ####### LOAD RF AND DF                   ##########
    ###################################################
    
    # create the input path of the results, using the different subdirectories  # _div-shannon_entropy_seed-100_targ-kndvi_lambda_variance.RData'
    input_file  <- paste0( root_data_proce, input_dir_boot_imp, #, '_nIter-', l_iter[i], 
                          'list_rf_model_pdp_results_boot_parallel_nIter-20', 
                           '_div-', var_name_i, '_seed-', n_seed, '_targ-', target_name_j, '.RData') #input_subdir[i],
    print(input_file)
    load(input_file) # results 
    
    df_importance_divMetric_resMetric_seed  <- data.frame() # data frame containing importance metrics for all variables for each diversity
    
    for ( l in 1: length(results$rf.models) ){ # l <- 1
    
      df_importance_ijkl <- importance(results$rf.models[[l]], scale = b_scale_importance)
      df_importance_ijkl <- as.data.frame(df_importance_ijkl) #, row.names = row.names(df_importance_ijkl))
      df_importance_ijkl <- df_importance_ijkl %>%
        rownames_to_column("Variable")  # "Variable" is the name of the new column
      df_importance_ijkl <- df_importance_ijkl[,1:2]
      df_importance_ijkl$Variable[df_importance_ijkl$Variable == var_name_i] <- "diversity_metric"
      names(df_importance_ijkl)[2] <- 'importance'
      
      # widen
      df_importance_ijkl <- df_importance_ijkl %>%
        pivot_wider(names_from = Variable, values_from =importance) %>% as.data.frame() 
      
      # save the seed
      df_importance_ijkl$div_metric <- var_name_i
      df_importance_ijkl$res_metric <- target_name_j
      df_importance_ijkl$n_seed  <- n_seed
      df_importance_ijkl$iter    <- l
      
      df_importance_divMetric_resMetric_seed <- rbind(df_importance_divMetric_resMetric_seed, df_importance_ijkl)
     } # end loop over iterations
    
    # collect the different seeds and iters for a given resilience and diversity metric
    df_importance_divMetric_resMetric <- rbind(df_importance_divMetric_resMetric, df_importance_divMetric_resMetric_seed)
      
    } # end loop over seeds
    
    # collect the different seeds, iters and res metric for a given diversity metric
    save(df_importance_divMetric_resMetric    , file=paste0(output_path, 'df_importance_boot_divMetric-',var_name_i, '_resMetric-', target_name_j, '.RData' )    ) #
    df_importance_divMetric <- rbind(df_importance_divMetric, df_importance_divMetric_resMetric )
    
  } # end loop over resilience metrics
  
  save(df_importance_divMetric    , file=paste0(output_path, 'df_importance_boot_divMetric-',var_name_i,  '.RData' )    ) #
  df_importance <- rbind(df_importance, df_importance_divMetric)
  
} # end loop over diversity metrics
   
# save the final dataset   
save(df_importance    , file=paste0(output_path, 'df_importance_boot_all.RData' )    ) #



################################################################################

###################################################
####### CREATE IMPORTANCE BOOT SUMMARY   ##########
###################################################
# load( file=paste0('/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_selectionsCommonTestTrain_2024-07-02_importance_boot/type1_noScale/'  , 'df_importance_boot_all.RData' )    ) #
# load( file=paste0('/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_selectionsCommonTestTrain_2024-07-02_importance_boot/type1_withScale/', 'df_importance_boot_all.RData' )    ) # 
load( file=paste0('/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_selectionsCTT_shanUpdate_kurtInvert_2024-09-17_importance_boot/', 'df_importance_boot_all.RData' )    ) #   # this is the version we used in paper

#### Take summary statistics 

df_importance <- melt(df_importance, 
                  id.vars = c("res_metric", "div_metric", "n_seed", "iter"), 
                  variable.name = "Variable", value.name="Value")

# summary(df_importance) ; dim(df_importance) ; str(df_importance)
df_importance$Variable <- as.character(df_importance$Variable)

# calc summary stats over the seeds and iterations
# use quantiles rather than CI eqn for non-parametric models
df_importance_stats <- df_importance %>%
  group_by(res_metric, div_metric, Variable) %>%
  summarise(
    mean = mean(Value, na.rm = TRUE), 
    sd = sd(Value, na.rm = TRUE),
    yhat_lower = quantile(Value, probs = 0.025), 
    yhat_upper = quantile(Value, probs = 0.975)
    
  ) # %>% as.data.frame
summary(df_importance_stats)

# rename diversity_metric in variable
df_importance_stats$Variable[df_importance_stats$Variable == "diversity_metric"] <- df_importance_stats$div_metric[df_importance_stats$Variable == "diversity_metric"]


# add the true name of each variable
df_importance_stats <- df_importance_stats %>%
  mutate(Variable_name = sapply(Variable, function(v) l_lables_metrics[[v]]))
df_importance_stats$Variable_name <- as.character(df_importance_stats$Variable_name)

# head(df_importance_stats)

# set factors
df_importance_stats$res_metric <- as.factor(df_importance_stats$res_metric)
df_importance_stats$div_metric <- as.factor(df_importance_stats$div_metric)
df_importance_stats$Variable   <- as.factor(df_importance_stats$Variable)

# put categories as a factor
# categorise each of the RF variables into the different climate types 
df_importance_stats <- f_add_category(df_importance_stats, 'Variable', div_metrics_list = l_diversity_metrics_list)
df_importance_stats$Category <- factor(df_importance_stats$Category, levels = c('Diversity', 'T2M',  'TP' , 'SSR',  'VPD' , 'Other' ) )


summary(df_importance_stats) ; head(df_importance_stats)

###################################################
####### PLOT IMPORTANCE BOOT SUMMARY     ##########
###################################################

# use CL and unssymetric - like bootstrap

# first loop over res and div metrics

for (i in 1:length(v_optional_predictors)){ # i <- 1
  var_name_i <- v_optional_predictors[i] # extract individual diversity predictor
  print(var_name_i)

  # loop over resilience metrics
  for (j in 1:length(v_target)){  # j  <- 1
    target_name_j <- v_target[j] ; print(target_name_j)

    df_vari_imp_cols <- df_importance_stats %>% filter(div_metric == var_name_i & res_metric == target_name_j )
    
    # set scales
    l_lims <- c(-10,160) # c(0,230)
    if(! b_scale_importance)  l_lims <- c(0,0.16)

    g_importance_i <- f_importance_ranking(df_vari_imp_cols, var_in = 'Variable_name', var_in_full = 'Predictor', 
                                       value_in = 'mean', value_in_full = paste0('% inc. MSE for ' , 
                                       l_lables_metrics[[target_name_j]]) , 
                                       # s_title = 'he', #paste0('Importance in predicting ', l_lables_metrics[[target_name_j]]),
                                       point_color = group.colors_Gio, lims_c = l_lims,  #c(0,0.2), # 
                                       b_unc = 'CI')  # 'CI') 
    
    g_importance_i <- g_importance_i + theme(legend.position = "none")
    
    

    # plot(g_importance_i)
    ggsave(plot = g_importance_i, filename = paste0('g_importance_ranking_boot_MSE_scaled-', b_scale_importance,'_div-', var_name_i ,  '_targ-', target_name_j, '.png'), 
           path = output_path, width = fig_width_wide , height = fig_height_wide)
  } # end loop over  v_target
} # end loop over div_metric


      

      

df_importance$div_metric <- as.factor(df_importance$div_metric)
df_importance$res_metric <- as.factor(df_importance$res_metric)
save(df_importance, file = paste0( output_path, 'df_importance.RData') )

###################################################
######     MEAN IMPORTANCE PLOT               #####
###################################################

##### plot average diversity (and average all variables score) ; Then print the different biodiv importances
if(b_run_importance){
  
  # save overall dataframe
  # save(df_importance, file=paste0(output_path, 'df_importance_all.RData' )    )
  
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


# ###################################################
# ####### test null hypothesis for p-val   ##########
# ###################################################
# 
# set.seed(100) ; n_permutations <- 100 # set seed and permutations for p-val testing of importance (not rf model production) for reproducibility
# p_values <- numeric(length('shannon_entropy'))
# 
# for (i in 1:length(v_optional_predictors)){ # i <- 3
#   var_name_i <- v_optional_predictors[i] # extract individual diversity predictor
#   # loop over resilience metrics
#   for (j in 1:length(v_target)){  # j  <- 1
#     target_name_j <- v_target[j] ; print(target_name_j)
#     
#     # loop over n_seed
#     for (k in 1:length(l_seeds)){  # k  <- 1
#       n_seed <- l_seeds[k] ; print(n_seed)
#       # loop over iter
#       for ( l in 1: length(results$rf.models) ){ # l <- 1
#         
#         # load rf model
#         input_file  <- paste0( root_data_proce, input_dir_boot_imp, #, '_nIter-', l_iter[i], 
#                                'list_rf_model_pdp_results_boot_parallel_nIter-20', 
#                                '_div-', var_name_i, '_seed-', n_seed, '_targ-', target_name_j, '.RData') #input_subdir[i],
#         
#         
#         # Perform permutations
#         for (p in 1:n_permutations) {
#           for (i in 1:length(rf.models)) {
#             # Permute feature values
#             permuted_data <- original_data
#             permuted_data[[feature_name]] <- sample(permuted_data[[feature_name]])
#             
#             # Update model with permuted data and compute importance
#             df_importance_ijkl_null <- importance(results$rf.models[[l]], scale = b_scale_importance)
#             permuted_importances[p, i] <- importance(update(rf.models[[i]], data = permuted_data), type = 1, scale = TRUE)[feature_name, "MeanDecreaseMSE"]
#           }
#         }
#         
#         
#         
#       } # end loop iter
#     } # end loop seed
#   } # end loop target
# } # end loop predictor