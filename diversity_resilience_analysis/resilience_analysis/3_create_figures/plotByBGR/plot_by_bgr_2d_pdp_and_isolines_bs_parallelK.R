# ########################################################
# Title         : plot_by_bgr_2d_pdp_and_isolines_bs_parallelK.R
# Description   : run parallelly 2d pdp by bgr using all the bootstrapped rf model and for 
#                 each extract the resilience isoline corresponding to mean/median
#                 diversity and temperature (t2m mean) of bgr
#                 
# Aims          : extract resilience isolines 
# Inputs	      : bootstrapped rf models, bgr df, inout training/testing df 
# Outputs	      : df of 2d pdp and df of isolines
# Options	      : 
# Date          : 25/01/24
# Version       : 1 
# Authors       : Mark Pickering & Agata Elia
# Notes		      : 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())    

# set initialisation script names
main_initialisation_file   <- '0_main/initialise_R.R'
script_config_dir          <- '3_create_figures/input/'    ;   script_config_file <- 'input_plot_by_bgr.R'

# initialise code setup and repo building
source(main_initialisation_file)
# initialise figure common formatting for code base
source('0_main/initialise_figs.R')
# load common plotting functions
source('0_main/functions/plotting_functions.R')
# initialise user inputs (config) to script
source( paste0( script_config_dir, script_config_file) )
# load RF plotting functions
source('3_create_figures/functions/f_plotRF.R')

######     SET LIBRARIES                      #####
library(dplyr)        # use %>%
library(reshape)      # reshaping dataframes
require(ggplot2)      # for plotting
require(scales)       # for ggplot2 functions eg oob & trans
# library(ggpubr)       # for arranging ggplots together (ggarrange)
# library(ggExtra)      # more complex figures, marginal hists
# library(grid)         # for making multi-gird plots and tables
# library(gridExtra)    # for making multi-gird plots and tables
# library(lattice)      # for making multi-gird plots and tables
library(RColorBrewer) # colour palettes
library(sf)           # utilise shapefiles etc
library(cowplot)      # for ggdraw
library(ICEbox)  
library(terra)
library(raster)
library(randomForest)
library(pdp)
library(tidyr)
# parallelisation
library(doParallel) # run in parallel
library(foreach)    # run in parallel

###################################################
######       I/O                              #####
###################################################

# output location
# set/create output directory
# full_date <- '2024-01-31'   # for last meeting with Ale and Mirco
# full_date <- '2024-02-22'   # parallel
# full_date <- 'shannon_kndvi_TAC_2024-03-05'     # parallel with new rfs
# full_date <- 'shannon_lambda_xt_2024-03-05'     # parallel with new rfs
# full_date <- 'shannon_lambda_variance_2024-03-05'     # parallel with new rfs
# full_date <- 'sd_rh98_kndvi_TAC_2024-03-05'     # parallel with new rfs
# full_date <- 'sd_rh98_lambda_xt_2024-03-05'     # parallel with new rfs
# full_date <- 'sd_rh98_lambda_variance_2024-03-05'     # parallel with new rfs
# full_date <- 'shannon_lambda_variance_TACTgt003_2024-03-21'
# full_date <- 'mu_kurt_kndvi_TAC_2024-03-05'     # parallel with new rfs
# full_date <- 'mu_kurt_lambda_xt_2024-03-05'     # parallel with new rfs
# full_date <- 'mu_kurt_lambda_variance_2024-03-05'     # parallel with new rfs
# full_date <- 'shannon_lambda_xt_2024-08-30'     # parallel with new rfs with updated shannon
full_date <- 'shannon_lambda_variance_2024-08-30'     # parallel with new rfs with updated shannon

output_path <- paste0(root_data_figs, script_output_ext, '_2d_pdp_and_isolines_bs_parallelK_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for figures of dataframe cols : ', output_path ) ) }

# copy configuration input variables to output for storage
if( ! file.exists( paste0(output_path, script_config_file ) ) ) { print('copy config file') 
  file.copy(from= paste0( script_config_dir, script_config_file) , to= paste0(output_path, script_config_file ), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
} else{ print('could not copy config file') }

# load BGR file
load(paste0(input_dir_bgr, input_file_bgr))

# round digit of bgr file
df_var[1:2] <- df_var[1:2] %>% round( digits = 3)

# load all data (since they are now all in one df)
load(paste0(input_dir_rf, 'df_all.RData') )

# initialize train/test df from the all data
df_comb.train_i <- subset(df_comb, train_sample == T) # head(df_comb.train_i)
df_comb.test_i  <- subset(df_comb, train_sample == F) # head(df_comb.test_i)

# create the pdps using the training, testing or all data
if(s_train_test_all == 'train'){ df_pdp <-  df_comb.train_i[complete.cases(df_comb.train_i), ] ; print('using train data') }
if(s_train_test_all == 'test') { df_pdp <-  df_comb.test_i[complete.cases(df_comb.test_i), ]   ; print('using test data')  }
if(s_train_test_all == 'all')  { df_pdp <-  df_comb_i[complete.cases(df_comb_i), ]             ; print('using all data')   }

# add the bgr to the df
df_pdp_bgr <- left_join(df_pdp, df_var)
df_pdp_bgr <- df_pdp_bgr[complete.cases(df_pdp_bgr), ]

#############################################################
##### DEFINE THE FUNCTION TO BE RUN PARALLELY            ####
#############################################################

# test_function <- function(k,  var_name_i, pdp_2d_var_l, target_i, bgr_region, stats, df_pdp_bgr_r, df_pdp_bgr_r_mean, grid_values, n_seed){                   # this without loading the list of rf
# test_function <- function(k,  l_rf_boot_k,  var_name_i, pdp_2d_var_l, target_i, bgr_region, stats, df_pdp_bgr_r, df_pdp_bgr_r_mean, grid_values, n_seed){    # loading only the kth element of the list of rf
#   print(paste0('entering parallel t = ' , f_time_update(t_start_parallel)))
#   print(paste0('k : ' , k)) 
#   print(l_rf_boot_k$importance)
#   print(paste0('var_name_i : ', var_name_i)) 
#   print(paste0('target_i : ', target_i)) 
#   print(paste0('isoline for ', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '_', stats,  '_seed-', n_seed, '_k-', k,' saved'))
#   print(paste0('exiting parallel t = ', f_time_update(t_start_parallel)))
#   return(paste0('done'))
# }

# pdp_2d_parallelK <- function(k, l_rf_boot, var_name_i, pdp_2d_var_l, target_i, bgr_region, stats, df_pdp_bgr_r, df_pdp_bgr_r_mean, grid_values, n_seed){
# pdp_2d_parallelK <- function(k, var_name_i, pdp_2d_var_l, target_i, bgr_region, stats, df_pdp_bgr_r, df_pdp_bgr_r_mean, grid_values, n_seed){             # this without loading the list of rf
pdp_2d_parallelK <- function(k, rf.model_k, var_name_i, pdp_2d_var_l, target_i, bgr_region, stats, df_pdp_bgr_r, df_pdp_bgr_r_mean, grid_values, n_seed){   # loading only the kth element of the list of rf
  
  # print start time
  print(paste0('entering parallel t = ' , f_time_update(t_start_parallel)))
  
  # select model
  print(paste0('model: ', k))
  # rf.model_k <- l_rf_boot[[k]]
  # rm(l_rf_boot)
  
  # run 2d pdp if it does not already exists
  if (!file.exists(paste0(output_path, 'df_pdp_2d-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '_', stats,  '_seed-', n_seed, '_k-', k, '.RData'))) {
    # run 2d pdp at bgr scale
    print(paste0("running 2d pdp for model ", k))
    pdp_2d_i <- partial(rf.model_k, train = df_pdp_bgr_r, pred.var = c(pdp_2d_var_l, var_name_i), pred.grid = grid_values)
    # save 2d pdp df
    save(pdp_2d_i, file=paste0(output_path, 'df_pdp_2d-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '_', stats,  '_seed-', n_seed, '_k-', k, '.RData'))
  } else {
    print(paste0("2d pdp for model ", k,  "already created, loading it"))
    load(paste0(output_path, 'df_pdp_2d-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '_', stats,  '_seed-', n_seed, '_k-', k, '.RData'))
  }

  # plot 2d pdp
  g_pdp1 <- plotPartial(pdp_2d_i, contour=TRUE)
  save(g_pdp1, file=paste0(output_path, 'g_pdp_2d-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '_', stats,  '_seed-', n_seed, '_k-', k, '.RData'))

  # find the resilience value corresponding to mean/median t2m and div of bgr in the 2d pdp
  if (stats == 'mean') {
    pdp_2d_i_target_res <- pdp_2d_i[pdp_2d_i$t2m_mean == df_pdp_bgr_r_mean[1, 1] & pdp_2d_i[[var_name_i]] == df_pdp_bgr_r_mean[1, 3],]
    target_res <- pdp_2d_i_target_res[1, 3]; pdp_2d_i_target_res; target_res
    target_t2m <- df_pdp_bgr_r_mean[1, 1]
    target_div <- df_pdp_bgr_r_mean[1, 3]
  } else {
    pdp_2d_i_target_res <- pdp_2d_i[pdp_2d_i$t2m_mean == df_pdp_bgr_r_mean[1, 4] & round(pdp_2d_i[[var_name_i]], 7) == round(df_pdp_bgr_r_mean[1, 6], 7),]
    target_res <- pdp_2d_i_target_res[1, 3]; pdp_2d_i_target_res; target_res
    target_t2m <- df_pdp_bgr_r_mean[1, 4]
    target_div <- df_pdp_bgr_r_mean[1, 6]
  }

  # sort the dataframe based on diversity variable and t2m mean
  pdp_2d_i_sorted <- pdp_2d_i[order(pdp_2d_i[[var_name_i]], pdp_2d_i$t2m_mean), ]

  # calculate contour lines corresponding to the target resilience
  contour_lines <- contourLines(x = unique(pdp_2d_i_sorted$t2m_mean),
                                y = unique(pdp_2d_i_sorted[[var_name_i]]),
                                z = matrix(pdp_2d_i_sorted$yhat, nrow = length(unique(pdp_2d_i_sorted$t2m_mean))),
                                levels = target_res)

  # loop through the isolines to find the one containing target_t2m and target_div
  contour_line <- NULL

  # if more than one contour lines are identifies, select the one that contains the mean/median t2m and diversity at the same time
  for (isoline in contour_lines) {
    if (round(target_t2m, 3) %in% round(isoline$x, 3) & round(target_div, 7) %in% round(isoline$y, 7)) {
      contour_line <- isoline
      break  # stop the loop once the isoline is found
    }
  }

  # plot isoline
  t2m_values <- contour_line$x
  div_values <- contour_line$y

  # create isoline dataframe
  isoline_df <- data.frame(col1 = t2m_values, col2 = div_values, col3 = target_res)
  names(isoline_df) <- c('t2m_mean', var_name_i, target_i)
  # plot(x=isoline_df$t2m_mean, y=isoline_df[[var_name_i]], type='l')

  # save the isoline df
  isoline_name <- paste0(output_path, 'df_pdp_2d_isoline-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '_', stats,  '_seed-', n_seed, '_k-', k, '.RData')
  save(isoline_df, file=isoline_name)
  
  # print finish time
  print(paste0('exiting parallel t = ', f_time_update(t_start_parallel)))
  
  # return a txt
  print(paste0('isoline for ', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '_', stats,  '_seed-', n_seed, '_k-', k,' saved'))

  # remove not needed object to lighten up the server
  rm(rf.model_k)
  }


###################################################################
##### RUN 2D PDP BY BGR ON A FINER GRID AND EXTRACT ISOLINES  #####
###################################################################
print(paste0('calculating 2d pdp and isoline for ', stats, ' of t2m and diversity'))

# loop over diversity metric and resilience metric
for (i in 1:length(v_optional_predictors)){
  for (j in 1:length(v_target)){
    
    target_i <- v_target[j] ; print(target_i)
    var_name_i <- v_optional_predictors[i] ; print(var_name_i)
    var_i_full <- l_vars[[var_name_i]][['label']] ; print(var_i_full)
    
    # select only relevant predictors and the identifiers
    v_all_vars <- c(target_i, var_name_i, v_predictors)  ; print(v_all_vars)
    
    # # load all data
    # load(paste0(input_dir_rf, 'df_all_div-', var_name_i, '_targ-', target_i, '.RData') )
    # 
    # # initialize train/test df from the all data
    # df_comb.train_i <- subset(df_comb_i, train_sample == T) # head(df_comb.train_i)
    # df_comb.test_i  <- subset(df_comb_i, train_sample == F) # head(df_comb.test_i)
    # 
    # # create the pdps using the training, testing or all data
    # if(s_train_test_all == 'train'){ df_pdp <-  df_comb.train_i[complete.cases(df_comb.train_i), ] ; print('using train data') }
    # if(s_train_test_all == 'test') { df_pdp <-  df_comb.test_i[complete.cases(df_comb.test_i), ]   ; print('using test data')  }
    # if(s_train_test_all == 'all')  { df_pdp <-  df_comb_i[complete.cases(df_comb_i), ]             ; print('using all data')   }
    # 
    # # add the bgr to the df
    # df_pdp_bgr <- left_join(df_pdp, df_var)
    # df_pdp_bgr <- df_pdp_bgr[complete.cases(df_pdp_bgr), ]
    
    #### RUN 2D PDPS BY BIOGEOGRAPHICAL REGION ####
    
    # identify unique KG classes to loop over
    # bgr <- sort(unique(df_pdp_bgr[['BiogeoRegions2016']])) ; print(bgr) # run on all bgr
    # bgr <- c(1, 4, 7, 9, 11, 12) ; print(bgr) # run only on a subselection of bgr
    bgr <- c(1, 7, 9) ; print(bgr) # run only on a subselection of bgr mediterranean, alpine and continental+atlantic+pannoian+steppe

    # loop over bgr 
    for (r in 1:length(bgr)){
      
      # select bgr
      bgr_region <- bgr[r]  ; print(bgr_region)
      
      # select from the df only the data points belonging to the selected bgr
      df_pdp_bgr_r <- df_pdp_bgr[df_pdp_bgr[['BiogeoRegions2016']] == bgr_region, ]
      
      # identify average resilience, temperature and diversity value at bgr scale
      mean_resilience <- paste0('mean_', target_i) ; print(mean_resilience)
      mean_diversity <- paste0('mean_', var_name_i); print(mean_diversity)
      
      # identify median resilience, temperature and diversity value at bgr scale
      median_resilience <- paste0('median_', target_i) ; print(median_resilience)
      median_diversity <- paste0('median_', var_name_i); print(median_diversity)
      
      # create a df containing mean and median parameters of the biogeoregion
      df_pdp_bgr_r_mean <- df_pdp_bgr_r %>% summarize(mean_t2m_mean = mean(t2m_mean), !!mean_resilience := mean(!!sym(target_i)), !!mean_diversity := mean(!!sym(var_name_i)),
                                                      median_t2m_mean = median(t2m_mean), !!median_resilience := median(!!sym(target_i)), !!median_diversity := median(!!sym(var_name_i)))
      
      # add 1.5 degree celsius to the current mean or median temperature
      df_pdp_bgr_r_mean$mean_t2m_mean_p1 <- df_pdp_bgr_r_mean$mean_t2m_mean + 1.5 
      df_pdp_bgr_r_mean$median_t2m_mean_p1 <- df_pdp_bgr_r_mean$median_t2m_mean + 1.5 ; df_pdp_bgr_r_mean
      
      # define an additional increase of the temperature to expand the 2d pdp (if mediterranean 2.5 if other 2)
      if (bgr_region==9){
        t_upper_inc <- 1
      } else {
        t_upper_inc <- 0.5
      }
      
      # create a list of temperature and diversity values around the mean/median values (changed the increase in div from +10 to +3, and from -5 to -3 by looking at pdp and histograms)
      if (stats == 'mean') {
        t2m_list <- seq(df_pdp_bgr_r_mean[1, 1]-0.5, df_pdp_bgr_r_mean[1, 7]+t_upper_inc, by = 0.1); t2m_list; length(t2m_list)
        div_list <- seq(df_pdp_bgr_r_mean[1, 3]-3, df_pdp_bgr_r_mean[1, 3]+3, by = 0.1); div_list; length(div_list); length(t2m_list)*length(div_list)
      } else{
        t2m_list <- seq(df_pdp_bgr_r_mean[1, 4]-0.5, df_pdp_bgr_r_mean[1, 8]+t_upper_inc, by = 0.1); t2m_list; length(t2m_list)
        div_list <- seq(df_pdp_bgr_r_mean[1, 6]-3, df_pdp_bgr_r_mean[1, 6]+3, by = 0.1); div_list; length(div_list); length(t2m_list)*length(div_list)        
      }
      
      # create a grid of values for t2m_list and div_list
      grid_values <- expand.grid(feature1 = t2m_list, feature_2 = div_list); dim(grid_values)
      names(grid_values) <- c('t2m_mean', var_name_i)
      
      # save df with average and median resilience, temperature and diversity for the specific combination of diversity and resilience metrics for the current bgr
      save(df_pdp_bgr_r_mean, file=paste0(output_path, 'df_all_div-', var_name_i, '_targ-', target_i, '_bgr-', bgr_region, '_mean_t2m_div_res.RData'))
      
      # select only relevant predictors without the identifiers to run in model
      df_pdp_bgr_r <- df_pdp_bgr_r[, v_all_vars]
      
      # run through the extra vars with which to create the 2d pdp
      for (l in 1:length(pdp_2d_extra_vars)){
        
        # select second independent var for 2d pdp (for now is just t2m_mean)
        pdp_2d_var_l <- pdp_2d_extra_vars[l] 
        print(paste0('2d pdp plot between ', var_name_i, ' and ', pdp_2d_var_l) )
        
        #### RUN 2D PDPS FOR EACH SEED AND EACH K OF THE RF ####
        
        # loop over the seeds 
        for(n in 1:length(l_seed) ){
          
          #select seed
          n_seed <- l_seed[n] ; print(n_seed)
          
          # initialize the file containing rf models for the specific seed
          input_rf_file  <- paste0(input_dir_rf, 'list_of_rfs/list_rf_model_pdp_results_boot_parallel_nIter-20_div-', var_name_i, '_seed-', n_seed, '_targ-', target_i, '.RData')
          # input_rf_file  <- paste0(input_dir_rf, 'list_rf_model_pdp_results_boot_parallel_nIter-20_div-', var_name_i, '_seed-', n_seed, '_targ-', target_i, '.RData')
          
          # load the rf models for the specific seed
          load(input_rf_file) 
          l_rf_boot <- results$rf.models
          length(l_rf_boot)
          
          # timings
          t_start_parallel <- Sys.time() ; f_time_update(t_start_parallel)
          
          #### RUN 2D PDPS FOR THE FIRST 10 K ####
          
          # loop over the k different rf.models in the list for the seed and compute 2d pdp and isolines parallely for k
          # register parallel backend
          cl <- makeCluster(n_cores,  outfile=paste0(output_path, 'log_110_pdp_2d_targ-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '_', stats,  '_seed-', n_seed, '.txt'))
          # cl <- makeCluster(5,  outfile=paste0(output_path, 'log_test_pdp_2d_targ-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '_', stats,  '_seed-', n_seed, '.txt'))
          registerDoParallel(cl)
          
          # run foreach loop with parallel processing for each k
          # results <- foreach(k = 1:length(l_rf_boot), .packages = c('randomForest', "pdp", "caTools"), .combine = c ) %dopar% {
          results <- foreach(k = 1:10, .packages = c('randomForest', "pdp", "caTools"), .combine = c ) %dopar% {
          # results <- foreach(k = 1:2, .packages = c('randomForest', "pdp", "caTools"), .combine = c ) %dopar% {
            # pdp_2d_parallelK(k, l_rf_boot, var_name_i, pdp_2d_var_l, target_i, bgr_region, stats, df_pdp_bgr_r, df_pdp_bgr_r_mean, grid_values, n_seed)
            pdp_2d_parallelK(k, l_rf_boot[[k]], var_name_i, pdp_2d_var_l, target_i, bgr_region, stats, df_pdp_bgr_r, df_pdp_bgr_r_mean, grid_values, n_seed)
            # test_function(k, var_name_i, pdp_2d_var_l, target_i, bgr_region, stats, df_pdp_bgr_r, df_pdp_bgr_r_mean, grid_values, n_seed)
            # test_function(k, l_rf_boot[[k]], var_name_i, pdp_2d_var_l, target_i, bgr_region, stats, df_pdp_bgr_r, df_pdp_bgr_r_mean, grid_values, n_seed)
          }
          
          f_time_update(t_start_parallel)
          # stop cluster
          print('stop cluster)')
          stopCluster(cl)
          
          #### RUN 2D PDPS FOR THE LAST 10 K ####

          # timings
          t_start_parallel <- Sys.time() ; f_time_update(t_start_parallel)

          # loop over the k different rf.models in the list for the seed and compute 2d pdp and isolines parallely for k
          # register parallel backend
          cl <- makeCluster(n_cores,  outfile=paste0(output_path, 'log_1120_pdp_2d_targ-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '_', stats,  '_seed-', n_seed, '.txt'))
          # cl <- makeCluster(5,  outfile=paste0(output_path, 'log_pdp_2d_targ-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '_', stats,  '_seed-', n_seed, '.txt'))
          registerDoParallel(cl)

          # run foreach loop with parallel processing for each k
          # results <- foreach(k = 1:length(l_rf_boot), .packages = c('randomForest', "pdp", "caTools"), .combine = c ) %dopar% {
          results <- foreach(k = 11:20, .packages = c('randomForest', "pdp", "caTools"), .combine = c ) %dopar% {
            # pdp_2d_parallelK(k, l_rf_boot, var_name_i, pdp_2d_var_l, target_i, bgr_region, stats, df_pdp_bgr_r, df_pdp_bgr_r_mean, grid_values, n_seed)
            pdp_2d_parallelK(k, l_rf_boot[[k]], var_name_i, pdp_2d_var_l, target_i, bgr_region, stats, df_pdp_bgr_r, df_pdp_bgr_r_mean, grid_values, n_seed)
            # test_function(k, var_name_i, pdp_2d_var_l, target_i, bgr_region, stats, df_pdp_bgr_r, df_pdp_bgr_r_mean, grid_values, n_seed)
            # test_function(k, l_rf_boot[[k]], var_name_i, pdp_2d_var_l, target_i, bgr_region, stats, df_pdp_bgr_r, df_pdp_bgr_r_mean, grid_values, n_seed)
          }

          f_time_update(t_start_parallel)
          # stop cluster
          print('stop cluster)')
          stopCluster(cl)
          
        }
        
      }
      
    } 
    
  }
  
}