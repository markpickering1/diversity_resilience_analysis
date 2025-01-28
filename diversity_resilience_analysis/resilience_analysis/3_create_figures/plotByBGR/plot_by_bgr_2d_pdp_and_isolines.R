# ########################################################
# Title         : plot_by_bgr_2d_pdp_and_isolines.R
# Description   : run 2d pdp by bgr and for each extract the resilience isoline corresponding to mean/median
#                 diversity and temperature (t2m mean) of bgr
#                 
# Aims          : extract resilience isolines 
# Inputs	      : rf models, bgr df, inout training/testing df 
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


###################################################
######       I/O                              #####
###################################################

# output location
# set/create output directory
# full_date <- '2024-01-31' # for last meeting with Ale and Mirco
output_path <- paste0(root_data_figs, script_output_ext, '_2d_pdp_and_isolines_', full_date,  '/')
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

#############################################################
##### RUN 2D PDP BY BGR ON A FINER GRID                 #####
#############################################################
# this loops over the variables and plots each of the diversity variables against their respective ICE
print('plotting')
print(paste0('calculating 2d pdp and isoline for ', stats, ' of t2m and diversity'))

# loop over 'diversity metrics' to load specific df and rf model
for (i in 1:length(v_optional_predictors)){
  for (j in 1:length(v_target)){
    
    target_i <- v_target[j] ; print(target_i)
    var_name_i <- v_optional_predictors[i] ; print(var_name_i)
    var_i_full <- l_vars[[var_name_i]][['label']] ; print(var_i_full)
    
    # select only relevant predictors and the identifiers
    v_all_vars <- c(target_i, var_name_i, v_predictors)  ; print(v_all_vars)
    
    # load rf model
    #load( paste0(input_dir_rf, 'rf_model_div-' ,var_name_i, '_targ-', target_i, '_seed-102.RData' ) ) # rf.model load the rf model for each div variable
    load( paste0(input_dir_rf, 'list_rf_model_results_parallelDiv_div-' ,var_name_i, '_targ-', target_i, '.RData' ) ) # after trimming gedi points
    
    # load all data
    load(paste0(input_dir_rf, 'df_all_div-', var_name_i, '_targ-', target_i, '.RData') )
    
    # # initialize train/test df from the all data
    df_comb.train_i <- subset(df_comb_i, train_sample == T) # head(df_comb.train_i)
    df_comb.test_i  <- subset(df_comb_i, train_sample == F) # head(df_comb.test_i)
    
    # create the pdps using the training testing or all data
    if(s_train_test_all == 'train'){ df_pdp <-  df_comb.train_i[complete.cases(df_comb.train_i), ] ; print('using train data') }
    if(s_train_test_all == 'test') { df_pdp <-  df_comb.test_i[complete.cases(df_comb.test_i), ]   ; print('using test data')  }
    if(s_train_test_all == 'all')  { df_pdp <-  df_comb_i[complete.cases(df_comb_i), ]             ; print('using all data')   }
    
    # add the bgr regions
    df_pdp_bgr <- left_join(df_pdp, df_var)
    df_pdp_bgr <- df_pdp_bgr[complete.cases(df_pdp_bgr), ]
    
    #### RUN 2D PDPS BY BIOGEOGRAPHICAL REGION ####
    
    # identify unique KG classes to loop over
    #bgr <- sort(unique(df_pdp_bgr[['BiogeoRegions2016']])) ; print(bgr) # run on all bgr
    bgr <- c(1, 4, 7, 9, 11, 12) ; print(bgr) # run only on a subselection of bgr
    
    # run through every BGR and run PDP separately by classes
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
      
      # define increase
      if (bgr_region==9){
        t_upper_inc <- 1
      } else {
        t_upper_inc <- 0.5
      }
      
      # create a list of temperature and diversity values between the average and the max
      if (stats == 'mean') {
        t2m_list <- seq(df_pdp_bgr_r_mean[1, 1]-0.5, df_pdp_bgr_r_mean[1, 7]+t_upper_inc, by = 0.1); t2m_list; length(t2m_list)
        div_list <- seq(df_pdp_bgr_r_mean[1, 3]-5, df_pdp_bgr_r_mean[1, 3]+10, by = 0.1); div_list; length(div_list); length(t2m_list)*length(div_list)
      } else{
        t2m_list <- seq(df_pdp_bgr_r_mean[1, 4]-0.5, df_pdp_bgr_r_mean[1, 8]+t_upper_inc, by = 0.1); t2m_list; length(t2m_list)
        div_list <- seq(df_pdp_bgr_r_mean[1, 6]-5, df_pdp_bgr_r_mean[1, 6]+10, by = 0.1); div_list; length(div_list); length(t2m_list)*length(div_list)        
      }
      
      # create a grid of values for t2m_list and div_list
      grid_values <- expand.grid(feature1 = t2m_list, feature_2 = div_list); dim(grid_values)
      names(grid_values) <- c('t2m_mean', var_name_i)
            
      # save df with average and median resilience, temperature and diversity
      save(df_pdp_bgr_r_mean, file=paste0(output_path, 'df_all_div-', var_name_i, '_targ-', target_i, '_bgr-', bgr_region, '_mean_t2m_div_res.RData'))
      
      # select only relevant predictors without the identifiers to run in model
      df_pdp_bgr_r <- df_pdp_bgr_r[, v_all_vars]
      
      # run through the extra vars and create 2d pdp
      for (l in 1:length(pdp_2d_extra_vars)){
        
        # select second independent var for 2d pdp
        pdp_2d_var_l <- pdp_2d_extra_vars[l] 
        print(paste0('2d pdp plot between ', var_name_i, ' and ', pdp_2d_var_l) )
        
        # run 2d pdp
        if (!file.exists(paste0(output_path, 'df_pdp_2d-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region, '_', stats, '.RData'))) {
          # run 2d pdp at bgr scale
          print("running 2d pdp")
          pdp_2d_i <- partial(rf.model, train = df_pdp_bgr_r, pred.var = c(pdp_2d_var_l, var_name_i), pred.grid = grid_values)
          # save 2d pdp df
          save(pdp_2d_i, file=paste0(output_path, 'df_pdp_2d-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '_', stats, '.RData'))
        } else {
          print("2d pdp already created, loading it")
          load(paste0(output_path, 'df_pdp_2d-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '_', stats, '.RData'))
        }
        
        # plot 2d pdp
        g_pdp1 <- plotPartial(pdp_2d_i, contour=TRUE); plot(g_pdp1)
        save(g_pdp1, file=paste0(output_path, 'g_pdp_2d-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region, '_', stats, '.RData'))
        
        # find the resilience value corresponding to mean/median t2m and div of bgr in the 2d pdp
        if (stats == 'mean') {
          pdp_2d_i_target_res <- pdp_2d_i[pdp_2d_i$t2m_mean == df_pdp_bgr_r_mean[1, 1] & pdp_2d_i[[var_name_i]] == df_pdp_bgr_r_mean[1, 3],]
          target_res <- pdp_2d_i_target_res[1, 3]; pdp_2d_i_target_res; target_res
          target_t2m <- df_pdp_bgr_r_mean[1, 1]
          target_div <- df_pdp_bgr_r_mean[1, 3]
        } else {
          pdp_2d_i_target_res <- pdp_2d_i[pdp_2d_i$t2m_mean == df_pdp_bgr_r_mean[1, 4] & pdp_2d_i[[var_name_i]] == df_pdp_bgr_r_mean[1, 6],]
          target_res <- pdp_2d_i_target_res[1, 3]; pdp_2d_i_target_res; target_res
          target_t2m <- df_pdp_bgr_r_mean[1, 4]
          target_div <- df_pdp_bgr_r_mean[1, 6]
        }
        
        # sort the dataframe based on diversity variable and t2m mean
        pdp_2d_i_sorted <- pdp_2d_i[order(pdp_2d_i[[var_name_i]], pdp_2d_i$t2m_mean), ]
        
        # plot contour lines
        #contour_plot <- contour(sort(div_list), sort(t2m_list), matrix(pdp_2d_i$yhat, nrow = length(div_list)), levels = res, col = "red", lwd = 2)
        
        # calculate contour lines corresponding to the target resilience
        contour_lines <- contourLines(x = unique(pdp_2d_i_sorted$t2m_mean),
                                      y = unique(pdp_2d_i_sorted[[var_name_i]]),
                                      z = matrix(pdp_2d_i_sorted$yhat, nrow = length(unique(pdp_2d_i_sorted$t2m_mean))),
                                      levels = target_res)
        
        # loop through the isolines to find the one containing target_t2m and target_div
        selected_isoline <- NULL
        
        for (isoline in contour_lines) {
          if (target_t2m %in% isoline$x & target_div %in% isoline$y) {
            contour_line <- isoline
            break  # stop the loop once the isoline is found
          }
        }
        
        # plot isoline 
        t2m_values <- contour_line$x
        div_values <- contour_line$y
        #plot(x=t2m_values, y=div_values, type='l')
        
        # create isoline dataframe
        isoline_df <- data.frame(col1 = t2m_values, col2 = div_values, col3 = target_res)
        names(isoline_df) <- c('t2m_mean', var_name_i, target_i)
        plot(x=isoline_df$t2m_mean, y=isoline_df[[var_name_i]], type='l')
        
        # save the isoline df
        save(isoline_df, file=paste0(output_path, 'df_pdp_2d_isoline-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region, '_', stats, '.RData'))

        }
    
    } 

  }
  
}