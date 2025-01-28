# ########################################################
# Title         : plot_by_bgr_2d_pdp_and_isolines_bs_parallelK_extract_quantile.R
# Description   : extract the quantile corresponding to the increase of diversity for each bgr
# Aims          :
# Inputs	      : 
# Outputs	      : 
# Options	      : 
# Date          : 
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
library(ggplot2)
library(gridExtra)
library(stringr)


###################################################
######       I/O                              #####
###################################################

# output location
# set/create output directory
# full_date <- '2024-02-22'
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
output_path_plots <- paste0(root_data_figs, script_output_ext, '_2d_pdp_and_isolines_bs_parallelK_', full_date,  '/plots/')
# output_path_plots <- paste0(root_data_figs, script_output_ext, '_2d_pdp_and_isolines_bs_parallelK_', full_date,  '/plots_inv/')   # for inverted kurtosis

# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for figures of dataframe cols : ', output_path ) ) }
print(paste0('output_path is : ', output_path ))

# create output if not present
if(! dir.exists(output_path_plots)) {dir.create(paste0(output_path_plots),recursive=T) ; 
  print( paste0( 'creating output dir for figures of dataframe cols : ', output_path_plots ) ) }
print(paste0('output_path_plots is : ', output_path_plots ))

# copy configuration input variables to output for storage
if( ! file.exists( paste0(output_path_plots, script_config_file ) ) ) { print('copy config file') 
  file.copy(from= paste0( script_config_dir, script_config_file) , to= paste0(output_path_plots, script_config_file ), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
} else{ print('could not copy config file') }

# load BGR file
load(paste0(input_dir_bgr, input_file_bgr))

# round digit of bgr file
df_var[1:2] <- df_var[1:2] %>% round( digits = 3)

#############################################################
##### IDENTIFY DIVERSITY FOR MAXIMUM LOCAL RESILIENCE   #####
#############################################################
# this loops over the variables and plots each of the diversity variables against their respective ICE
print('plotting')

# loop over 'diversity metrics' to load specific df and rf model
for (i in 1:length(v_optional_predictors)){
  for (j in 1:length(v_target)){
    
    target_i <- v_target[j] ; print(target_i)
    tlabs <- l_lables_metrics[[target_i]] ; print(tlabs)
    var_name_i <- v_optional_predictors[i] ; print(var_name_i)
    var_i_full <- l_lables_metrics[[var_name_i]] ; print(var_i_full)
    
    #### LOAD DATASET TO ADD HISTOGRAMS TO PLOTS ####
    
    # select only relevant predictors and the identifiers
    v_all_vars <- c(target_i, var_name_i, v_predictors)  ; print(v_all_vars)
    
    # load all data
    # load(paste0(input_dir_rf, 'df_all_div-', var_name_i, '_targ-', target_i, '.RData') )
    load(paste0(input_dir_rf, 'df_all.RData') )
  
    # select complete cases only
    df_pdp <-  df_comb[complete.cases(df_comb), ]
  
    # add the bgr to the df
    df_pdp_bgr <- left_join(df_pdp, df_var)
    df_pdp_bgr <- df_pdp_bgr[complete.cases(df_pdp_bgr), ]
    df_pdp_bgr <- df_pdp_bgr[df_pdp_bgr$BiogeoRegions2016 %in% c(1, 7, 9), ]
    
    # list all the files in the folder of interest
    file_list <- list.files(path = output_path, full.names = TRUE)
    
    # identify names of df with averages temperature and diversity
    median_df_list <- grep(paste0('df_all_div-', var_name_i, '_targ-', target_i, '_bgr-'), file_list, value = TRUE)
    
    # rowbind all df with mean and median of t2m_mean and div_var
    merged_median_df <- NULL
    for (k in 1:length(median_df_list)){
      df <- median_df_list[[k]]
      load(df)
      # extract bgr filename
      bgr_index <- as.numeric(gsub('.*_bgr-(\\d+)_.*', '\\1', df))
      # add bgr column 
      df_pdp_bgr_r_mean$bgr <- bgr_index
      df_pdp_bgr_r_mean$bgr <- factor(df_pdp_bgr_r_mean$bgr)
      # merge dataframes row-wise
      if (is.null(merged_median_df)) {
        merged_median_df <- df_pdp_bgr_r_mean
      } else {
        merged_median_df <- rbind( merged_median_df, df_pdp_bgr_r_mean)
      }
    }
    
    # change temperature in degrees
    merged_median_df$median_t2m_mean <- merged_median_df$median_t2m_mean-273.15
    
    # select second independent var for 2d pdp
    pdp_2d_var_l <- 't2m_mean'
    
    # identify names of isoline df of all seed and k (so for all bs models) for the specific combination of div and res variable
    isolines_df_list <- grep(paste0('df_pdp_2d_isoline-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-.*_', stats,  '_seed-'), file_list, value = TRUE)
    
    # rowbinds all df of bs isolines 
    merged_isolines_df <- NULL
    for (k in 1:length(isolines_df_list)){
      df <- isolines_df_list[[k]]
      load(df)
      # extract bgr, seed and k values from filename
      seed <- as.numeric(gsub('.*_seed-(\\d+)_.*', '\\1', df))
      k <- as.numeric(gsub('.*_k-(\\d+).*', '\\1', df))
      bgr_index <- as.numeric(gsub('.*_bgr-(\\d+)_.*', '\\1', df))
      index <- paste0(seed, k, bgr_index)
      # add index column based on bgr, seed and k
      isoline_df$index <- index
      isoline_df$bgr <- bgr_index
      isoline_df$bgr <- factor(isoline_df$bgr)
      # merge dataframes row-wise
      if (is.null( merged_isolines_df)) {
        merged_isolines_df <- isoline_df
      } else {
        merged_isolines_df <- rbind( merged_isolines_df, isoline_df)
      }
    }
    
    #### SUMMARISE THE ISOLINES DATAFRAME BY BGR ####
    
    # define names of average, standard deviation and confidence interval band of div_var columns (for each t2m_mean value)
    mean_div <- paste0('mean_', var_name_i); mean_div
    sd_div <- paste0('sd_', var_name_i); sd_div
    ci_low_div <- paste0('ci_low_', var_name_i); ci_low_div
    ci_up_div <- paste0('ci_up_', var_name_i); ci_up_div
    
    # calculate average, standard deviation and confidence interval band of div_var columns for each t2m_mean value, within same bgr
    merged_isolines_df_bs <- merged_isolines_df %>% group_by(bgr, t2m_mean) %>% 
      summarise(!!sym(mean_div) := mean(!!sym(var_name_i)), 
                !!sym(sd_div) := sd(!!sym(var_name_i)),
                !!sym(ci_low_div) := quantile(!!sym(var_name_i), probs = 0.025),
                !!sym(ci_up_div) := quantile(!!sym(var_name_i), probs = 0.975)
      )
    
    # keep only complete cases (not all the bs models in same bgr can have the same t2m_mean values)
    merged_isolines_df_bs <- merged_isolines_df_bs[complete.cases(merged_isolines_df_bs), ]
    
    # change temperature in degrees
    merged_isolines_df_bs$t2m_mean <- merged_isolines_df_bs$t2m_mean-273.15
    
    # define bgr
    bgr <- c(1, 7, 9) 
    
    # create empty dataframe
    quantiles <- NULL
    
    # loop over bgr 
    for (r in 1:length(bgr)){
      
      # select bgr
      bgr_region <- bgr[r]  ; print(bgr_region)
      
      # select from the df only the data points belonging to the selected bgr
      df_pdp_bgr_r <- df_pdp_bgr[df_pdp_bgr[['BiogeoRegions2016']] == bgr_region, ]
      
      # identify ecdf function of the diversity metrics
      ecdf_function <- ecdf(df_pdp_bgr_r[[var_name_i]])
      
      # select from the df only the data points belonging to the selected bgr
      merged_isolines_df_bs_bgr_r <- merged_isolines_df_bs[merged_isolines_df_bs[['bgr']] == bgr_region, ]
      
      # median temperature of the bgr
      median_t2m_mean <- merged_median_df$median_t2m_mean[merged_median_df[['bgr']] == bgr_region]
      
      quantiles_r <- NULL
      quantiles_r$bgr <- bgr_region
      quantiles_r$median_t2m_mean <- median_t2m_mean
      quantiles_r$median_div <- merged_median_df$median_shannon_entropy[merged_median_df[['bgr']] == bgr_region]
      quantiles_r$div_median_t2m_mean_p1 <- merged_isolines_df_bs_bgr_r$mean_shannon_entropy[merged_isolines_df_bs_bgr_r$t2m_mean==median_t2m_mean+1]
      quantiles_r$quantile_div_median_t2m_mean_p1 <-ecdf_function(merged_isolines_df_bs_bgr_r$mean_shannon_entropy[merged_isolines_df_bs_bgr_r$t2m_mean==median_t2m_mean+1])
      quantiles_r$div_median_t2m_mean_p1.5 <- merged_isolines_df_bs_bgr_r$mean_shannon_entropy[merged_isolines_df_bs_bgr_r$t2m_mean==median_t2m_mean+1.5]
      quantiles_r$quantile_div_median_t2m_mean_p1.5 <- ecdf_function(merged_isolines_df_bs_bgr_r$mean_shannon_entropy[merged_isolines_df_bs_bgr_r$t2m_mean==median_t2m_mean+1.5])
      quantiles_r$div_median_t2m_mean_p2 <- merged_isolines_df_bs_bgr_r$mean_shannon_entropy[merged_isolines_df_bs_bgr_r$t2m_mean==median_t2m_mean+2]
      quantiles_r$quantile_div_median_t2m_mean_p2 <- ecdf_function(merged_isolines_df_bs_bgr_r$mean_shannon_entropy[merged_isolines_df_bs_bgr_r$t2m_mean==median_t2m_mean+2])
      quantiles_r$div_median_t2m_mean_p2.5 <- merged_isolines_df_bs_bgr_r$mean_shannon_entropy[merged_isolines_df_bs_bgr_r$t2m_mean==median_t2m_mean+2.5]
      quantiles_r$quantile_div_median_t2m_mean_p2.5 <- ecdf_function(merged_isolines_df_bs_bgr_r$mean_shannon_entropy[merged_isolines_df_bs_bgr_r$t2m_mean==median_t2m_mean+2.5])
      
      if (is.null(quantiles)) {
        quantiles <- quantiles_r
      } else {
        quantiles <- rbind(quantiles, quantiles_r)
      }
    }
    save(quantiles, file= paste0(output_path_plots, 'df_pdp_2d_isoline_bs_quantiles-', var_name_i, '-t2m_mean_targ-', target_i, '.RData'))
  }
}