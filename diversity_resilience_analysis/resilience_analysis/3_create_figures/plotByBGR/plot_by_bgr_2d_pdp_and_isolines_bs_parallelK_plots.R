# ########################################################
# Title         : plot_by_bgr_2d_pdp_and_isolines_bs_parallelK_plots.R
# Description   : plot results of plot_by_bgr_2d_pdp_and_isolines_bs_parallelK.R
#                 specifically all the isolines from bs rf by bgr all in one single plot
#                 both overlayed and showing std error
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
full_date <- '2024-02-22'
output_path <- paste0(root_data_figs, script_output_ext, '_2d_pdp_and_isolines_bs_parallelK_', full_date,  '/')
output_path_plots <- paste0(root_data_figs, script_output_ext, '_2d_pdp_and_isolines_bs_parallelK_', full_date,  '/plots/')

# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for figures of dataframe cols : ', output_path ) ) }
print(paste0('output_path is : ', output_path ))

# create output if not present
if(! dir.exists(output_path_plots)) {dir.create(paste0(output_path_plots),recursive=T) ; 
  print( paste0( 'creating output dir for figures of dataframe cols : ', output_path_plots ) ) }
print(paste0('output_path_plots is : ', output_path_plots ))

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
##### IDENTIFY DIVERSITY FOR MAXIMUM LOCAL RESILIENCE   #####
#############################################################
# this loops over the variables and plots each of the diversity variables against their respective ICE
print('plotting')

# loop over 'diversity metrics' to load specific df and rf model
for (i in 1:length(v_optional_predictors)){
  for (j in 1:length(v_target)){
    
    target_i <- v_target[j] ; print(target_i)
    var_name_i <- v_optional_predictors[i] ; print(var_name_i)
    var_i_full <- l_vars[[var_name_i]][['label']] ; print(var_i_full)
    
    #### LOAD DATASET TO ADD HISTOGRAMS TO PLOTS ####
    
    # select only relevant predictors and the identifiers
    v_all_vars <- c(target_i, var_name_i, v_predictors)  ; print(v_all_vars)
    
    # load all data
    load(paste0(input_dir_rf, 'df_all_div-', var_name_i, '_targ-', target_i, '.RData') )
    
    # initialize train/test df from the all data
    df_comb.train_i <- subset(df_comb_i, train_sample == T) # head(df_comb.train_i)
    df_comb.test_i  <- subset(df_comb_i, train_sample == F) # head(df_comb.test_i)
    
    # create the pdps using the training, testing or all data
    if(s_train_test_all == 'train'){ df_pdp <-  df_comb.train_i[complete.cases(df_comb.train_i), ] ; print('using train data') }
    if(s_train_test_all == 'test') { df_pdp <-  df_comb.test_i[complete.cases(df_comb.test_i), ]   ; print('using test data')  }
    if(s_train_test_all == 'all')  { df_pdp <-  df_comb_i[complete.cases(df_comb_i), ]             ; print('using all data')   }
    
    # add the bgr to the df
    df_pdp_bgr <- left_join(df_pdp, df_var)
    df_pdp_bgr <- df_pdp_bgr[complete.cases(df_pdp_bgr), ]
    
    # select labels
    if(target_i=='kndvi_TAC'){
      tlabs <- 'kNDVI TAC'
    } else if (target_i =='kndvi_lambda_variance') {
      tlabs <- "kNDVI lambda var"
    } else {
      tlabs <- 'kNDVI lambda xt'
    }
    
    # create plot limits
    if (var_name_i == "shannon_entropy"){
      xlims <- c(285.5, 292.1)
      ylims <- c(4.5, 5.8)
    } else {
      xlims <- c(285.5, 292.1)
      ylims <- c(5, 15)
    }
   
    # identify unique KG classes to loop over
    bgr <- c(1, 7, 9)
    colors <- c("blue", "cyan", "red")
    regions <- c("alpine", "continental-atlantic", "mediterranean")
    
    # list all the files in the folder of interest
    file_list <- list.files(path = output_path, full.names = TRUE)
    
    # identify names of df with averages temperature and diversity
    median_df_list <- grep(paste0('df_all_div-', var_name_i, '_targ-', target_i, '_bgr-'), file_list, value = TRUE)
    
    # rowbind all df with averages temperature and diversity
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
      
    # select second independent var for 2d pdp
    pdp_2d_var_l <- 't2m_mean'

    # identify names of isoline df of all seed and k for the specific combination of div and res variable
    isolines_df_list <- grep(paste0('df_pdp_2d_isoline-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-.*_', stats,  '_seed-'), file_list, value = TRUE)
    
    # rowbinds all df of isolines 
    merged_isolines_df <- NULL
    for (k in 1:length(isolines_df_list)){
      df <- isolines_df_list[[k]]
      load(df)
      # extract seed and k values from filename
      seed <- as.numeric(gsub('.*_seed-(\\d+)_.*', '\\1', df))
      k <- as.numeric(gsub('.*_k-(\\d+).*', '\\1', df))
      bgr_index <- as.numeric(gsub('.*_bgr-(\\d+)_.*', '\\1', df))
      index <- paste0(seed, k, bgr_index)
      # add index column based on seed and k
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
    
    #### CREATE PLOTS ####
    
    # plot each isolines overlayed, one color per bgr
    g_isolines <- ggplot(merged_isolines_df, aes(x = t2m_mean, y = !!sym(var_name_i), group = index, color = bgr)) + 
      geom_line(linewidth = 0.05) + 
      ylab(var_i_full) +
      xlab('t2m (mean)') +
      scale_y_continuous(limits = ylims, breaks = seq(ylims[1], ylims[2], by = 0.2), expand = expansion(0, 0), labels = fixed_width_labels(12)) +
      scale_x_continuous(limits = xlims, breaks = seq(xlims[1], xlims[2], by = 1), expand = expansion(0, 0)) +
      scale_color_manual(values = colors, labels = regions) +
      ggtitle(paste0("Isolines of ", tlabs, " for each bs rf by bgr")) +
      theme_light() +
      guides(color = guide_legend(override.aes = list(linewidth = 2)))
    ggsave(plot = g_isolines, filename = paste0(output_path_plots, 'df_pdp_2d_isolines_overlay-', var_name_i, '-t2m_mean_targ-', target_i, '.png'), width = 7, height = 5)
    
    # create df of bootstrapped isolines (mean and standard deviation)
    for (r in 1:length(bgr)){
      
      # select bgr
      bgr_region <- bgr[r]  ; print(bgr_region)
      
      # identify names of isoline df of all seed and k for the specific combination of div and res variable and the bgr
      isolines_df_list_bgr <- grep(paste0('df_pdp_2d_isoline-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region, '.*_', stats,  '_seed-'), file_list, value = TRUE)
      
      # merge the isoline dataframes based on t2m_mean column
      load(isolines_df_list_bgr[[1]])
      # print(dim(isoline_df))
      isolines_merged <- isoline_df[, c('t2m_mean', var_name_i)]
      for (m in 2:length(isolines_df_list_bgr)){
        load(isolines_df_list_bgr[[m]])
        # print(dim(isoline_df))
        isolines_merged <- merge(isolines_merged, isoline_df[, c('t2m_mean', var_name_i)], by = "t2m_mean", all = FALSE, suffixes = c("", paste0("_", m)))
      }
      
      # calculate average, standard deviation and confidence interval band of div_var values for each t2m_mean value
      mean_div <- paste0('mean_', var_name_i); mean_div
      sd_div <- paste0('sd_', var_name_i); sd_div
      ci_low_div <- paste0('ci_low_', var_name_i); ci_low_div
      ci_up_div <- paste0('ci_up_', var_name_i); ci_up_div
      
      # identify columns to compute stats on
      isoline_df_bs <- isolines_merged
      div_columns <- names(isolines_merged)[grepl(paste0("^", var_name_i), names(isolines_merged))]
      
      # calculate mean
      isoline_df_bs[, mean_div] <- rowMeans(isolines_merged[div_columns], na.rm = TRUE)
      # calculate sd
      isoline_df_bs[, sd_div] <- apply(isolines_merged[div_columns], 1, function(x) {sd(x, na.rm = TRUE)})
      # calculate a 95% CI band
      isoline_df_bs[, ci_low_div] <- apply(isolines_merged[div_columns], 1, function(x) {quantile(x, 0.025, na.rm = TRUE)}) 
      isoline_df_bs[, ci_up_div] <-  apply(isolines_merged[div_columns], 1, function(x) {quantile(x, 0.975, na.rm = TRUE)})
      
      # select only relevant columns
      isoline_df_bs <- isoline_df_bs[, c('t2m_mean', mean_div, sd_div, ci_low_div, ci_up_div)]

      # save isoline bs
      save(isoline_df_bs, file=paste0(output_path, 'df_pdp_2d_isoline-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region, '_', stats, '_bs.RData'))
    }
    
    # list all the files in the folder of interest
    file_list <- list.files(path = output_path, full.names = TRUE)
    file_names <- basename(file_list)

    # identify names of isoline bs
    isolines_bs_df_list <- file_list[grep("bs", file_names)]
    # isolines_bs_df_list <- grep(paste0(output_path, 'df_pdp_2d_isoline-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-.*', '_bs'), file_list, value = TRUE)
    
    # rowbinds all df of isolines 
    merged_isolines_bs_df <- NULL
    for (k in 1:length(isolines_bs_df_list)){
      df <- isolines_bs_df_list[[k]]
      load(df)
      # extract bgr
      bgr_index <- as.numeric(gsub('.*_bgr-(\\d+)_.*', '\\1', df))
      isoline_df_bs$bgr <- bgr_index
      isoline_df_bs$bgr <- factor(isoline_df_bs$bgr)
      # merge dataframes row-wise
      if (is.null(merged_isolines_bs_df)) {
        merged_isolines_bs_df <- isoline_df_bs
      } else {
        merged_isolines_bs_df <- rbind( merged_isolines_bs_df, isoline_df_bs)
      }
    }
    
    # plot bs isoline with sd error
    g_isoline_bs_sd <- ggplot(merged_isolines_bs_df, aes(x = t2m_mean, y = !!sym(paste0('mean_', var_name_i)), group = bgr, color = bgr)) +
      geom_ribbon(aes(ymin = !!sym(paste0('mean_', var_name_i)) - !!sym(paste0('sd_', var_name_i)),
                      ymax = !!sym(paste0('mean_', var_name_i)) + !!sym(paste0('sd_', var_name_i)),
                      group = bgr,
                      linetype = NA),
                  alpha = 0.2) +
      geom_line(linewidth = 0.25) +
      labs(x = "t2m (mean)", y = paste0(var_i_full, ' +/- Std Dev')) +
      scale_y_continuous(limits = ylims, breaks = seq(ylims[1], ylims[2], by = 0.2), expand = expansion(0, 0), labels = fixed_width_labels(12)) +
      scale_x_continuous(limits = xlims, breaks = seq(xlims[1], xlims[2], by = 1), expand = expansion(0, 0)) +
      scale_color_manual(values = colors, labels = regions) +
      ggtitle(paste0("Isolines of ", tlabs, " with sd error by bgr")) +
      theme_light() 
    
    g_isoline_bs_sd <- g_isoline_bs_sd + geom_point(merged_median_df, mapping=aes(x = median_t2m_mean, y = !!sym(paste0('median_', var_name_i)), group =bgr, color = bgr))
    
    ggsave(plot = g_isoline_bs_sd, filename = paste0(output_path_plots, 'df_pdp_2d_isolines_bs_sd-', var_name_i, '-t2m_mean_targ-', target_i, '.png'), width = 7, height = 5)
    
    # plot bs isoline with confidence interval
    g_isoline_bs_ci <- ggplot(merged_isolines_bs_df, aes(x = t2m_mean, y = !!sym(paste0('mean_', var_name_i)), group = bgr, color = bgr)) +
      geom_ribbon(aes(ymin = !!sym(paste0('ci_low_', var_name_i)),
                      ymax = !!sym(paste0('ci_up_', var_name_i)),
                      group = bgr,
                      linetype = NA),
                  alpha = 0.2) +
      geom_line(linewidth = 0.25) +
      labs(x = "t2m (mean)", y = paste0(var_i_full, ' +/- Std Dev')) +
      scale_y_continuous(limits = ylims, breaks = seq(ylims[1], ylims[2], by = 0.2), expand = expansion(0, 0), labels = fixed_width_labels(12)) +
      scale_x_continuous(limits = xlims, breaks = seq(xlims[1], xlims[2], by = 1), expand = expansion(0, 0)) +
      scale_color_manual(values = colors, labels = regions) +
      ggtitle(paste0("Isolines of ", tlabs, " with CI by bgr")) +
      theme_light()

    g_isoline_bs_ci <- g_isoline_bs_ci + geom_point(merged_median_df, mapping=aes(x = median_t2m_mean, y = !!sym(paste0('median_', var_name_i)), group =bgr, color = bgr))

    ggsave(plot = g_isoline_bs_ci, filename = paste0(output_path_plots, 'df_pdp_2d_isolines_bs_ci-', var_name_i, '-t2m_mean_targ-', target_i, '.png'), width = 7, height = 5)

  }
  
}