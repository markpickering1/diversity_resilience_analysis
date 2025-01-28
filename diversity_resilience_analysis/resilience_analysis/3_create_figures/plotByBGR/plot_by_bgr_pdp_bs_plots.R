# ########################################################
# Title         : plot_by_bgr_pdp_bs_plots.R
# Description   : plot results of plot_by_bgr_pdp_bs.R
#                 specifically all the 1d pdp from bs rf by bgr all in one single plot
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
# full_date <- '2024-03-14'     # parallel with new rfs
output_path <- paste0(root_data_figs, script_output_ext, '_pdp_bs_', full_date,  '/')
output_path_plots <- paste0(root_data_figs, script_output_ext, '_pdp_bs_', full_date,  '/plots/')

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
    df_pdp_bgr <- df_pdp_bgr[df_pdp_bgr$BiogeoRegions2016 %in% c(1, 7, 9), ]
    
    # select labels
    if(target_i=='kndvi_TAC'){
      if (var_name_i == 'sd_rh98'){
        xlims <- c(2, 7)
        ylims <- c(0.2, 0.55)
      } else if (var_name_i == 'shannon_entropy') {
        xlims <- c(4, 7)
        ylims <- c(0.2, 0.55)
      } else {
        xlims <- c(-0.8, -0.3)
        ylims <- c(0.2, 0.55)
      }
    } else if (target_i =='kndvi_lambda_variance') {
      if (var_name_i == 'sd_rh98'){
        xlims <- c(2, 7)
        ylims <- c(-2, -0.6)
      } else if (var_name_i == 'shannon_entropy') {
        xlims <- c(4, 7)
        ylims <- c(-2, -0.6)
      } else {
        xlims <- c(-0.8, -0.3)
        ylims <- c(-2, -0.6)
      }
    } else {
      if (var_name_i == 'sd_rh98'){
        xlims <- c(2, 7)
        ylims <- c(-2, -0.6)
      } else if (var_name_i == 'shannon_entropy') {
        xlims <- c(4, 7)
        ylims <- c(-2, -0.6)
      } else {
        xlims <- c(-0.8, -0.3)
        ylims <- c(-2, -0.6)
      }
    }
    
    if(t2m_mean_pdp){
      var_pdp <- 't2m_mean'
      xlims <- c(285.5, 292.1)
    } else {
      var_pdp <- var_name_i
    }
    
    # identify bgr classes for plots
    bgr <- c(1, 7, 9)
    colors <- c("blue", "cyan", "red")
    regions <- c("alpine", "temperate", "mediterranean")
    
    # list all the files in the folder of interest
    file_list <- list.files(path = output_path, full.names = TRUE)
    
    # identify names of pdp df of all seed and k (so for all bs models) for the specific combination of div and res variable
    pdp_df_list <- grep(paste0('df_pdp-', var_pdp, '_', var_name_i, '_targ-', target_i, '_bgr-.*', '_seed-'), file_list, value = TRUE)
    
    # rowbinds all df of bs pdp 
    merged_pdp_df <- NULL
    for (k in 1:length(pdp_df_list)){
      df <- pdp_df_list[[k]]
      load(df)
      # extract bgr, seed and k values from filename
      seed <- as.numeric(gsub('.*_seed-(\\d+)_.*', '\\1', df))
      k <- as.numeric(gsub('.*_k-(\\d+).*', '\\1', df))
      bgr_index <- as.numeric(gsub('.*_bgr-(\\d+)_.*', '\\1', df))
      index <- paste0(seed, k, bgr_index)
      # add index column based on bgr, seed and k
      pp_i$index <- index
      pp_i$bgr <- bgr_index
      pp_i$bgr <- factor(pp_i$bgr)
      # merge dataframes row-wise
      if (is.null( merged_pdp_df)) {
        merged_pdp_df <- pp_i
      } else {
        merged_pdp_df <- rbind(merged_pdp_df, pp_i)
      }
    }
    
    #### CREATE PLOTS WITH OVERLAYED PDP ####
    
    # plot each pdp (one for seed and k, so one for each bs model) overlayed, one color per bgr
    g_pdp <- ggplot(merged_pdp_df, aes(x = !!sym(var_pdp), y = !!sym(target_i), group = index, color = bgr)) + 
      geom_line(linewidth = 0.6) + 
      ylab(tlabs) +
      xlab(var_i_full) +
      scale_y_continuous(limits = ylims, breaks = seq(ylims[1], ylims[2], by = 0.2)) + #, expand = expansion(0, 0), labels = fixed_width_labels(12)) +
      scale_x_continuous(limits = xlims, breaks = seq(xlims[1], xlims[2], by = 0.2)) + #, expand = expansion(0, 0), labels = fixed_width_labels(10)) +
      scale_color_manual('Biogeographical \nregion', values = colors, labels = regions) +
      basic_graph_theme
      # theme_light() +
      # guides(color = guide_legend(override.aes = list(linewidth = 2)))
   
    #### ADD THE HISTOGRAMS ####
    
    # extract the legend and remove
    legend_grob <- cowplot::get_legend(g_pdp)
    g_pdp <- g_pdp + theme(legend.position = "none")
    
    # # create histograms
    # df_pdp_subset_d <- subset(df_pdp_bgr, get(var_name_i) >= xlims[1] & get(var_name_i) <= xlims[2])
    # 
    # h_dist_div <- ggplot(df_pdp_subset_d, aes(x = df_pdp_subset_d[[var_name_i]], colour=factor(BiogeoRegions2016), fill=factor(BiogeoRegions2016))) +
    #   geom_density(alpha=0.5, aes(fill = factor(BiogeoRegions2016))) +
    #   scale_x_continuous(limits = xlims, breaks = seq(xlims[1], xlims[2], by = 1)) + #, expand = expansion(0, 0), labels = fixed_width_labels(12)) +
    #   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.5), oob=squish, expand = expansion(0, 0)) +
    #   theme_light() +
    #   scale_color_manual(values = colors, labels = regions, guide = "none") +
    #   scale_fill_manual(values = colors, labels = regions, guide = "none") +
    #   labs( y= paste0( 'Density'), 
    #         x= paste0(var_i_full)) + 
    #   theme(axis.text.y = element_text(size = 8))
    # 
    # # create histograms
    # df_pdp_subset_t <- subset(df_pdp_bgr, t2m_mean >= xlims[1] & t2m_mean <= xlims[2])
    # 
    # h_dist_t2m <- ggplot(df_pdp_subset_t, aes(x = df_pdp_subset_t[['t2m_mean']], colour=factor(BiogeoRegions2016), fill=factor(BiogeoRegions2016))) +
    #   geom_density(alpha=0.5, aes(fill = factor(BiogeoRegions2016))) +
    #   scale_x_continuous(limits = xlims, breaks = seq(xlims[1], xlims[2], by = 1)) + #, expand = expansion(0, 0), labels = fixed_width_labels(10)) +
    #   scale_y_continuous(limits = c(0, 0.45), oob=squish, breaks=waiver(), expand = expansion(0, 0)) +
    #   theme_light() +
    #   scale_color_manual(values = colors, labels = regions, guide = "none") +
    #   scale_fill_manual(values = colors, labels = regions, guide = "none") +
    #   labs( y= paste0( 'Density'), 
    #         x= 't2m (mean)') + 
    #   theme(axis.text.y = element_text(size = 8))
    # 
    # # create histograms
    # df_pdp_subset_r <- subset(df_pdp_bgr, get(target_i) >= ylims[1] & get(target_i) <= ylims[2])
    # 
    # h_dist_r <- ggplot(df_pdp_subset_r, aes(x = df_pdp_subset_r[[target_i]], colour=factor(BiogeoRegions2016), fill=factor(BiogeoRegions2016))) +
    #   geom_density(alpha=0.5, aes(fill = factor(BiogeoRegions2016))) +
    #   scale_x_continuous(limits = ylims, breaks = seq(ylims[1], ylims[2], by = 0.2)) + #, expand = expansion(0, 0), labels = fixed_width_labels(10)) +
    #   scale_y_continuous(limits = c(0, 1.55), oob=squish, breaks=waiver(), expand = expansion(0, 0)) +
    #   theme_light() +
    #   scale_color_manual(values = colors, labels = regions, guide = "none") +
    #   scale_fill_manual(values = colors, labels = regions, guide = "none") +
    #   labs( y= paste0( 'Density'), 
    #         x= ylabs) + 
    #   theme(axis.text.y = element_text(size = 8)) +
    #   coord_flip()
    
    # # arrange plots
    # if(t2m_mean_pdp){
    #   g_pdp_hist <- grid.arrange(h_dist_r, g_pdp, legend_grob, h_dist_t2m, nrow = 2, ncol = 2, widths = c(1, 3), heights = c(3, 1), top = paste0("PDP of ", ylabs, " for five bs rf by bgr"))
    # }
    # else{
    #   g_pdp_hist <- grid.arrange(h_dist_r, g_pdp, legend_grob, h_dist_div, nrow = 2, ncol = 2, widths = c(1, 3), heights = c(3, 1), top = paste0("PDP of ", ylabs, " for five bs rf by bgr"))
    # }
    
    # arrange plots
    g_pdp_legend <- grid.arrange(g_pdp, NULL, legend_grob, nrow = 1, ncol = 3, widths = c(3, -0.15, 1.3), heights = c(3))
    
    # save
    ggsave(plot = g_pdp_legend, filename = paste0(output_path_plots, 'df_pdp_overlay-', var_pdp, '_', var_name_i, '_targ-', target_i, '_basic_with_legend.png'), width = 18, height = 12)
    ggsave(plot = g_pdp, filename = paste0(output_path_plots, 'df_pdp_overlay-', var_pdp, '_', var_name_i, '_targ-', target_i, '_basic.png'), width = fig_width_wide, height = fig_height_wide)
    ggsave(plot = legend_grob, filename = paste0(output_path_plots, 'df_pdp_overlay-', var_pdp, '_', var_name_i, '_targ-', target_i, '_basic_legend.png'), width = fig_width_wide, height = fig_height_wide)
    
    }
  
}