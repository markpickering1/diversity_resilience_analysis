# ########################################################
# Title         : plot_by_bgr_2d_pdp_and_isolines_bs_parallelK_basic.R
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
# full_date <- '2024-02-22'
# full_date <- 'shannon_kndvi_TAC_2024-03-05'     # parallel with new rfs
# full_date <- 'shannon_lambda_xt_2024-03-05'     # parallel with new rfs
# full_date <- 'shannon_lambda_variance_2024-03-05'     # parallel with new rfs
# full_date <- 'sd_rh98_kndvi_TAC_2024-03-05'     # parallel with new rfs
# full_date <- 'sd_rh98_lambda_xt_2024-03-05'     # parallel with new rfs
# full_date <- 'sd_rh98_lambda_variance_2024-03-05'     # parallel with new rfs
# full_date <- 'shannon_lambda_variance_TACTgt003_2024-03-21'
# full_date <- 'mu_kurt_kndvi_TAC_2024-03-05'     # parallel with new rfs
full_date <- 'mu_kurt_lambda_xt_2024-03-05'     # parallel with new rfs
# full_date <- 'mu_kurt_lambda_variance_2024-03-05'     # parallel with new rfs
# full_date <- 'shannon_lambda_xt_2024-08-30'     # parallel with new rfs with updated shannon
# full_date <- 'shannon_lambda_variance_2024-08-30'     # parallel with new rfs with updated shannon

output_path <- paste0(root_data_figs, script_output_ext, '_2d_pdp_and_isolines_bs_parallelK_', full_date,  '/')
# output_path_plots <- paste0(root_data_figs, script_output_ext, '_2d_pdp_and_isolines_bs_parallelK_', full_date,  '/plots_new_col/')
output_path_plots <- paste0(root_data_figs, script_output_ext, '_2d_pdp_and_isolines_bs_parallelK_', full_date,  '/plots_inv_new_col/')   # for inverted kurtosis

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
    
    # create plot limits
    if (var_name_i == "shannon_entropy"){
      xlims <- c(12, 20)
      # ylims <- c(4.2, 5.8)
      ylims <- c(3, 4.2)            # with the new shannon 
    } else if (var_name_i == "mu_kurt"){
      xlims <- c(12, 20)
      # ylims <- c(-0.8, -0.3)
      ylims <- c(0.3, 0.8) # for inverted kurtosis
    } else {
      xlims <- c(12, 20)
      ylims <- c(4, 15)
    }
    
    # identify bgr classes for plots
    bgr <- c(1, 7, 9)
    # colors <- c("blue", "cyan", "red")
    colors <- c("#D55E00", "#5BB99F","#F0E442")
    regions <- c("alpine", "temperate", "mediterranean")
    
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
    
    # joining the two dataframes on the 'bgr' column
    merged_df <- merge(merged_isolines_df, merged_median_df[, c('median_t2m_mean', paste0('median_', var_name_i), 'bgr')], by = "bgr")

    # subsetting based on conditions
    # merged_isolines_df <- merged_df %>% filter(t2m_mean >= (median_t2m_mean - 0.3) & !!sym(var_name_i) >= (!!sym(paste0('median_', var_name_i)) - 0.3 ))  # for shannon
    merged_isolines_df <- merged_df %>% filter(t2m_mean >= (median_t2m_mean - 0.3) & !!sym(var_name_i) <= (!!sym(paste0('median_', var_name_i)) + 0.3 ))    # for kustoris
    
    #### CREATE PLOTS WITH OVERLAYED ISOLINES ####
    
    # plot each isoline (one for seed and k, so one for each bs model) overlayed, one color per bgr
    g_isolines <- ggplot(merged_isolines_df, aes(x = t2m_mean-273.15, y = !!sym(var_name_i), group = index, color = bgr)) + 
      geom_line(linewidth = 0.05) + 
      labs(x = var_i_full, y = '2m temp mean [°C]', title = paste0("Isolines of ", tlabs, " for bootstrapped RFs")) +
      scale_y_continuous(limits = ylims, breaks = seq(ylims[1], ylims[2], by = 0.2)) + #, expand = expansion(0, 0), labels = fixed_width_labels(12)) +
      scale_x_continuous(limits = xlims, breaks = seq(xlims[1], xlims[2], by = 1)) + #, expand = expansion(0, 0), labels = fixed_width_labels(10)) +
      scale_color_manual('Biogeographical\nregion', values = colors, labels = regions) +
      basic_graph_theme +
      guides(color = guide_legend(override.aes = list(linewidth = 2)))
      
    # extract the legend and remove
    legend_grob <- cowplot::get_legend(g_isolines)
    g_isolines <- g_isolines + theme(legend.position = "none")
    
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
    
    #### CREATE PLOTS WITH ERROR BAND/CI ISOLINES ####

    # # plot bs isolines with confidence interval (ci)
    # g_isoline_bs_ci <- ggplot(merged_isolines_df_bs, aes(x = t2m_mean-273.15, y = !!sym(paste0('mean_', var_name_i)), group = bgr, color = bgr)) +
    #   geom_ribbon(aes(ymin = !!sym(paste0('ci_low_', var_name_i)),
    #                   ymax = !!sym(paste0('ci_up_', var_name_i)),
    #                   group = bgr,
    #                   fill = bgr,
    #                   linetype = NA),
    #               alpha = 0.2) +
    #   # geom_line(linewidth = 0.25) +
    #   geom_line(linewidth = 0.6) +
    #   labs(x = "\n2m temp mean [°C]", y = paste0(var_i_full, '\n')) + #, title = paste0("Isolines of ", tlabs, " with Confidence Interval")) +
    #   scale_y_continuous(limits = ylims, breaks = seq(ylims[1], ylims[2], by = 0.2)) + #by = 0.5 , expand = expansion(0, 0), labels = fixed_width_labels(12)) +
    #   scale_x_continuous(limits = xlims, breaks = seq(xlims[1], xlims[2], by = 2)) + #, expand = expansion(0, 0)) +
    #   scale_color_manual('Biogeographical \nregion', values = colors, labels = regions) +
    #   scale_fill_manual('Biogeographical \nregion', values = colors, labels = regions, guide = 'none') +
    #   basic_graph_theme
    # 
    # g_isoline_bs_ci <- g_isoline_bs_ci + geom_point(merged_median_df, mapping=aes(x = median_t2m_mean-273.15, y = !!sym(paste0('median_', var_name_i)), group =bgr, color = bgr), size=5)
    # 
    # plot bs isolines with confidence interval (ci) (multiply -1 if Kurtosis)
    g_isoline_bs_ci <- ggplot(merged_isolines_df_bs, aes(x = t2m_mean-273.15, y = -1 * !!sym(paste0('mean_', var_name_i)), group = bgr, color = bgr)) +
      geom_ribbon(aes(ymin = -1 * !!sym(paste0('ci_low_', var_name_i)),
                      ymax = -1 * !!sym(paste0('ci_up_', var_name_i)),
                      group = bgr,
                      fill = bgr,
                      linetype = NA),
                  alpha = 0.2) +
      # geom_line(linewidth = 0.25) +
      geom_line(linewidth = 0.6) +
      labs(x = "\n2m temp mean [°C]", y = paste0(var_i_full, '\n')) + #, title = paste0("Isolines of ", tlabs, " with Confidence Interval")) +
      scale_y_continuous(limits = ylims, breaks = seq(ylims[1], ylims[2], by = 0.2)) + #by = 0.5 , expand = expansion(0, 0), labels = fixed_width_labels(12)) +
      scale_x_continuous(limits = xlims, breaks = seq(xlims[1], xlims[2], by = 2)) + #, expand = expansion(0, 0)) +
      scale_color_manual('Biogeographical \nregion', values = colors, labels = regions) +
      scale_fill_manual('Biogeographical \nregion', values = colors, labels = regions, guide = 'none') +
      basic_graph_theme

    g_isoline_bs_ci <- g_isoline_bs_ci + geom_point(merged_median_df, mapping=aes(x = median_t2m_mean-273.15, y = -1 * !!sym(paste0('median_', var_name_i)), group =bgr, color = bgr), size=5)

    # extract the legend and remove
    # legend_grob <- cowplot::get_legend(g_isoline_bs_ci)
    g_isoline_bs_ci <- g_isoline_bs_ci + theme(legend.position = "none")
    
    # arrange plots
    # g_isolines_g_isoline_bs_ci <- grid.arrange(g_isolines, g_isoline_bs_ci, NULL, legend_grob, nrow = 1, ncol = 4, widths = c(3, 3, -0.1, 1.3), heights = c(3))
    g_isoline_bs_ci_legend <- grid.arrange(g_isoline_bs_ci, NULL, legend_grob, nrow = 1, ncol = 3, widths = c(3, -0.15, 1.3), heights = c(3))
    
    # save
    ggsave(plot = g_isoline_bs_ci_legend, filename = paste0(output_path_plots, 'df_pdp_2d_isolines_bs_ci-', var_name_i, '-t2m_mean_targ-', target_i, '_basic_with_legend.png'), width = 18, height = 12)
    ggsave(plot = g_isoline_bs_ci, filename = paste0(output_path_plots, 'df_pdp_2d_isolines_bs_ci-', var_name_i, '-t2m_mean_targ-', target_i, '_basic.png'), width = fig_width_wide, height = fig_height_wide)
    ggsave(plot = legend_grob, filename = paste0(output_path_plots, 'df_pdp_2d_isolines_bs_ci-', var_name_i, '-t2m_mean_targ-', target_i, '_basic_legend.png'), width = fig_width_wide, height = fig_height_wide)
    
  }
  
}