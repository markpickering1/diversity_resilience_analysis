# ########################################################
# Title         : plotByBGR_plotPDP_byBGR_bs.R
# Description   : plot results of plotByBGR_createPDP_byBGR_bs.R
#                 specifically all the 1d pdp from bs rf by bgr all in one single plot
#                 both overlayed and showing confidence iterval
# Inputs	      : df of pdp by bgr
# Outputs	      : plots
# Date          : 23/09/25
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
script_config_dir          <- '3_create_figures/input/'    ;   script_config_file <- 'input_plotByBGR.R'

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
input_path <- paste0(root_data_figs, script_output_ext, '_1d_pdp_bs/')
output_path <- paste0(root_data_figs, script_output_ext, '_1d_pdp_bs/plots/')

# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for figures of dataframe cols : ', output_path ) ) }
print(paste0('output_path is : ', output_path ))

# copy configuration input variables to output for storage
if( ! file.exists( paste0(output_path, script_config_file ) ) ) { print('copy config file') 
  file.copy(from= paste0( script_config_dir, script_config_file) , to= paste0(output_path, script_config_file ), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
} else{ print('could not copy config file') }

# load BGR file
load(paste0(input_dir_static, input_merged_bgr))

# round digit of bgr file
df_var[1:2] <- df_var[1:2] %>% round( digits = 3)

# load all data (since they are now all in one df)
load(paste0(root_data_proce, input_dir_rf, input_all_data))

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
df_pdp_bgr <- df_pdp_bgr[df_pdp_bgr$BiogeoRegions2016 %in% bgr, ]

#########################################################################
##### PLOT 1D PDPS FOR CURRENT AND INCREASED TEMPERATURE WITH CI    #####
#########################################################################
# this loops over the variables and plot the bs 1d pdp for current and increased temperature with ci
print('plotting')

# loop over diversity metrics and resilience metrics
for (i in 1:length(v_optional_predictors)){
  for (j in 1:length(v_target)){
    
    target_i <- v_target[j] ; print(target_i)
    tlabs <- l_lables_metrics[[target_i]] ; print(tlabs)
    tlabs <- paste0( '|', l_lables_metrics[[target_i]] , '|' ) ; print(tlabs)
    var_name_i <- v_optional_predictors[i] ; print(var_name_i)
    var_i_full <- l_lables_metrics[[var_name_i]] ; print(var_i_full)

    # create plot limits
    if (var_name_i == "Shannon"){
      xlims <- c(3, 4.5)
    } else if (var_name_i == "Kurtosis"){
      xlims <- c(0.3, 0.8) # for inverted kurtosis
    } else {
      xlims <- c(4, 15)
    }
       
    if(target_i=='kndvi_lambda_variance'){
      ylims <- c(0.85, 1.85)
      } else {
        ylims <- c(0.85, 1.7)
      }
    
    # list all the files in the folder containing the 1d pdps
    file_list <- list.files(path = input_path, full.names = TRUE)
    
    # identify names of pdps df of all seed and k (so for all bs models) for the specific combination of div and res variable
    pdp_df_list <- grep(paste0('df_pdp_1d-', var_name_i, '_targ-', target_i, '.*'), file_list, value = TRUE)
    
    # rowbinds all df of bs pdp 
    merged_pdp_df <- NULL
    for (k in 1:length(pdp_df_list)){
      df <- pdp_df_list[[k]]
      load(df)
      # extract bgr, seed and k values from filename
      seed <- as.numeric(gsub('.*_seed-(\\d+)_.*', '\\1', df))
      k <- as.numeric(gsub('.*_k-(\\d+).*', '\\1', df))
      bgr_index <- as.numeric(gsub('.*_bgr-(\\d+)_.*', '\\1', df))
      index <- paste0(pp_i$t[1], seed, k, bgr_index)
      index_tbgr <- paste0(pp_i$t[1], bgr_index)
      # add index column based on bgr, seed and k
      pp_i$seed <- seed
      pp_i$k <- k
      pp_i$bgr <- bgr_index
      pp_i$bgr <- factor(pp_i$bgr)
      pp_i$index <- index
      pp_i$index_tbgr <- index_tbgr
      # merge dataframes row-wise
      if (is.null( merged_pdp_df)) {
        merged_pdp_df <- pp_i
      } else {
        merged_pdp_df <- rbind(merged_pdp_df, pp_i)
      }
    }
    
    # fix kurtosis sign
    if (var_name_i == 'Kurtosis') {
      merged_pdp_df[[var_name_i]] <- -1*merged_pdp_df[[var_name_i]]
    }
    
    # define names of average, standard deviation and confidence interval band of res columns (for each div value)
    mean_res <- paste0('mean_', target_i)
    sd_res <- paste0('sd_', target_i)
    ci_low_res <- paste0('ci_low_', target_i)
    ci_up_res <- paste0('ci_up_', target_i)

    # calculate average, standard deviation and confidence interval band of div_var columns for each t2m_mean value, within same bgr
    merged_pdp_df_bs <- merged_pdp_df %>% group_by(index_tbgr, !!sym(var_name_i)) %>%
      summarise(!!sym(mean_res) := mean(!!sym(target_i)),
                !!sym(sd_res) := sd(!!sym(target_i)),
                !!sym(ci_low_res) := quantile(!!sym(target_i), probs = 0.025),
                !!sym(ci_up_res) := quantile(!!sym(target_i), probs = 0.975)
      )
    
    # keep only complete cases 
    merged_pdp_df_bs <- merged_pdp_df_bs[complete.cases(merged_pdp_df_bs), ]
    
    # create column with temperature and bgr value
    merged_pdp_df_bs$t <- substr(merged_pdp_df_bs$index_tbgr, 1, 1)
    merged_pdp_df_bs$bgr <- substr(merged_pdp_df_bs$index_tbgr, 2, 2)
      
    #### CREATE PLOTS ####
    
    # plot each pdp (one for seed and k, so one for each bs model) overlayed, one color per bgr
    g_pdp <- ggplot(merged_pdp_df, aes(x = !!sym(var_name_i), y = !!sym(target_i), group = index, color = bgr, linetype = as.factor(t))) + 
      geom_line(linewidth = 1) +
      ylab(tlabs) +
      xlab(var_i_full) +
      scale_y_continuous(limits = ylims, breaks = seq(ylims[1], ylims[2], by = 0.2)) + 
      scale_x_continuous(limits = xlims, breaks = seq(xlims[1], xlims[2], by = 0.2)) + 
      scale_color_manual(name = NULL, values = colors, labels = regions) +
      scale_linetype_manual(name = NULL, values = c("solid", "dashed"), labels = c("Current", "+1°C")) + 
      basic_graph_theme
    
    # extract the legend and remove
    legend_grob <- cowplot::get_legend(g_pdp)
    g_pdp <- g_pdp + theme(legend.position = "none")
    
    # plot pdps one color per bgr with ci
    g_pdp_bs_ci <- ggplot(merged_pdp_df_bs, aes(x = !!sym(var_name_i), y = !!sym(paste0('mean_', target_i)), group = index_tbgr, color = bgr, linetype = as.factor(t))) + 
      geom_ribbon(aes(ymin = !!sym(paste0('ci_low_', target_i)),
                      ymax = !!sym(paste0('ci_up_', target_i)),
                      group = index_tbgr,
                      fill = bgr),
                  linetype = "blank",
                  alpha = 0.2) +
      geom_line(linewidth = 1) +
      ylab(tlabs) +
      xlab(var_i_full) +
      scale_y_continuous(limits = ylims, breaks = seq(ylims[1], ylims[2], by = 0.2)) + 
      scale_x_continuous(limits = xlims, breaks = seq(xlims[1], xlims[2], by = 0.2)) + 
      scale_color_manual(name = NULL, values = colors, labels = regions) +
      scale_fill_manual(name = NULL, values = colors, labels = regions, guide = 'none') +
      scale_linetype_manual(name = '', values = c("solid", "dashed"), labels = c("Current", "+1°C")) +
      basic_graph_theme
    
    g_pdp_bs_ci <- g_pdp_bs_ci + theme(legend.position = "none")
    
    # make a df with no ci for t + 1
    merged_pdp_df_bs_no_ci <- merged_pdp_df_bs
    merged_pdp_df_bs_no_ci[[ci_low_res]][merged_pdp_df_bs_no_ci$t==1] <- merged_pdp_df_bs_no_ci[[mean_res]][merged_pdp_df_bs_no_ci$t==1]
    merged_pdp_df_bs_no_ci[[ci_up_res]][merged_pdp_df_bs_no_ci$t==1] <- merged_pdp_df_bs_no_ci[[mean_res]][merged_pdp_df_bs_no_ci$t==1]
    
    # plot pdps one color per bgr with ci only for current temperature
    g_pdp_bs_ci_no_ci <- ggplot(merged_pdp_df_bs_no_ci, aes(x = !!sym(var_name_i), y = !!sym(paste0('mean_', target_i)), group = index_tbgr, color = bgr, linetype = as.factor(t))) + 
      geom_ribbon(aes(ymin = !!sym(paste0('ci_low_', target_i)),
                      ymax = !!sym(paste0('ci_up_', target_i)),
                      group = index_tbgr,
                      fill = bgr),
                  linetype = "blank",
                  alpha = 0.2) +
      geom_line(linewidth = 1) +
      ylab(tlabs) +
      xlab(var_i_full) +
      scale_y_continuous(limits = ylims, breaks = seq(ylims[1], ylims[2], by = 0.2)) + 
      scale_x_continuous(limits = xlims, breaks = seq(xlims[1], xlims[2], by = 0.2)) + 
      scale_color_manual(name = NULL, values = colors, labels = regions) +
      scale_fill_manual(name = NULL, values = colors, labels = regions, guide = 'none') +
      scale_linetype_manual(name = '', values = c("solid", "dashed"), labels = c("Current", "+1°C")) +
      basic_graph_theme
    
    g_pdp_bs_ci_no_ci <- g_pdp_bs_ci_no_ci + theme(legend.position = "none")
    
    ggsave(plot = g_pdp_bs_ci, filename = paste0(output_path, 'pdp_1d_byBGR-', var_name_i, '_targ-', target_i, '_basic_bs_ci.png'), width = fig_width_wide, height = fig_height_wide)
    ggsave(plot = g_pdp_bs_ci_no_ci, filename = paste0(output_path, 'pdp_1d_byBGR-', var_name_i, '_targ-', target_i, '_basic_bs_ci_no_t1.png'), width = fig_width_wide, height = fig_height_wide)
    ggsave(plot = legend_grob, filename = paste0(output_path, 'pdp_1d_byBGR-', var_name_i, '_targ-', target_i, '_basic_legend.png'), width = fig_width_wide, height = fig_height_wide)
}
  
}
