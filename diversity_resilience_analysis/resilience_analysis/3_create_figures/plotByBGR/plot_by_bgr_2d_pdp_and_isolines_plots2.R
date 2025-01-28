# ########################################################
# Title         : plot_by_bgr_2d_pdp_and_isolines_plots2.R
# Description   : plot results of plot_by_bgr_2d_pdp_and_isolines.R
#                 specifically all the isolines by bgr all in one single plot
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


###################################################
######       I/O                              #####
###################################################

# output location
# set/create output directory
# full_date <- '2024-01-31'
output_path <- paste0(root_data_figs, script_output_ext, '_2d_pdp_and_isolines_', full_date,  '/')
output_path_plots <- paste0(root_data_figs, script_output_ext, '_2d_pdp_and_isolines_', full_date,  '/plots/')

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
    
    # select labels
    if(target_i=='kndvi_TAC'){
      tlabs <- 'kNDVI TAC'
    } else if (target_i =='kndvi_lambda_variance') {
      tlabs <- "kNDVI lambda var"
    } else {
      tlabs <- 'kNDVI lambda xt'
    }
    
    # create plot
    png(paste0(output_path_plots, 'df_pdp_2d_isolines-', var_name_i, '-t2m_mean_targ-', target_i, '.png'), width = 800, height = 800)
    
    # create plot limits
    if (var_name_i == "shannon_entropy"){
      xlims <- c(285.5, 292.1)
      ylims <- c(4.5, 5.8)
    } else {
      xlims <- c(285.5, 292.1)
      ylims <- c(5, 15)
    }
    
    # create a new blank plot
    plot(1, type = "n", xlim = xlims, ylim = ylims, xlab = 't2m (mean)', ylab = var_i_full, main = paste0("Isolines of ", tlabs, " by bgr"))
    
    # identify unique KG classes to loop over
    bgr <- c(1, 4, 7, 9)#, 11, 12)
    colors <- c("blue", "darkgreen", "cyan", "red")
    colors_legend <- c("blue", "blue", "darkgreen", "darkgreen", "cyan", "cyan", "red", "red")#, "lightgreen", "orange")
    regions <- c("alpine", "atlantic", "continental", "mediterranean") #, "pannonian", "steppic")
    
    # create legend item
    legend_item = c('alpine - median', 'alpine - mean', 'atlantic - median', 'atlantic - mean', 'continental - median', 'continental - mean', 'mediterranean - median', 'mediterranean - mean')
    
    # run through every BGR and run PDP separately by classes
    for (r in 1:length(bgr)){
      
      # select bgr
      bgr_region <- bgr[r]  ; print(bgr_region)
      
      # load df with averages temperature and diversity
      load(file=paste0(output_path, 'df_all_div-', var_name_i, '_targ-', target_i, '_bgr-', bgr_region, '_mean_t2m_div_res.RData'))
      
      # identify mean t2m and mean div
      df_pdp_bgr_r_mean
      mean_t2m_mean <- df_pdp_bgr_r_mean[1, 1]; mean_t2m_mean
      mean_div <- df_pdp_bgr_r_mean[1, 3]; mean_div
      mean_res <- df_pdp_bgr_r_mean[1, 2]; mean_res
      median_t2m_mean <- df_pdp_bgr_r_mean[1, 4]; median_t2m_mean
      median_div <- df_pdp_bgr_r_mean[1, 6]; median_div
      median_res <- df_pdp_bgr_r_mean[1, 5]; median_res
      
      # run through the extra vars and create 2d pdp
      for (l in 1:length(pdp_2d_extra_vars)){
        
        # select second independent var for 2d pdp
        pdp_2d_var_l <- pdp_2d_extra_vars[l] 
        
        #### PLOT THE ISOLINES
        
        # load the isoline df
        load(file=paste0(output_path, 'df_pdp_2d_isoline-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region, '_', 'median', '.RData'))
        
        # select a color for plotting
        color <- colors[r]
        
        # plot the isoline
        lines(x=isoline_df$t2m_mean, y=isoline_df[[var_name_i]], type='l', col = color, lwd = 2, lty=1)
        points(x=median_t2m_mean, y=median_div, col=color, pch = 19)
        
        # load the isoline df
        load(file=paste0(output_path, 'df_pdp_2d_isoline-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region, '_', 'mean', '.RData'))
        
        # plot the isoline
        lines(x=isoline_df$t2m_mean, y=isoline_df[[var_name_i]], type='l', col = color, lwd = 2, lty=2)
        points(x=mean_t2m_mean, y=mean_div, col=color)
        
      }
      
    }
    
    # add legend
    legend("topright", legend=legend_item, col=colors_legend, lty=c(1, 2, 1, 2, 1, 2, 1, 2), cex=1, bty = "n", y.intersp = 1)
    
    grid(nx = NULL, ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 0.5)      # Grid line width
    
    # save the plot
    dev.off()
    
  }
  
}