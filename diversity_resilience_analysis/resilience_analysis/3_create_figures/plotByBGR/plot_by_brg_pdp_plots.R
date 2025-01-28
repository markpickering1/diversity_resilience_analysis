# ########################################################
# Title         : plot_by_bgr_pdp_plots.R
# Description   : plot results of plot_by_bgr_pdp.R
#                 specifically all the pdp by bgr in one single plot
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

###################################################
######       I/O                              #####
###################################################

# output location
# set/create output directory
output_path <- paste0(root_data_figs, script_output_ext, '_pdp_', full_date,  '/')
output_path_plots <- paste0(root_data_figs, script_output_ext, '_pdp_', full_date, '/plots/')

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
    
    # create plot
    png(paste0(output_path_plots, 'df_rf_model_partDep_', var_name_i, '_targ-', target_i, '_by_bgr.png'), width = 300, height = 600)
    
    # select labels
    if(target_i=='kndvi_TAC'){
      ylabs <- 'kNDVI TAC'
      if (var_name_i == 'sd_rh98'){
        xlims <- c(2, 7)
        ylims <- c(0.2, 0.55)
      } else {
        xlims <- c(4, 7)
        ylims <- c(0.2, 0.55)
      }
    } else if (target_i =='kndvi_lambda_variance') {
      ylabs <- "kNDVI lambda var"
      if (var_name_i == 'sd_rh98'){
        xlims <- c(2, 7)
        ylims <- c(-1.8, -0.6)
      } else {
        xlims <- c(4, 7)
        ylims <- c(-1.8, -0.6)
      }
    } else {
      ylabs <- 'kNDVI lambda xt'
      if (var_name_i == 'sd_rh98'){
        ylims <- c(-2.0, -0.6)
        xlims <- c(2, 7)
      } else {
        ylims <- c(-2.0, -0.6)
        xlims <- c(4, 7)
      }
    }
    
    # create an empty plot where to store all the PDP by bgr
    plot(1, type = "n", xlim =xlims, ylim = ylims, xlab=var_name_i, ylab=ylabs, main = paste0(var_i_full, ' PDP by BGR'))
    
    #### PLOT PDPS BY BIOGEOGRAPHICAL REGION ####
    # identify unique KG classes to loop over
    bgr <- c(1, 4, 7, 9, 11, 12)
    colors <- c("blue", "darkgreen", "cyan", "red", "lightgreen", "orange")
    regions <- c("alpine", "atlantic", "continental", "mediterranean", "pannonian", "steppic")
    
    # bgr <- c(1, 2, 4, 5, 7, 9, 10, 11, 12)
    # colors <- c("red", "green", "blue", "orange", "purple", "pink", "brown", "cyan", "magenta")
    # regions <- c("alpine", "anatolian", "atlantic", "blacksea", "continental", "mediterranean", "outside", "pannonian", "steppic")
    
    # add legend
    legend("topleft", legend=regions, col=colors, lty=1, cex=0.8, bty = "n")
    
    # run through every BGR and run PDP separately by classes
    for (r in 1:length(bgr)){
      
      # select bgr
      bgr_region <- bgr[r]  ; print(bgr_region)
      color <- colors[r]
      
      # load the pdp df
      load(paste0(output_path, 'df_rf_model_partDep_div-', var_name_i, '_targ-', target_i, '_bgr-', bgr_region,  '.RData'))
      
      # plot the pdp by bgr
      lines(x=pp_i$variable, y=pp_i$resilience, type='l', lwd = 1, col = color)
      
    }
    
    grid(nx = NULL, ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 0.5)      # Grid line width
    
    # save the plot
    dev.off()
    
  }
}