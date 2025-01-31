# ########################################################
# Title         : plot_by_bgr_map.R
# Description   :plot a map of bgr
#                 
# Aims          : map bgr 
# Inputs	      : bgr df 
# Outputs	      : map of bgr
# Options	      : 
# Date          : 13/12/24
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
output_path <- paste0(root_data_figs, script_output_ext, '_map_bgr_', full_date,  '/')
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
df_pdp_bgr <- df_pdp_bgr[df_pdp_bgr$BiogeoRegions2016 %in% c(1, 7, 9), ]

# identify bgr classes for plots
bgr <- c(1, 7, 9)
# colors <- c("blue", "cyan", "red")
# colors <- c("darkturquoise", "darkgreen", "salmon4")
#colors <- c("#D55E00", "#009E73","#F0E442")
#colors <- c("#753401", "#05A076","#F0E442")
#colors <- c("#0C94EC", "#0B5843","#F0E442")
#colors <- c("#5F5F5F", "#009E73","#F0E442")
colors <- c("#D55E00", "#5BB99F","#F0E442")
regions <- c("alpine", "temperate", "mediterranean")

# # find boundaries of data
# df_bbox <- st_as_sf(df_pdp_bgr, coords = c("x", "y"))
# bbox <- st_bbox(df_bbox)
# bbox["ymin"] <- bbox["ymin"] - 10 
# 
# # calculate bounding box and crop the land shapefile
# land_shapefile_cropped <- st_crop(land_shapefile, bbox)

g_input <-  ggplot() +
  geom_tile(data = df_pdp_bgr, aes(x = x, y = y, color = as.factor(BiogeoRegions2016))) + 
  geom_sf(data = land_shapefile) +
  labs( x='', y='') +
  coord_sf() + 
  scale_color_manual(
    values = colors,
    labels = regions,
    name = "Biogeographical region"
  ) +
  basic_fig_theme +
  theme(legend.position = "none")

g_input

ggsave(plot = g_input, filename = paste0(output_path, 'bgr_map_pastel_green.png'), width = fig_width_wide, height = fig_height_wide)
