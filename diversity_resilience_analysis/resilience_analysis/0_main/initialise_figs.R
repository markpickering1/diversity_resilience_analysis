# ########################################################
# Title         : initialise_figs.R
# Description   : This script initialises certain plotting standards and variables 
#                 in order to run the analysis R code. Script should be sourced when 
#                 plotting analysis figures.
#                 Themes
# Date          : 19/02/25
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################

## to setup this environment, run this file in each plotting script via:
# source(path_figures_init)

###################################################
######     LOAD PLOTTING FUNCTIONS & packages #####
###################################################
# this contains all the packages required to use this script
# there may be extra packages required in other local scripts that call this
source('0_main/functions/plotting_functions.R')

require(ggplot2)      # for plotting
library(raster)       # package for raster manipulation
library(sf)           # utilise shapefiles etc


###################################################
######     PLOTTING VARIABLES                 #####
###################################################
# set global variables for plotting
#library(terra)

########### PLOT SIZE AND SHAPES     #############
text_size = 20                # text size in figures - big maps
legend_size <- 1.4	      # size of legend in figures
legend_width <- 1.4	      # width of legend in figures

# figure width and height (selected for fitting histogram under maps)
fig_width  = 8
fig_height = 10

# figure wide - width and height for wider plots
fig_width_wide  = 12
fig_height_wide = 8

########### LABLES FOR METRICS       #############
# set universal labels for metrics
l_lables_metrics <- list(
  "Kurtosis" = 'FSDV'  ,   "Canopy_heights_sd"  = 'FSDH' ,   "Shannon"    = 'FSDH+V',         
  "Coefficient_variation" = "CoV", "Skewness" = "Skewness",            
  "Cover_sd" = "S.D. in Cover",  "Rao"   = 'Rao Quadratic Entropy' ,    "Hull" = "Hull Vol." ,
  
  # key div metrics - OLD Nomenclature - expanded vars.
  # 'sd_rh98' = 'S.D. in RH 98 [m]',    , "mu_kurt" = 'Excess kurtosis', "shannon_entropy" = 'Shannon Entropy',
  # 'sd_rh98' = 'Horizontal diversity'      , "mu_kurt" = 'Vertical diversity', "shannon_entropy" = 'Combined diversity',
  # 'sd_rh98' = 'FSDH'      , "mu_kurt" = 'FSDV', "shannon_entropy" = 'FSDH+V',
  # diversity predictors 
  # 'mu_rh50' = 'RH 50 [m]'              , 'mu_rh75' = 'RH 75 [m]'          , 'mu_rh98' = 'RH 98 [m]',
  # 'sd_rh50' = 'S.D. in RH 50 [m]' , 'sd_rh75' = 'S.D. in RH 75 [m]', 'sd_rh98' = 'S.D. in RH 98 [m]',
  # "mu_skew" = 'Skewness'               , 
  # "simpson_index" = 'Simpson Index',
  # 'mu_fhd_normal'  = 'FHD Normal'      , 'euclidean_distances_mean' = 'Euclid. Dist.',
  # 'rao_quadratic_entropy' = 'Rao Quadratic Entropy',
  
  # Resilience metrics
  "kndvi_TAC" = 'kNDVI TAC' , 
  "kndvi_lambda_xt" = 'Rest. Rate AC1' , "kndvi_lambda_variance" = 'Rest. Rate Variance', 
  "kndvi_lambda"    = 'Restoration Rate'     ,
  
  # other predictors 
  'kndvi_mean' = 'Mean kNDVI'            , "kndvi_CV" = 'kNDVI C.V.' , 
  'socc30cm' = 'Soil Carbon 30cm [Mg/ha]', 
  'forestcover'= 'Forest Cover'          , 'topology_elevation_std' = 'Elevation S.D. [m]',
  't2m_mean'   = '2m temp mean [K]'      , 't2m_CV'   = '2m temp C.V.'  ,     't2m_TAC' = '2m temp AC1',
  'tp_mean'    = 'Precip. Mean [mm/day]' , 'tp_CV'    = 'Precip. C.V.'  ,     'tp_TAC'  = 'Precip. AC1', 
  'VPD_mean'   = 'VPD mean [kPa]'        , 'VPD_CV'   = 'VPD C.V.'      ,     'VPD_TAC' = 'VPD AC1'    ,
  'ssr_mean'   = 'SSR mean [W/m2]'       , 'ssr_CV'   = 'SSR C.V.'      ,     'ssr_TAC' = 'SSR AC1',

  # others
  't2m_trend'  = 'Temperature trend [°C/yr]',  # is actually per 19-years -must to convert
  'Ndep'       = 'N deposition [mgN/m2]'       # mean 2013-2018
)

l_diversity_metrics_list <- list(
  "no_diversity",
  "Coefficient_variation", "Skewness" ,            
  "Kurtosis"  ,            "Canopy_heights_sd" ,    "Cover_sd" ,             "Shannon"     ,         
  "Rao"       ,            "Hull"  
  # "mu_rh98"                 ,   "mu_rh50"                 , "mu_rh75"                 ,  "mu_rh25",
  # "mu_skew"                 ,   "mu_kurt"                 , "mu_sd"                   ,
  # "sd_rh98"                 ,   "sd_rh75"                 , "sd_rh50"                 ,  "sd_rh25"                  ,
  # "shannon_entropy"         ,   "simpson_index"           ,
  # "rao_quadratic_entropy"   , "euclidean_distances_mean"
)

########### COLOUR SCHEMES           #############
# agg_fact <- 1        # aggregation T/F and factor for visualisation purposes (speeds up visualisation but associated problems)
# color scheme for variables    diversity          red             blue           brown            yellow              green
group.colors_Gio <- c(Diversity = "#9933ff", T2M = "#e60000",  TP = "#0000ff", SSR ="#734d26",  VPD = "#ffcc00", Other = "#00cc00")

colors_resilience_metrics <- setNames(c("red", "blue", "green"), c("kndvi_lambda_variance", "kndvi_lambda_xt", "kndvi_TAC") )

colors_diversity_metrics <- setNames(c("#E41A1C", "#377EB8", "#4DAF4A"), c("FSDH", "FSDV", "FSDH+V") ) # CB robust  palette.colors(8, "Dark 2")

########### HISTOGRAMS ONLY #############
n_bins <- 100

########### MAPS ONLY       #############
# extent_europe <- c(-10.66164, 44.56369, 33.0, 71.18416 ) # full Europe
extent_europe <- c(-10.66164, 44.56369, 33.0, 53 ) # Europe GEDI


###################################################
######     THEMES                             #####
###################################################

# set theme for figures/maps
basic_fig_theme <- ggplot2::theme( # set the 'theme' various aesthetic features
  plot.title = element_text(size=text_size, hjust = 0.5), # resize centre the plot title
  text = element_text(size = text_size),
  
  axis.text.x  = element_blank(),    # remove text on axis
  axis.text.y  = element_blank(),    # remove text on axis
  axis.ticks.x = element_blank(),   # remove tick marks for x-axis
  axis.ticks.y = element_blank(),   # remove tick marks for y-axis

  legend.key.width = unit(legend_width, 'cm'), #change legend key width
  legend.key.size = unit(legend_size, 'cm'), #change legend key size
  legend.position = "bottom",
  legend.background = element_rect(fill = "white"), # grey95 grey box around legend
  # legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')
  # element_rect = element_blank(),
  
  panel.grid = element_line(color = "white"), # "transparent" set the colour of the grid lines
  panel.background = element_blank() # set the colour of the panel behind the figure
)

basic_graph_theme <- theme( # set the 'theme' various aesthetic features
  plot.title = element_text(size=text_size*2, hjust = 0.5), # resize centre the plot title
  text = element_text(size = text_size*2),
  # legend.position = "bottom",
  legend.background = element_rect(fill = "white"), # grey95 grey box around legend
  # legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')
  # element_rect = element_blank(),

  panel.grid = element_line(color = "grey95"), # "transparent" set the colour of the grid lines
  panel.background = element_blank(), # set the colour of the panel behind the figure
  axis.line.x = element_line(color="black", linewidth = 1),
  axis.line.y = element_line(color="black", linewidth = 1),
  
  legend.key.width = unit(legend_width, 'cm'), #change legend key width
  legend.key.size = unit(legend_size, 'cm') #change legend key size
)

########### HISTOGRAMS ONLY #############

basic_hist_theme <- theme( # plot.margin = margin(0, 210, 0, 210),    # add left right margin so that the hist arranges better
                    text = element_text(size = text_size)
)           


###################################################
######     MAP SHAPEFILES                     #####
###################################################

# Europe only 
# load and process shapefile
# land_shapefile_in <- paste0(root_data_input, input_land_shapefile) # set shapefile path (global coastlines@50m)
land_shapefile_in <- sf::st_read(land_shapefile_in, quiet = TRUE)                         # read shapefile
# summary(land_shapefile) # plot(land_shapefile)

# load bounding box from shapefile (has coords: -10.66164 34.56369 44.82037 71.18416)
# bb_shapefile <- 'data/ancillary/world_vectors/boundingBoxes/Europe_BB.shp'
# bboxpolygon <- sf::st_read(bb_shapefile, quiet = TRUE)                         # read shapefile

# alternative - create bounding box manually
# bb_shapefile <- as(raster::extent(-10.66164, 34.56369, 44.82037, 71.18416), "SpatialPolygons")
bb_shapefile <- as(raster::extent(extent_europe), "SpatialPolygons")
sp::proj4string(bb_shapefile) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# crs(bb_shapefile) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# bb_shapefile <- rgeos::bbox2SP(n = -10.66164, s = 34.56369, w = 44.82037, e = 71.18416, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# crop land_shapefile
land_shapefile <- sf::st_crop(land_shapefile_in, bb_shapefile)                          # crop shapefile to bounding box
# plot(land_shapefile)
