# ########################################################
# Title         : elevation_createDF.R
# Description   : create a df of X | Y | elevation | elevation s.d. | slope | slope s.d.
# Aims          : to create a combined df of elevation and slope data
# Inputs	      : nc files of elevation and slope
# Outputs	      : combined dfs of variables
# Date          : 02/08/23
# Version       : 1
# Authors       : Mark Pickering & Agata Elia
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################
rm(list = ls())                                   # remove loaded objects

source('main/initialise_R.R')

######     GLOBAL VARS                        #####
script_info      <- 'elevation_createDF.R'               # used in output name

######     SET LIBRARIES                      #####
library(chron)   # useful for converting time values
library(dplyr)   # use %>%
# library(ncdf4)   # read ncdf
# library(reshape) # reshaping dataframes
library(raster)  # package for raster manipulation
library(terra)   # terra library for raster manip
# library(reshape) # melt dfs to wide-long
# library(rasterVis)  # enables levelplots
# require(ggplot2)      # for plotting
# library(ggpubr)       # for arranging ggplots together
# library(ggExtra)      # more complex figures, marginal hists
# library(grid)       # for making multi-gird plots and tables 
# library(gridExtra)  # for making multi-gird plots and tables 
# library(ggplotify)  # plots within levelplots
# library(rgdal)        # 
# library(RColorBrewer) # colour palettes
# library(sf)           # utilise shapefiles etc
# library(glcm)           # calculate image textures (e.g. homogeneity/dissimilarity)


###################################################
######       I/O                              #####
###################################################

# input dir
dir_ext_elevation <- 'static_variables/srtm/'
input_dir <- paste0( root_data_input, dir_ext_elevation, 'SRTM_005_nc_aligned/' )

# initialise output
output_path <- paste0( root_data_input, dir_ext_elevation,  'SRTM_005_nc_aligned_df/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; print('creating dir')} # create directory if not present

###################################################
######       OPEN TIFs AND PUT IN DF          #####
###################################################

# get list of files to loop over and put in DF
file_list <- list.files(path = input_dir, pattern = "*.nc") ; print(file_list)

# initialise dataframe to store values
df_elevation   <- data.frame()

# Loop through the RData objects, load them,
for (i in 1:length(file_list)){ 
  rdata_file <- file_list[i]
  print(rdata_file)
  
  # open raster and inspect
  r_i  <- terra::rast(paste0(input_dir,rdata_file) )
  
  ######       CONVERT TO DF                    #####
  df_i <- as.data.frame(r_i, xy = T, na.rm=T)
  
  # create df or join to existing dfs
  if(i == 1){df_elevation <- df_i
  } else{ 
    df_elevation <- full_join(df_elevation, df_i) 
  }
  
} # end of file loop  

names(df_elevation) <- c('x', 'y', 'elevation_mean', 'elevation_std', 'slope_mean', 'slope_std')
head(df_elevation) ; summary(df_elevation) ; dim(df_elevation)

save(df_elevation, file = paste0(output_path, 'df_elevation.RDATA' ) )