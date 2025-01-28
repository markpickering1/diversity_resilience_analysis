# ########################################################
# Title         : createDF_kndviclim.R
# Description   : create a df of X | Y | Date & one of each of KNDVI + climate variables
#                 given in  input/input_createDf_fromInputNC.R
#                 Also create df of residual & separately a static X | Y df of other variables
# Aims          : create two dataframes of all data and RF predictor data for each X/Y
# Inputs	      : links to netcdf containing KNDVI and CLIMATE data as well as non time-series data from 
# Outputs	      : two dataframes for each timeseries variable (var summary stats and residual summary stats)
# Options	      : 
# Date          : 20/5/23
# Version       : 3 
# Authors       : Mark Pickering & Agata Elia
# Notes		      : 
# Example use   : 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())                                   

# set initialisation script names
main_initialisation_file   <- '0_main/initialise_R.R'
script_config_dir          <- '1_create_df/input/'    ;   script_config_file <- 'input_createDf_fromInputNC.R'

# initialise code setup and repo building
source(main_initialisation_file)
# initialise user inputs (config) to script
source( paste0( script_config_dir, script_config_file) )
# initialise functions
source('1_create_df/functions/f_convert_nc_to_df.R')

######     SET LIBRARIES                      #####
library(chron)   # useful for converting time values
library(dplyr)   # use %>%
library(ncdf4)   # read ncdf
library(reshape) # reshaping dataframes e.g. melt
library(raster)  # package for raster manipulation
library(terra)   # terra library for raster manip
library(rgdal)        # 
library(sf)           # utilise shapefiles etc

###################################################
######       I/O                              #####
###################################################

# set/create output directory
output_path <- paste0(root_data_proce, script_output_ext, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))

# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for dataframes of analysis inputs : ', output_path ) ) }

# copy configuration input variables to output for storage
if( ! file.exists( paste0(output_path, script_config_file ) ) ) { print('copy config file') 
  file.copy(from= paste0( script_config_dir, script_config_file) , to= paste0(output_path, script_config_file ), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
  } else{ print('could not copy config file') }

###################################################
######       OPEN TS INPUT - EXTRACT STATS    #####
###################################################
# open the time-series input ncdf files and extract 

# loop over the time-series variables
for (i in 1:length(v_variables)){
  var_i <- names(v_variables)[i] ; print(var_i)
  
  # extract the file path for the time-series of values and time-series of residuals
  # currently a specific treatment for kndvi due to separate paths for time-series of values & residuals
  if(var_i =='kndvi'){
    file_i_deseason   <- paste0(root_data_input, input_kndvi)
    file_i_baseVar    <- paste0(root_data_input, input_kndvi_nodeseason)
  } else {
    file_i_deseason   <- paste0(root_data_input, v_variables[[var_i]] , var_i, residuals_ts_suffix, '.nc' ) # prev v_files[i]
    file_i_baseVar    <- paste0(root_data_input, v_variables[[var_i]] , var_i, values_ts_suffix   , '.nc' )
  }

  #### create dataframe of time-series of variable values ####
  df_var <- f_convert_nc_to_df(file_i_baseVar)
  names(df_var)[4] <- var_i
  save(df_var, file=paste0(output_path, 'df_',var_i, '_baseVar_full.RData' )    )
  
  #### create dataframe of time-series of variable residuals ####
  df_var <- f_convert_nc_to_df(file_i_deseason)
  names(df_var)[4] <- var_i
  save(df_var, file=paste0(output_path, 'df_',var_i, '_deseason_full.RData' )    )
  
  # output timings
  f_time_update(t_start_time)
}  


###################################################
######   OPEN STATIC INPUT - EXTRACT STATS    #####
###################################################
# open the time-series input ncdf files and extract 

# loop over the time-series variable
for (j in 1:length(v_variables_static)){ # j <- 5
  v_variable_j   <- names(v_variables_static)[j] ; print(v_variable_j)
  
  file_i_baseVar <- paste0(root_data_input, v_variables_static[[v_variable_j]]  ) #v_files_static[j] )
  
  # extract file type from right of fullstop
  file_type <- strsplit(file_i_baseVar, "[.]")[[1]][2]   
  # loading method depends on file type
  if       ( file_type == 'nc' & v_variable_j == 'div_vert' ){
    # if file netcdf format - and latest format of split spatial mean + spatial sd of vertical values
    # need to treat differently as contains bands of sd mean values
    
    text_band <- f_convert_nc_to_df_textBand(file_i_baseVar, label_text_band = "bands_labels")
    
    # open in terra to extract rast objects and relabel
    r_i_baseVar_mu <- rast(file_i_baseVar, subds	="band_mean")
    r_i_baseVar_sd <- rast(file_i_baseVar, subds	="band_stdev")
    names(r_i_baseVar_mu) <- paste0('mu_', text_band)
    names(r_i_baseVar_sd) <- paste0('sd_', text_band)
    
    # now put together in a df
    df_var_mu <- terra::as.data.frame(r_i_baseVar_mu, xy = T, na.rm = NA) # set to NA for cells with NA in all layers removed
    df_var_sd <- terra::as.data.frame(r_i_baseVar_sd, xy = T, na.rm = NA) # set to NA for cells with NA in all layers removed
    
    df_var <- full_join(df_var_mu, df_var_sd)
    
  } else if (file_type == 'nc'){
    # if file netcdf format - repeat process from above
    r_i_baseVar    <- rast(file_i_baseVar) # ; plot(r_i_baseVar)
    
    # Calculate the area of each pixel in square kilometers and add as layer
    if (v_variable_j == 'forestpixelcount'){
      df_area <- terra::cellSize(r_i_baseVar, unit="km")
      r_i_baseVar <- c(r_i_baseVar, df_area)
    }
    df_var <- terra::as.data.frame(r_i_baseVar, xy = T, na.rm = NA) # set to NA for cells with NA in all layers removed
    # calculate the total area in each pixel that we consider forest
    if (v_variable_j == 'forestpixelcount'){  df_var <- df_var %>% mutate( forestarea = 0.01 * forestpixelcount * area ) }

    
  } else if (file_type == 'tif'){
    # if file in tif format - need to load then resample to fit resilience metric (i.e. v_variables[1])
    
    r_i_baseVar_div <- rast(file_i_baseVar)
    r_i_baseVar     <- rast(paste0(root_data_input, input_kndvi))  #  plot(r_i_baseVar[[1]])
    
    # match the diversity grid to kndvi for compatibility
    r_i_baseVar_div_resamp <- resample(r_i_baseVar_div, r_i_baseVar, "near") 
    # crs(r_i_baseVar, describe=TRUE, proj=TRUE) ; plot(r_i_baseVar_div) ; plot(r_i_baseVar_div_resamp) ; res(r_i_baseVar)
    df_var <- terra::as.data.frame(r_i_baseVar_div_resamp, xy = T, na.rm = NA) # set to NA for cells with NA in all layers removed
    # head(df_var) ; summary(df_var)
    
  } else if (file_type == 'RDATA'){
    # if file is already formatted in dataframe format, simply copy across
    # load(file_i_baseVar)            # 
    df_var <- load_RDATA(file_i_baseVar)
  } else{
    print(paste0('error in file type of file: ', file_i_baseVar) )
  }

  # if only a single values column then match to variable name
  if( length(names(df_var)) == 3 ) { names(df_var)[3] <- v_variable_j
  } else if( grepl('div', v_variable_j) | v_variable_j == 'forestarea'  ){ # for the diversity metrics don't give a prefix
    print("don't rename cols") # colnames(df_var)[3:length(df_var)] <- paste0('div_', colnames(df_var)[3:length(df_var)])   
  }else{ # for the rest give the column names a prefix of the variable names 
    colnames(df_var)[3:length(df_var)] <- paste0(v_variable_j, '_', colnames(df_var)[3:length(df_var)])   # paste(colnames(df_stats)[3:length(df_stats)], var_name, sep="_")
  }
  
  # head(df_var)
  save(df_var, file=paste0(output_path, 'df_',v_variable_j, '_baseVar_full.RData' )    ) 
  
} 

# output timings
print('end of script')
# output final timing
f_time_update(t_start_time)


#########
## END ##
#########


# ###################################################
# ######       TEST PLOT SINGLE SLICE           #####
# ###################################################
# df_i_baseVar_1 <- df_i_baseVar[1:3]
# 
# r_i_baseVar_1 <- terra::rast(  as.matrix(df_i_baseVar_1) , type="xyz")
# plot(r_i_baseVar_1)
# # lost a lot of data - why?
# 
# r2_i_baseVar  <- raster(file_i_baseVar) #, varname = 'kndvi') ; plot(r_i_baseVar[[1]])
# plot(r2_i_baseVar)
# # convert r to dataframe, reset titles, convert to long format
# df2_i_baseVar <- as.data.frame(r2_i_baseVar, xy = T, na.rm = T)
# head(df2_i_baseVar)
