# ########################################################
# Title         : f_convert_nc_to_df.R
# Description   : function taking a netcdf filepath and creating a df
#                 df is corrected for to label layers as different dates
#                 and converted to long format
#                 
#                 
# Date          : 21/05/23
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################

###################################################
######     CONVERT NC TO DF                   #####
###################################################

# take the path to a netcdf file and convert it to a long dataframe incorporating the layers as dates
f_convert_nc_to_df <- function(path_nc_in){
  
  # open raster and extract dates of entries for the values
  r_i_baseVar  <- rast(path_nc_in) #  plot(r_i_baseVar[[1]])
  times_i_baseVar <- list(time(r_i_baseVar)) # ; typeof(input_time[[1]]) # [[c(1,2)]]
  dates_i_baseVar <- sapply(  strsplit( as.character(times_i_baseVar[[1]]), ' ' ) , '[', 1 )
  
  # convert raster to dataframe, reset titles, convert to long format
  df_i_baseVar <- terra::as.data.frame(r_i_baseVar, xy = T, na.rm = NA) # set to NA for cells with NA in all layers removed
  # head(df_i_baseVar[,1:6]) ; dim(df_i_baseVar)  # summary(df_i_baseVar) # hist(df_i_baseVar[[3]])
  # set the column names as the corresponding dates
  names(df_i_baseVar)[3:length(names(df_i_baseVar))] <- dates_i_baseVar
  # convert to a long format dataframe and set variable of interest
  df_i_baseVar <- melt(df_i_baseVar, id=c("x","y"), variable_name = "date") # head(df_i_baseVar) , value.name = 'kndvi') # value_name = "kndvi" )
  
  return(df_i_baseVar)
}

# take a filepath of rdata object and load the object to a variable
load_RDATA <- function(file_name){
  #loads an RDATA file, and returns it
  load(file_name)
  get(ls()[ls() != "file_name"])
}

# take the path to a netcdf file and convert the text band 
f_convert_nc_to_df_textBand <- function(path_nc_in, label_text_band){
  # open in netcdf to extract label names
  nc_data <- nc_open(file_i_baseVar) 
  text_band <- ncvar_get(nc_data, label_text_band)
  nc_close(nc_data)
  return(text_band)
}

