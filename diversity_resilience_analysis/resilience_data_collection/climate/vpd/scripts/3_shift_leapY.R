# original name : 3_shift_leapY.R
#
# ########################################################
# Title         : shift leap years
# Description   : Load the 2004, 2008, 2012, 2016, 2020 files and shift the days after the leapyear by a day
#                 
# Outputs	      : netcdf 
# Inputs        : netcdf 
# Options	      : 
# Date          : 14/09/22
# Version       : 1.1
# Licence       : N/A
# Authors       : Mark Pickering
# Maintainer    : Mark Pickering
# Notes		      : when applying file to other directories change variable_i and check the year extraction in the file loop works
#                 set variable_i_ncName , had to change longitude latitude and remove unused variables in other files
# Example use   : 
# resources:    : 

# ########################################################



###################################################
######     SCRIPT DETAILS AND ALGO            #####
###################################################
# loop over files in input directory
#   load netcdf details
#   shift the timesteps as required
#   save new netcdf

###################################################
######     INITIALISE                         #####
###################################################

rm(list = ls())
start_time <- Sys.time()
print(start_time)
# print(paste0('tempdir :',tempdir() ) ) # write("TMP = /scratch/pickmar", file=file.path('~/.Renviron'))
# find date:
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ")
full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]

######     GLOBAL VARS                        #####
script_name  <- '3_shift_leapY.R'           # name of the script
script_info  <- '3_shift_leapY'           # first attempt
variable_i   <- 'vpd'
variable_i_ncName <- 'vpd'

######     SET LIBRARIES                      #####
library(chron) # useful for converting time values
library(dplyr) # use %>%
library(raster) # package for raster manipulation
library(lattice)    # enables levelplots
library(ncdf4)
# library(lubridate)   # enables date manipulation
# require(ggplot2)
# require(magrittr)
# require(here)
# require(tidyr)
# requred for JEODPP
# library("rgdal",lib="~/R") #  personally installed library(gdal) #
library(RColorBrewer)
# sessionInfo() # check versions of packages


###################################################
######     SET I/O PATHS                      #####
###################################################

# base paths
root_data   <- paste0(variable_i,'/')
input_path  <- paste0(root_data, variable_i,'_1_separate/')
output_path     <- paste0(root_data, variable_i,'_2_align/leap_years/LY/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {print('create output_path'); dir.create(paste0(output_path),recursive=T)}

# copy snapshot of r_script and headers to output directory for future reference
root_script <- paste0(root_data, 'scripts')
# file.copy(paste0(root_script,'/', script_name),(paste0(output_path,'scripts/','copy_',full_date,'_',full_time,'_',script_name)))

###################################################
######     LOOP OVER PET PCP FILES            #####
###################################################
# for(year_i in start_year:end_year){ # year_i <- 2008
nc_files <- list.files(path=input_path, pattern=paste0("*"), full.names=FALSE, recursive=FALSE) # view all files
# nc_files <- nc_files[2:length(nc_files)]
for(file_i in nc_files){ # file_i <- nc_files[3]
  
  # select only nc files with leap years 
  file_i_segment <- strsplit(file_i, "\\.")[[1]][2]   # extract filename after '.'
  if( is.na(file_i_segment) || file_i_segment != 'nc'){next}
  file_i_name <-  strsplit(file_i, "\\.")[[1]][1]
  file_i_year <- strsplit(file_i_name , '_')[[1]][3] # extract the year
  file_i_year <- as.numeric( file_i_year )
  
  if(is.na(file_i_year) || file_i_year %% 4 != 0 ) {next}
  print(file_i) ; print (file_i_year)
  
  ####
  ## now we should have only leap years - we can load the nc files and then extract the data, shift time and close
  ####
  
  ncin <- nc_open(paste0(input_path, file_i))
  print(paste("The ncin has",names(ncin$var),"variables"))
  print(paste("The ncin has",names(ncin$dim),"dimensions"))
  
  # Extract time units/vals:
  time_units <- ncin$dim$time$units
  # print(paste0('time units in ncdfs: ', time_units))
  time_vals  <- ncin$dim$time$vals
  
  chron_date_1900 <- paste0("01/01/1900") ; chron_time_1900 <- times("00:00:00")
  time_vals_ref <- chron(dates= chron_date_1900, times = chron_time_1900)
  chron_date_2802 <- paste0("02/28/", as.character(file_i_year)) ; chron_time_2802 <- times("23:00:00")
  time_vals_2802 <- chron(dates= chron_date_2802, times = chron_time_2802)
  time_vals_human <- (time_vals/24) + time_vals_ref
  # print(time_vals_human ) ; # dates(time_vals_human)
  # print(length(time_vals_human))
  
  # extract the index of the first date after 28th February
  index_LYdate <- 0 # zero index of first date after 28/02
  for(j in 1:length(dates(time_vals_human)) ) {
    date_j <- dates(time_vals_human)[j]
    # print(date_j)
    if(date_j < time_vals_2802 )  { index_LYdate <- j }
  }
  index_LYdate <- index_LYdate + 1 # add one to account for difference
  
  # Shift the time of all dates above the index
  time_vals[index_LYdate:length(time_vals)] <- time_vals[index_LYdate:length(time_vals)] + 24
  time_vals_human_change <- (time_vals/24) + time_vals_ref
  
  ####
  ## extract remaining nc data
  ####
  
  # create the lon/lat grid for the output .nc  taking the input files
  # (This creates the grid at the normal orientation, the input files are rotated 180 - need to treat this)
  lat <- ncvar_get(ncin, 'lat' ) ; lon <- ncvar_get(ncin, 'lon' )
  # var_time <- ncvar_get(ncin_pcp, 'time' )
  nlat <- length(lat) ; nlon <- length(lon) 
  array_res <- 360/nlon ; #   array_ <- 0.00833 # hardcode
  print(paste0('resoltion: ', array_res, ' grid: ', nlon, ' x ', nlat))
  
  # extract water balance values
  var_mat <- ncvar_get(ncin, variable_i_ncName ) ; # dim(var_pet) ; plot(stack(var_pet[,,1]))
  # get units
  var_units <- ncin$var[[variable_i_ncName]][['units']] # ncatt_get(var_balance, 'balance' ,"units")
  var_longName <- ncin$var[[variable_i_ncName]][['longname']] # ncatt_get(var_balance, 'balance' ,"units")
  # image(lon,rev(lat),var_balance[,,1], col=rev(brewer.pal(10,"RdBu")))  # image array
  
  # fill values - should be superfluous
  var_fillvalue <- ncatt_get(ncin, variable_i_ncName ,"_FillValue")  
  var_mat[var_mat==var_fillvalue$value] <- NA  # should be superfluous
  
  history <- ncatt_get(ncin,0,"history")
  
  
  ####
  ## save new file
  ####
  
  ##### create and write a new file #############################
  # create and write the netCDF file -- ncdf4 version
  # define dimensions
  londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
  latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
  tunits <- time_units #"days since 1970-01-01 00:00:00.0 -0:00"
  timedim <- ncdim_def("time", time_units, time_vals)
  
  
  # define variable
  out_var_1_name <- variable_i_ncName
  out_var_1_long <- var_longName
  out_var_1_def <- ncvar_def(out_var_1_name,var_units,list(londim,latdim,timedim), var_fillvalue$value, out_var_1_long, prec="single",compression=9)
  
  # create file
  output_file <- paste0(output_path, file_i_name,  '.nc')
  print(paste0('create file: ', output_file))
  ncfname <- paste0(output_file)
  ncout <- nc_create(ncfname,list(out_var_1_def),force_v4=TRUE)
  
  
  ncvar_put(ncout,out_var_1_def, var_mat, verbose = T)  
  
  # put additional attributes into dimension and data variables
  ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout,"lat","axis","Y")
  ncatt_put(ncout,"time","axis","T")
  
  
  # ncatt_put(ncout,0,"history",history)
  
  # write
  nc_close(ncout) # write data
  print('file created')
  nc_close(ncin )
  
}
