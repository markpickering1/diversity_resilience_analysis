# original name : 2b_createNC_vpd.R
#
# ########################################################
# Title         : 
# Description   : This code takes the t2m 16-day max, t2m 16-day min and d2m 16-day mean (dewpoint temp) files
#                 It uses this information to calculate the saturated VP in kPa (via the average SVP(Tmax & Tmin)) 
#                 and actual (Tdew) vapour pressure via method: http://www.fao.org/3/x0490e/x0490e07.htm#calculation%20procedures
#                 Then a file is created of the SVP, AVP and VPD
# Inputs	      : DT_Tmin_Tmax/ T2M and D2M
#                 
# Outputs	      : netcdf vpd/
#                 
# Options	      : 
# Date          : 08/06/22
# Version       : 1.1
# Licence       : N/A
# Authors       : Mark Pickering
# Maintainer    : Mark Pickering
# Notes		      : 
#                 
# Example use   : 
# ########################################################

######     INITIALISE                         #####
rm(list = ls())
start_time <- Sys.time()
print(start_time)
print(paste0('tempdir :',tempdir() ) ) # write("TMP = /scratch/pickmar", file=file.path('~/.Renviron'))

######     GLOBAL VARS                        #####
script_name <-'2b_createNC_vpd.R'           # name of the script
script_info <-'2b_createNC_vpd'           # first attempt
start_year <- 2003 ; end_year <- 2021
timesel_period <- 8 # 16 # the time period the rolling average was calculated over

######     SET LIBRARIES                      #####
library(chron) # useful for converting time values
library(dplyr) # use %>%
library(raster) # package for raster manipulation
library(lattice)    # enables levelplots
# library(rasterVis)  # enables levelplots
library(ncdf4)
#library(lubridate)   # enables date manipulation
#require(ggplot2)
#require(magrittr)
require(here)
#require(tidyr)

######     SET I/O PATHS                      #####
# find date:
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ")
full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]

# base paths
root_data   <- 'vpd/'
input_path  <- paste0(root_data, 'DT_Tmin_Tmax/')
output_path <- paste0(root_data, 'vpd/')
print(paste0('output_path is : ', output_path ))
# if(! dir.exists(output_path)) {print('create output_path'); dir.create(paste0(output_path),recursive=T)}

# copy snapshot of r_script and headers to output directory for future reference
# root_script <- here()
root_script <- paste0(root_data, 'scripts')
# file.copy(paste0(root_script,'/', script_name),(paste0(output_path,'scripts/','copy_',full_date,'_',full_time,'_',script_name)))

######     SET FUNCTIONS                      #####

f_vapour_pressure <- function(T){
  # function takes a temperature T (or dewpoint T) in kelvin and calculates the vapour pressure using the formula:
  # VP E_saturated (kPa) = 0.6108 x e^(17.27 x T / (T + 237.3)
  # converte temperature to celsius
  T <- T - 273.15
  VP <- 0.6108 * exp( 17.27 *T / (T + 237.3))
  return(VP)
}

f_flip_reverse_array <- function(arr){
  # flip along the vertical and horizontal of an image
  # if there are multiple time slices (3rd dimension) it will do for each
  dim_3 <- dim(arr)[3]
  print(dim_3)
  if( is.null(dim_3) || is.na(dim_3)  ){
    arr <- apply(arr, 1, rev) #rev1 flips on horizontal, rev 2 flips along vertical ; 
    arr <- t(arr) # 
  } else{
    for(i in 1:dim_3){
      arr_i <- arr[,,i]
      arr_i <- apply(arr_i, 1, rev) 
      arr_i <- t(arr_i)
      arr[,,i] <- arr_i
    }
  }
  return(arr)
}


######     LOOP OVER FILES                    #####

#nc_files <- list.files(path=input_path, pattern=paste0("*"), full.names=FALSE, recursive=FALSE)

for(year_i in start_year:end_year){ # year_i <- 2008
print(year_i)

file_t2m_max <- paste0(input_path, 'cds_era5_t2m_',year_i,'_daymax_timsel', timesel_period, '.nc')
file_t2m_min <- paste0(input_path, 'cds_era5_t2m_',year_i,'_daymin_timsel', timesel_period, '.nc')
file_d2m     <- paste0(input_path, 'cds_era5_dt_',year_i,'_daymean_timsel', timesel_period, '.nc')

# We want to open the file - extract the raster stack, and the date, then rewrite the raster as a netcdf

# load the ncdf to extract attributes from the existing netcdf
ncin_t2m_max <- nc_open(paste0(file_t2m_max))
ncin_t2m_min <- nc_open(paste0(file_t2m_min))
ncin_d2m     <- nc_open(paste0(file_d2m))
print(paste("The ncin_t2m_max has",names(ncin_t2m_max$var),"variables"))
print(paste("The ncin_d2m has",names(ncin_d2m$dim),"dimensions"))

# Extract time units/vals:
time_units <- ncin_d2m$dim$time$units
time_vals  <- ncin_d2m$dim$time$vals

# create the lon/lat grid for the output .nc  taking the d2m file
lat <- ncvar_get(ncin_d2m, 'latitude' ) ; lon <- ncvar_get(ncin_d2m, 'longitude' )
#var_time <- ncvar_get(ncin_d2m, 'time' ) 
# nlon <- 43200 ; nlat <- 21600 # hardcode dimensions of the array
nlat <- length(lat) ; nlon <- length(lon) 
if(nlat != 3600 || nlon != 7200){ print(paste("NOTE: the file has ", nlon,' x ', nlat ," variables"))  }
array_res <- 360/nlon ; #   array_ <- 0.00833 # hardcode
print(paste0('resoltion: ', array_res, ' grid: ', nlon, ' x ', nlat))
# create lon/lat array
#lon <- as.array(seq(-180 + (array_res/2), 180 - (array_res/2), array_res)) ;  
#lat <- as.array(seq(-90 + (array_res/2) , 90  - (array_res/2), array_res)) ; 

# extract t2m vals
var_t2mMax_arr <- ncvar_get(ncin_t2m_max, 't2m' )
var_t2mMin_arr <- ncvar_get(ncin_t2m_min, 't2m' )
var_d2m_arr <- ncvar_get(ncin_d2m, 'd2m' )

# # check image
# stack_var_1 <- stack(paste0(file_d2m), varname = 'd2m') ;   
# plot(stack_var_1) 

# variable units
var_d2m_units <- ncatt_get(ncin_d2m, paste0('d2m') ,"units") 

# fill values
var_d2m_fillvalue <- ncatt_get(ncin_d2m, 'd2m' ,"_FillValue")  
#  var_t2m_fillvalue <- ncatt_get(ncin_t2m_max, 't2m' ,"_FillValue")  
var_t2mMax_arr[var_t2mMax_arr==var_d2m_fillvalue$value] <- NA  # should be superfluous
var_t2mMin_arr[var_t2mMin_arr==var_d2m_fillvalue$value] <- NA  # should be superfluous
var_d2m_arr[var_d2m_arr==var_d2m_fillvalue$value] <- NA  # should be superfluous

###### calculations
# flip transpose image to get correct orientation
#var_t2mMax_arr <- f_flip_reverse_array(var_t2mMax_arr)
#var_t2mMin_arr <- f_flip_reverse_array(var_t2mMin_arr)
#var_d2m_arr    <- f_flip_reverse_array(var_d2m_arr)

#image(lon,lat,var_d2m_arr_rev[,,1], col=rev(brewer.pal(10,"RdBu")))  # image array
# var_d2m_arr_test <- var_d2m_arr[,,1] 
# plot(stack(var_d2m_arr_test))

# get vapour pressure
es_t2m_max <- apply(var_t2mMax_arr,  c(1, 2, 3), FUN = f_vapour_pressure)
es_t2m_min <- apply(var_t2mMin_arr,  c(1, 2, 3), FUN = f_vapour_pressure)
ea_d2m     <- apply(var_d2m_arr   ,  c(1, 2, 3), FUN = f_vapour_pressure)

vpd <- (es_t2m_max + es_t2m_min)/2 - ea_d2m

#image(lon,lat,vpd[,,1], col=rev(brewer.pal(10,"RdBu")))  # image array

# get global attributes - only hist available
history <- ncatt_get(ncin_t2m_max,0,"history")

##### create and write a new file #############################
# create and write the netCDF file -- ncdf4 version
# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
tunits <- time_units #"days since 1970-01-01 00:00:00.0 -0:00"
timedim <- ncdim_def("time",tunits,time_vals)
VP_units <- 'kPa'  # define variables
fillvalue <- 32767

# es_tmax
out_var_1_name <- 'es_t2m_max'
out_var_1_long <- paste0('saturated vapour pressure (Tmax)')
out_var_1_def <- ncvar_def(out_var_1_name,VP_units,list(londim,latdim,timedim), fillvalue, out_var_1_long, prec="single",compression=9)

# es_tmin
out_var_2_name <- 'es_t2m_min'
out_var_2_long <- paste0('saturated vapour pressure (Tmin)')
out_var_2_def <- ncvar_def(out_var_2_name,VP_units,list(londim,latdim,timedim), fillvalue, out_var_2_long, prec="single",compression=9)

# es_d2m
out_var_3_name <- 'es_d2m'
out_var_3_long <- paste0('actual vapour pressure (d2m)')
out_var_3_def <- ncvar_def(out_var_3_name,VP_units,list(londim,latdim,timedim), fillvalue, out_var_3_long, prec="single",compression=9)

# vpd
out_var_4_name <- 'vpd'
out_var_4_long <- paste0('vapour pressure deficit)')
out_var_4_def <- ncvar_def(out_var_4_name,VP_units,list(londim,latdim,timedim), fillvalue, out_var_4_long, prec="double",compression=9)

# create file
output_file <- paste0(output_path, 'vpd_ERA5l_', year_i,'_timsel', timesel_period, '.nc')
print(paste0('create file: ', output_file))
ncfname <- paste0(output_file)
ncout <- nc_create(ncfname,list(out_var_1_def,out_var_2_def,out_var_3_def,out_var_4_def),force_v4=TRUE)

print('put variables')
# add variables
ncvar_put(ncout,out_var_1_def, es_t2m_max)  
ncvar_put(ncout,out_var_2_def, es_t2m_min) 
ncvar_put(ncout,out_var_3_def, ea_d2m )   
ncvar_put(ncout,out_var_4_def, vpd )   

# put additional attributes into dimension and data variables
ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncout,"lat","axis","Y")
ncatt_put(ncout,"time","axis","T")

# add global atts
history <- paste("convert t2m monthly mean of daily min and max, and d2m monthly mean of daily dewpoint temperature, to saturated/actual VP, and vapour pressure deficit", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
#ncout  # print data
nc_close(ncout) # write data
print('file created')
nc_close(ncin_d2m )
nc_close(ncin_t2m_max)
nc_close(ncin_t2m_min)
rm(var_d2m_arr ,var_t2mMax_arr , var_t2mMin_arr , vpd, es_t2m_max, es_t2m_min, ea_d2m )
}

# ######     FINALISE                           #####
# timing
end_time <- Sys.time()
print(end_time - start_time)
