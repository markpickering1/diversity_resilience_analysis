#########################################
##### 2_extract_SSR.sh     #####
#########################################
# algorithm description
# 1) create a 8-day moving cumulative total ssr

# insert your own local paths
IN_DIR="path_input_datasets_from_ERA5/"
OUT_PATH="path_local_output/"
OUT_DIR="ssr/1_ssr_daily/"
OUT_DIR_2="ssr/2_ssr_extract_8day/unshifted/"


# insert your own local downloaded hourly ERA5 data (by year)
FILE_BASE="cds_era5_land_surface_net_solar_radiation_"

START_Y=2003
END_Y=2021


for YEAR in $(seq $START_Y $END_Y) ; do
    echo $YEAR
    
    IN_PATH=${IN_DIR}${FILE_BASE}${YEAR}.nc
    OUT_PATH=${OUT_DIR}cds_era5_ssr_${YEAR}_daysum.nc
    OUT_PATH_2=${OUT_DIR_2}cds_era5_ssr_${YEAR}_daysum_timselmean8.nc


    echo ${IN_PATH}
    echo ${OUT_PATH}
    echo ${OUT_PATH_2}


    # method to get the cumulative
    # shift timesteps back 1 hour (as final value is at start of next day
    # select only the value at the end of the day
    # units are (per day, per unit area)
    cdo -b F64  -selhour,23  -shifttime,-1hour ${IN_PATH}  ${OUT_PATH}
    # next get the daily mean over 8days
    cdo -b F64  -timselmean,8  -delete,timestep=1  ${OUT_PATH} ${OUT_PATH_2}
    

done
