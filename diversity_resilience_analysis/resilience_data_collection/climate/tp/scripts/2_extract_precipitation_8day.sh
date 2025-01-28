#########################################
##### 2_extract_precipitation.sh    #####
#########################################
# algorithm description
# create a 8-day moving cumulative total precipitation 

# insert your own local paths
IN_DIR="path_input_datasets_from_ERA5/"
OUT_PATH="path_local_output/"
OUT_DIR="tp/1_tp_daily/"
OUT_DIR_2="tp/2_tp_extract_8day/unshifted/"


# insert your own local downloaded hourly ERA5 data (by year)
FILE_BASE="cds_era5_land_total_precipitation_"

START_Y=2003
END_Y=2021 


for YEAR in $(seq $START_Y $END_Y) ; do
    echo $YEAR
    
    IN_PATH=${IN_DIR}${FILE_BASE}${YEAR}.nc
    OUT_PATH=${OUT_DIR}cds_era5_precipitation_${YEAR}_daysum.nc
    OUT_PATH_2=${OUT_DIR_2}cds_era5_precipitation_${YEAR}_daysum_timselmean8.nc

    echo ${IN_PATH}
    echo ${OUT_PATH}
    echo ${OUT_PATH_2}
#    echo ${OUT_PATH}cds_era5_prcp_${YEAR}_daysum_timsel8.nc
    
    # method to get the cumulative
    # shift timesteps back 1 hour (as final value is at start of next day)
    # select only the value at the end of the day
    # recalculate the value as mm (per day, per unit area)
    cdo -b F64 -mulc,1000  -selhour,23  -shifttime,-1hour ${IN_PATH}  ${OUT_PATH}
    # now we can take the 8-day mean (after removing the first timestep in wrong year (can't be bothered to append to previous year)
    cdo -b F64  -timselmean,8  -delete,timestep=1  ${OUT_PATH} ${OUT_PATH_2}

    # user can then remove the OUT_DIR as appropriate

    
done

