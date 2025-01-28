#########################################
##### 2_extract_temp_for_T2M_8day.sh  #####
#########################################
# algorithm description
# 1) create a 8-day moving daily average for  temperature

# insert your own local downloaded hourly ERA5 data (by year)
IN_DIR="path_input_datasets_from_ERA5/"
OUT_DIR="t2m/1_t2m_daily/"

# insert your own local downloaded hourly ERA5 data (by year)
FILE_BASE="cds_era5_land_2m_temperature_"

START_Y=2003
END_Y=2021


for YEAR in $(seq $START_Y $END_Y) ; do
    echo $YEAR
    
    IN_PATH=${IN_DIR}${FILE_BASE}${YEAR}.nc

    echo ${IN_PATH}
    echo ${OUT_PATH}cds_era5_t2m_${YEAR}_daymean_timsel8.nc
    
    # get minimum temperature
    cdo -timselmean,8 -daymean ${IN_PATH} ${OUT_DIR}cds_era5_t2m_${YEAR}_daymean_timsel8.nc

done
