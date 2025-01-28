#########################################
##### 2_extract_temp_for_VPD.sh     #####
#########################################
# algorithm description
# 1) create a 8-day moving daily average for 2m dewpoint temperature
# 2) create a 8-day T2M max and min for 2m temperature

IN_DIR_T="path_input_datasets_from_ERA5/"
OUT_DIR="vpd/DT_Tmin_Tmax/"

FILE_BASE_T="cds_era5_land_2m_temperature_"
FILE_BASE_DT="cds_era5_land_2m_dewpoint_temperature_"


START_Y=2003
END_Y=2021 #2019


for YEAR in $(seq $START_Y $END_Y) ; do
    echo $YEAR
    
    IN_PATH_T=${IN_DIR_T}${FILE_BASE_T}${YEAR}.nc
    IN_PATH_DT=${IN_DIR_T}${FILE_BASE_DT}${YEAR}.nc

    echo ${IN_PATH_T}
    echo ${IN_PATH_DT}
    echo ${OUT_PATH}cds_era5_t2m_${YEAR}_daymin_timsel8.nc
    
    # get minimum temperature
    cdo -timselmean,8 -daymin ${IN_PATH_T} ${OUT_DIR}cds_era5_t2m_${YEAR}_daymin_timsel8.nc
    # get maximum temperature
    cdo -timselmean,8 -daymax ${IN_PATH_T} ${OUT_DIR}cds_era5_t2m_${YEAR}_daymax_timsel8.nc
    # get mean dewpoint temperature
    cdo -timselmean,8 -daymean ${IN_PATH_DT} ${OUT_DIR}cds_era5_dt_${YEAR}_daymean_timsel8.nc

done
