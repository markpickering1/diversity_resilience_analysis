#########################################
##### 4_merge_remap.sh              #####
#########################################
# algorithm description
# 1) merge to single file
# 2) convert to coordinates harmonised with vegetation

IN_DIR="vpd/vpd_2_align/leap_years/"
OUT_DIR="vpd/vpd_2_align/"
TRG_GRD_DIR="other/targetgrid/"
TRG_GRD="targetgrid_original_lonlat_01_36001801_-180to180_90to-90.txt"

cdo mergetime ${IN_DIR}cds*.nc ${OUT_DIR}vpd_allY_timsel8.nc

# remap
cdo remapbil,${TRG_GRD_DIR}${TRG_GRD}  ${OUT_DIR}vpd_allY_timsel8.nc ${OUT_DIR}vpd_allY_timsel8_remap.nc
