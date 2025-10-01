# ########################################################
# Title         : input_createDf_fromInputNC.R
# Description   : This text script acts as a user input to 1_createDf_fromInputNC.R
#                 By setting variables in this file, the user should not need to edit the main code
#                 This file should not change once initialised, except when running full separate
#                 analysis after major changes
# Date          : 19/02/25
# Authors       : Mark Pickering & Agata Elia
# Notes         : Create separate input file for different runs
# ########################################################


###################################################
######     I/O                                #####
###################################################

# set output path name - with date this will link output (variables) to next script
script_output_ext <- '1_inputDataframes_sv1'               


###################################################
######     SET INDIVIDUAL INPUT FILE DATASETS #####
###################################################
# Here are the links for the different input variables files that are to be 
# included in the analysis dataframe. The user must set the links to these files themselves
# links to individual are set from root_data_input area in input_initialise_R.R

# Resilience dataset of interest (currently separate paths for values time-series and deseasonalised residuals time-series)
input_kndvi_deseason            <- 'vegetation/8day/kNDVI/kndvi_2003_2021_deseasonalised_005.nc'  # deseasonalised data
input_kndvi_nodeseason <- 'vegetation/8day/kNDVI/kndvi_2003_2021_005.nc'        # original data                        

# directory containing datasets of time-series of climate 
input_t2m   <- 'climate/8day/T2M/T2M_3_europe/resolution/'   # t2m_allY_timsel8_europe3nn_deseason.nc
input_VPD   <- 'climate/8day/VPD/VPD_3_europe/resolution/'   # VPD_allY_timsel8_europe3nn_deseason.nc
input_ssr   <- 'climate/8day/SSR/4_SSR_europe/resolution/'   # SSR_allY_timsel8_europe3nn_deseason.nc
input_tp    <- 'climate/8day/tp/4_tp_europe/resolution/'     # tp_allY_timsel8_europe3nn_deseason.nc

# nomenclature of temporal datasets
# format of values    should be: var_suffix.nc
values_ts_suffix <- '_allY_timsel8_europe3nn_baseVar'
# format of residuals should be: var_suffix.nc
residuals_ts_suffix <- '_allY_timsel8_europe3nn_deseason'

# fixed datasets path (eventually will go to a more fitting location)
# other static variables
input_hansen            <- 'static_variables/hansen/hansen_forest_cover_nc_masked_005/hansenForestCoverNoLoss2000AtModisMean_masked50_005.nc'   # forest cover dataset 
input_socc              <- 'static_variables/socc/SOCC_nc_masked_005/SOCC30cmRescaledAtModis_masked50_005.nc'                                   # soil carbon content 
input_Ndep              <- 'static_variables/Ndepo/multiyear_mean/EMEP01_rv5_3_year_totalRdN.nc' # Nitrogen deposition
input_topology          <- 'static_variables/srtm/SRTM_005_nc_aligned_df/df_elevation.RDATA'  # elevation data 
input_bgr               <- 'ancillary/EEA_biogeographic_regions/BiogeoRegions2016_wgs84_europe.tif'

# calculations of forest area (not required)
input_forestpixelcount  <- 'ancillary/area/hansenForestCoverNoLoss2000AtModisMean_mask50_005.nc'  

# diversity metric variables - which may be split across several datasets
input_biodiv            <- 'biodiversity/figshare_ESSD_paper_output/dataset/Metrics/Structural_diversity_5km.nc'
# previous forms of diversity variable inputs
# input_biodiv_count      <- 'biodiversity/count.nc'           # number GEDI points
# input_biodiv_vert       <- 'biodiversity/mean_std.nc'        # diversity rhXX + mean + sd + kurt + skew
# input_biodiv_horiz_1    <- 'biodiversity/entropy-2-3-4-7_bins05.nc' #  diversity entropies
# input_biodiv_horiz_2    <- 'biodiversity/eucldist_2to4+7.nc' # diversity euclidean distance
# input_biodiv_hull       <- 'biodiversity/hull_2to4+7s.nc'    # diversity convex hull vol
# input_biodiv_struct     <- 'biodiversity/divmetrics.tif'        # structural diversity  if in single dataset

#####################################################
###### SELECT VARIABLES OF INTEREST IN ANALYSIS #####
#####################################################
# two sets of dataframes are created for different variable types:
# 1) time-series variables (to extract TAC, mean, CV)
# 2) static variables (to extract single value snapshot)
# comment out unnecessary variables

# time-series variables to create dataframes containing TAC, mean, CV
v_variables  <- list(
  'kndvi' = NA,       # separately set the input file
  't2m'   = input_t2m,
  'VPD'   = input_VPD,
  'ssr'   = input_ssr,
  'tp'    = input_tp
)

# static metrics
v_variables_static  <- list(
  'forestcover' = input_hansen  , 
  'socc30cm'    = input_socc    ,
  'Ndep'        = input_Ndep    ,
  'forestpixelcount'  = input_forestpixelcount    ,
  'topology'    = input_topology,
  'bgr'         = input_bgr,
  'div'         = input_biodiv
  # previous extra diversity var inputs
  # 'div_count'   = input_biodiv_count,
  # 'div_vert'    = input_biodiv_vert,
  # 'div_horiz1'  = input_biodiv_horiz_1,
  # 'div_horiz2'  = input_biodiv_horiz_2,
  # 'div_hull'    = input_biodiv_hull
  # 'diversity_structural' = input_biodiv_struct

)
