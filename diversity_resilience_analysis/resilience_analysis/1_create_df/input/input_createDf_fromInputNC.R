# ########################################################
# Title         : input_createDf_fromInputNC.R
# Description   : This text script acts as a user input to createDf_fromInputNC.R
#                 By setting variables in this file, the user should not need to edit the main code
#                 This file should not change once initialised, except when running full separate
#                 analysis after major changes
# Date          : 20/05/23
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
# included in the analysis dataframe
# links to individual are set from root_data_input area in input_initialise_R.R
# eventually these links should be standardised

# Resilience dataset of interest (currently separate paths for values time-series and residuals time-series)
#input_kndvi            <- 'vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10_deseasonalised_masked_005/merged_kndvi_no_mask_2003_2021_rescaled10_deseasonalised_masked50_005.nc' # old version pre alignment fix, now in data/old_pre_alignment_fix/... 
#input_kndvi_nodeseason <- 'vegetation/8day/KNDVI_no_mask/KNDVI_no_mask_nc_merged_rescaled10_masked_005/merged/merged_kndvi_no_mask_2003_2021_rescaled10_masked50_005.nc'                        # old version pre alignment fix, now in data/old_pre_alignment_fix/...
input_kndvi            <- 'vegetation/8day/kNDVI/kNDVI_nc_merged_rescaled10_masked_deseasonalised_005/merged_kndvi_2003_2021_rescaled10_masked50_deseasonalised_005.nc'                          # new version post alignment fix
input_kndvi_nodeseason <- 'vegetation/8day/kNDVI/kNDVI_nc_merged_rescaled10_masked_005/merged/merged_kndvi_2003_2021_rescaled10_masked50_005.nc'                                                 # new version post alignment fix

# directory containing datasets of time-series of climate 
input_t2m   <- 'climate/8day/T2M/T2M_3_europe/resolution/'   # t2m_allY_timsel8_europe3nn_deseason.nc
input_VPD   <- 'climate/8day/VPD/VPD_3_europe/resolution/'   # VPD_allY_timsel8_europe3nn_deseason.nc
input_ssr   <- 'climate/8day/SSR/4_SSR_europe/resolution/'   # SSR_allY_timsel8_europe3nn_deseason.nc
input_tp    <- 'climate/8day/tp/4_tp_europe/resolution/'     # tp_allY_timsel8_europe3nn_deseason.nc
input_spei  <- 'climate/8day/SPEI/SPEI_3_europe/resolution/' # SPEI_allY_timsel8_europe3nn_deseason.nc
input_swvl1 <- 'climate/8day/SM/SM_3_europe/resolution/'     # SM_allY_timsel8_europe3nn_deseason.nc

# old variables incorrect accumulation
# input_ssr   <- 'climate/8day/SSR/SSR_3_europe/resolution/'   # SSR_allY_timsel8_europe3nn_deseason.nc
# input_tp    <- 'climate/8day/tp/tp_3_europe/resolution/'     # tp_allY_timsel8_europe3nn_deseason.nc 


# nomenclature of climate datasets
# format of values    should be: var_suffix.nc
values_ts_suffix <- '_allY_timsel8_europe3nn_baseVar'
# format of residuals should be: var_suffix.nc
residuals_ts_suffix <- '_allY_timsel8_europe3nn_deseason'

# fixed datasets path (eventually will go to a more fitting location)
# other static variables
input_hansen            <- 'static_variables/hansen/hansen_forest_cover_nc_masked_005/hansenForestCoverNoLoss2000AtModisMean_masked50_005.nc'   # forest cover dataset new version post alignment fix, old in data/old_pre_alignment_fix/...
input_socc              <- 'static_variables/socc/SOCC_nc_masked_005/SOCC30cmRescaledAtModis_masked50_005.nc'                                   # soil carbon content new version post alignment fix, old in data/old_pre_alignment_fix/...
#input_topology          <- 'static_variables/srtm/SRTM_005/RData/df_elevation.RDATA'                                                           # elevation data old version pre alignment fix, now in data/old_pre_alignment_fix/...
input_topology          <- 'static_variables/srtm/SRTM_005_nc_aligned_df/df_elevation.RDATA'                                                    # elevation data new version post alignment fix

# climate zones
input_KG5               <- 'ancillary/KG_biome/europe/koppen-Geiger_5.nc'                            # 5 KG regions: tropical, arid, temperate, continental, ICE
# 5zones={'EF':5, 'ET':5, 'Cfc':3, 'BSk':2, 'Cfb':3, 'Csb':3, 'Csc':3, 'BWk':2, 'Cfa':3, \
#   'Cwa':3, 'Csa':3, 'BSh':2, 'BWh':2, 'Cwb':3, 'Aw':1, 'Cwc':3, 'Am':1,  \
#   'Af':1, 'As':1, 'Dwc':4, 'Dwb':4, 'Dfb':4, 'Dfc':4, 'Dsa':4, 'Dsb':4,  \
#   'Dsc':4, 'Dwa':4, 'Dfa':4, 'Dwd':4, 'Dfd':4, 'Dsd':4}
input_KG16              <- 'ancillary/KG_biome/europe/koppen-Geiger_16.nc'                           # 16 KG regions
# 16zones={'Dfa':16,  'Dfb':15,  'EF':14,   'ET':13,   'Dfc':12, 'Dfd':12,
#   'Dwc':11, 'Dwb':11, 'Dwa':11, 'Dwd':11,  'Dsa':10, 'Dsb':10,'Dsd':10,'Dsc':10,
#   'Cfc':9, 'Cfb':9,'Cfa':9,  'Cwa':8, 'Cwb':8, 'Cwc':8,  'Csb':7, 'Csc':7,'Csa':7
#   'BWk':6, 'BWh':6,  'BSk':5, 'BSh':5,   'Aw':4, 'As':3,'As':3, 'Am':2,  'Af':1 }
input_forestpixelcount  <- 'ancillary/area/hansenForestCoverNoLoss2000AtModisMean_mask50_005.nc'                           # 16 KG regions



# diversity metric variables
input_biodiv_count      <- 'biodiversity/forbiores/231108_aligned_data/count.nc'           # number GEDI points
input_biodiv_vert       <- 'biodiversity/forbiores/231108_aligned_data/mean_std.nc'        # rhXX + mean + sd + kurt + skew
# input_biodiv_horiz_1    <- 'biodiversity/forbiores/231108_aligned_data/entropy_2to4+7s.nc' # old (231109) too many data pointsshannon/simpson/rao entropy
input_biodiv_horiz_1    <- 'biodiversity/forbiores/231108_aligned_data/entropy-2-3-4-7_bins05.nc' # new (240907)
input_biodiv_horiz_2    <- 'biodiversity/forbiores/231108_aligned_data/eucldist_2to4+7.nc' # euclidean distance
input_biodiv_hull       <- 'biodiversity/forbiores/231108_aligned_data/hull_2to4+7s.nc'    # convex hull vol
# input_biodiv_struct     <- 'biodiversity/forbiores/postEGU_update/divrast.tif'        # post EGU metrics (4/23 -> 8/11/23)
# input_biodiv_struct     <- 'biodiversity/forbiores/first_look/divmetrics.tif'        # structural diversity first dataset

# alt diversity metric variables
input_EVI_dissimilarity <- 'biodiversity/earthenv/dissimilarity_nc_005/Dissimilarity_01_05_5km_uint32_europe_rescaled_005.nc'     # dissimilarity in EVI - prev called heterogeneity
input_EVI_homogeneity   <- 'biodiversity/earthenv/homogeneity_nc_005/Homogeneity_01_05_5km_uint16_europe_rescaled_005.nc'         # homogeneity in EVI
input_biomass           <- 'biodiversity/biomass/dissimilarity_homogeneity/biomass_diversity_MW3_rescaled_005.nc'                 # biomass diversity 
input_kndvi_diversity   <- 'biodiversity/kndvi/dissimilarity_homogeneity/df_kndvi_diversity_5km_MW3.RDATA'                        # kndvi dissimilarity


#####################################################
###### SELECT VARIABLES OF INTEREST IN ANALYSIS #####
#####################################################
# two sets of dataframes are created for different variable types:
# 1) time-series variables (to extract TAC, mean, CV)
# 2) static variables (to extract single value snapshot)
# comment out unnecessary variabels

# # time-series variables to create dataframes containing TAC, mean, CV
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
  'KG5'         = input_KG5     , 
  'KG16'        = input_KG16    ,
  'forestpixelcount'  = input_forestpixelcount    ,
  'topology'    = input_topology,
  'div_count'   = input_biodiv_count,
  'div_vert'    = input_biodiv_vert,
  'div_horiz1'  = input_biodiv_horiz_1,
  'div_horiz2'  = input_biodiv_horiz_2,
  'div_hull'    = input_biodiv_hull
  # 'diversity_structural' = input_biodiv_struct
  
)

##### old input files


# # time-series variables to create dataframes containing TAC, mean, CV
# v_variables        <- c('kndvi', 't2m', 'VPD',  'ssr',  'tp' ) #; v_variables <- c('kndvi', 'spei','swvl1')
# 
# # links to files containing dataframes (currently kndvi in unique format)
# v_files            <- c(NA,  input_t2m, input_VPD, input_ssr, input_tp) #  input_spei, input_swvl1


# v_variables_static <- c( 'forestcover', 'socc30cm',
#                          'KG5', 'KG16',
#                          'topology',
#                          'diversity_structural' )
# #                        , 'diversity_EVI_dissimilarity', 'diversity_EVI_homogeneity', 'diversity_biomass', 'diversity_kndvi')
# v_files_static     <- c(input_hansen,  input_socc, 
#                         input_KG5, input_KG16,
#                         input_topology,
#                         input_biodiv_struct )
# #                       , input_EVI_dissimilarity, input_EVI_homogeneity, input_biomass, input_kndvi_diversity )


