########################################################
# Title         : input_createDf_statistics.R
# Description   : This text script acts as a user input to createDf_statistics.R
#                 By setting variables in this file, the user should not need to edit the main code
#                 This file should not change once initialised, except when running full separate
#                 analysis after major changes
# Date          : 19/02/25
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################

###################################################
######     I/O                                #####
###################################################

# set output path name - with date this will link output (variables) to next script
script_output_ext <- '2_ts_statistics_sv1'           # time-series stats     

# input dataset containing the dataframes to calculate stats for
# version_num <- '2025-02-19' # extra version indicator, e.g. full_date from run of the previous script
input_dir <- paste0(root_data_proce, '1_inputDataframes_sv1', '/')

# input phenology dataframe to be used in case of growing season masking
f_phenology_df <- 'ancillary/phenology/modis_phenology_nc_masked_005_df/phenology_df.RData'   

#####################################################
###### SELECT VARIABLES OF INTEREST IN ANALYSIS #####
#####################################################
# two sets of dataframes are created for different variable types:
# 1) time-series variables (to extract TAC, mean, CV)
# 2) static variables (to extract single value snapshot)

# time-series variables to create dataframes containing TAC, mean, CV
v_variables        <- c( 'kndvi',
                         't2m',
                         'VPD',
                         'ssr', 
                          'tp'
                         ) 

###################################################
######     SET OUTLIERS THRESHOLD             #####
###################################################
# set the threshold (std. dev) for cloud cover contamination removal
outlier_threshold <- -2.5

###################################################
######     ACTIVATE GS AND DETREND            #####
###################################################

# set options to activate/deactivate outliers masking, growing season masking and detrending of time-series
b_stats_outliers       <- F      # create df of kNDVI z-scores before and after outlier masking for cross-checking
b_mask_outliers        <- TRUE   # remove outliers in the kNDVI above outlier_threshold
b_mask_gs              <- TRUE   # mask the growins season
b_detrend              <- TRUE   # de-trend the GS using a simple linear detrend
# set option to save intermediate dataframes throughout
b_save_intermediate_df <- TRUE

