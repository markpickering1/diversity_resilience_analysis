# ########################################################
# Title         : input_createDf_combine.R
# Description   : This text script acts as a user input to createDf_combine.R
#                 By setting variables in this file, the user should not need to edit the main code.
#                 createDf_combine.R should not change once initialised, except when running full separate
#                 analysis after major changes
# Date          : 23/05/23
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################


###################################################
######     I/O                                #####
###################################################

# set output path name - with date this will link output (variables) to next script
# instead just place data in the ts area
script_output_ext <- '3_combDFselect_origDetren'           # time-series stats

# input dataset containing the time-series statistic dataframes to combine
# input_dir_ts <- paste0(root_data_proce, '2_ts_statistics_sv1_2023-05-26/') # first attempt
# input_dir_ts <- paste0(root_data_proce, '2_ts_statistics_sv1_2023-06-28/') # post EGU presentation
# input_dir_ts <- paste0(root_data_proce, '2_ts_statistics_sv1_2023-11-01/')   # Smith Boers metrics
input_dir_ts <- paste0(root_data_proce, '2_ts_statistics_sv1_2023-11-09_origDetren/')   # New div metrics + SB metrics

# input dataset containing the static dataframes to combine
# input_dir_static <- paste0(root_data_proce, '1_inputDataframes_sv1_2023-05-23/')
# input_dir_static <- paste0(root_data_proce, '1_inputDataframes_sv1_2023-06-27/') # post EGU presentation
input_dir_static <- paste0(root_data_proce, '1_inputDataframes_sv1_2023-11-09/') # New div metrics + SB metrics

f_name_output_comb <- 'df_all_long_base'

#####################################################
###### SELECT VARIABLES OF INTEREST IN ANALYSIS #####
#####################################################
# two sets of dataframes are created for different variable types:
# 1) time-series variables (to extract TAC, mean, CV)
# 2) static variables (to extract single value snapshot)

# time-series variables to create dataframes containing TAC, mean, CV
v_variables        <- c('kndvi', 't2m', 'VPD',  'ssr',  'tp' ) #; v_variables <- c('kndvi', 'spei','swvl1')


# static variables to create dataframes of a single snapshot
# v_variables_static <- c('forestcover', 'socc30cm', 
#                         'diversity', 
#                         'EVI_dissimilarity', 'EVI_homogeneity', 'biomass', 'kndvi_diversity')
v_variables_static <- c('forestcover', 'socc30cm', 
                        'KG5', 'KG16',
                        'forestarea',
                        'topology',
                        'div_count',
                        'div_vert',
                        'div_horiz1', 'div_horiz2',
                        'div_hull'    
                        )
                        # 'diversity_structural') #,  # previous metric
                        # 'diversity_EVI_dissimilarity', 'diversity_EVI_homogeneity', 'diversity_biomass', 'diversity_kndvi')

# name of the forest cover label for use in masking
forestcover_name   <- 'forestcover'
