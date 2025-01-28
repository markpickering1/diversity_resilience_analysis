# ########################################################
# Title         : 2_createDF_statistics.R
# Description   : create a df of X | Y | mu | TAC | CoV 
#                 for each time-series variable at each point in the ncdf files
#                 create a df of mean, coeff of var, 1-lag TAC for each variable
# Aims          : dataframes of all data and RF predictor data for each X/Y
# Inputs	      : dfs containing KNDVI and CLIMATE data extracted from raster
#                 one df for residuals and one for actualy values
# Outputs	      : separate and combined dfs of variables
# Options	      : 
# Date          : 21/5/23
# Version       : 3 
# Authors       : Mark Pickering & Agata Elia
# Notes		      : 
# Example use   : 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())  

# set initialisation script names
main_initialisation_file   <- '0_main/initialise_R.R'
script_config_dir          <- '1_create_df/input/'    ;   script_config_file <- 'input_createDf_statistics.R'

# initialise code setup and repo building
source(main_initialisation_file)
# initialise user inputs (config) to script
source( paste0( script_config_dir, script_config_file) )
# initialise functions
source('1_create_df/functions/f_statistical_functions.R')

######     SET LIBRARIES                      #####
library(chron)   # useful for converting time values
library(dplyr)   # use %>%
library(robustbase)  # For robust regression

###################################################
######       I/O                              #####
###################################################

# set/create output directory
output_path <- paste0(root_data_proce, script_output_ext, '_', full_date,  '/')
print(paste0('output_path is : ', output_path ))

# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for dataframes of analysis inputs : ', output_path ) ) }

# copy configuration input variables to output for storage
if( ! file.exists( paste0(output_path, script_config_file ) ) ) { print('copy config file') 
  file.copy(from= paste0( script_config_dir, script_config_file) , to= paste0(output_path, script_config_file ), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
} else{ print('could not copy config file') }

###################################################
######     EXTRACT DATE INDEX                 #####
###################################################
# take all the dates and label indices for t1 item and t2 (1-lag) item for calculating autocorrel

# load any dataframe from list (that should contain entire timeseries)
load( paste0(input_dir, 'df_', v_variables[1], '_deseason_full.RData'  ) )
df_var <- na.omit(df_var) 

# select all the unique timestamps to be considered in the analysis
v_dates <-  unique(df_var$date)
if( length(v_dates) != 874) {'Warning: check dates maybe something wrong'}
rm(df_var)

###################################################
######       CYCLE OVER VARIABLE              #####
###################################################

# if growing season masking option is active, load phenology dataframe
if(b_mask_gs){  load( paste0(root_data_input, f_phenology_df)) }
# head(phenology_df)

# extract background mean, CoV and residual TAC for the different input variabes, v_variables
for (i in 1:length(v_variables)){ # i <- 1
  var_i <- v_variables[i] ; print(var_i)
  
  ###################################################
  ###### RESIDUALS: PRE-PROCESSING AND TESTING  #####
  ###################################################
  ### first run over and take statistics of the residuals time-series
  
  # load( paste0(input_dir, 'df_', var_i, '_deseason' ,'_full.RData'  ) ) # original
  load( paste0(input_dir, 'df_', var_i, '_deseason_full.RData'  ) ) # head(df_var) # dim(df_var)
  
  # MP Can probably remove this or move to separate anamaly checking script
  ### create a kndvi df with z_scores of kndvi anomalies
  ### and save it and a df of count of non-na values
  if(var_i =='kndvi' & b_stats_outliers){
    # create a df of kndvi anomalies with z_scores 
    df_z_scores <- f_z_scores(df_var, var_i)
    # head(df_z_scores) ; summary(df_z_scores)
    # save df of z_scores
    save(df_z_scores, file=paste0(output_path, 'df_', var_i, '_deseason_full_z_scores.RData'  ) )
    # create a df of count of non-na datapoints at each pixel of kndvi anomalies
    df_count_non_na <- df_var %>% dplyr::group_by( x, y ) %>% dplyr::summarise(non_na_count = sum(!is.na(!!as.symbol(var_i))))
  }
  
  ### can prob wrap up in function
  ### if var_i is kndvi and if outliers masking option is active, 
  ### create a df with binary index whether kndvi value is below
  ### a defined threshold and mask kndvi accordingly
  if(var_i =='kndvi' & b_mask_outliers){
    # create the outliers dataframe
    df_outliers <- f_find_outliers(df_var, var_i, outlier_threshold)
    # merge with the anomalies df
    df_var <- left_join(df_var, df_outliers)
    # mask the df if value is outlier
    df_var <- df_var %>% mutate(var_outlier = ifelse(is_outlier == 1, NaN, !!as.symbol(var_i)))
    # create a parallel df to plot z_scores masked for outliers
    df_z_scores_mo <- df_var
    # retain relevant column
    df_var <- df_var[,c("x", "y", "date", "var_outlier")]
    # change column name
    names(df_var)[4] <- var_i
  }

  # MP Can probably remove this or move to separate anamaly checking script
  ### create a df with z_scores of kndvi anomalies
  ### after masking them for outliers and save it and a df of count of non-na values after masking outliers
  if(var_i =='kndvi' & b_stats_outliers){
    # create a df of kndvi anomalies with z_scores masked for outliers
    df_z_scores_mo <- df_z_scores_mo %>% filter(!is.na(var_outlier))
    df_z_scores_mo <- df_z_scores_mo[,c("x", "y", "date", "SD", "z_score")]
    # save df of z_scores
    save(df_z_scores_mo, file=paste0(output_path, 'df_', var_i, '_deseason_full_z_scores_mo.RData'  ) )
    # create a df of count of non-na datapoints at each pixel of kndvi anomalies after masking outliers
    df_count_non_na_mo <- df_var %>% dplyr::group_by( x, y ) %>% dplyr::summarise(non_na_count_mo = sum(!is.na(!!as.symbol(var_i))))
  
    ### if var_i is kndvi and if stats outliers option is active, 
    ### create a df with % of data removed
    ### after masking outliers and save it  
    # create a df of % data removed 
    df_perc_data_removed <- f_perc_data_removed(df_count_non_na, df_count_non_na_mo)
    # save df of percentage data removed
    save(df_perc_data_removed, file=paste0(output_path, 'df_', var_i, '_deseason_full_perc_data_removed.RData'  ) )
    rm(df_z_scores_mo, df_z_scores)
  }
  
  ### if growing season masking option is active, mask dataframe of time-series of variable and variable residuals for growing season
  if(b_mask_gs){
    
    df_var <- f_mask_gs(df_var, phenology_df, var_i)
    if(b_save_intermediate_df) save(df_var, file=paste0(output_path, 'df_', var_i, '_deseason_full_gs_masked.RData'  ) )
    # keep only relevant columns and rename detrended with var_i name
    df_var <- df_var[,c("x", "y", "date", "var_gs")]
    names(df_var)[4] <- var_i
    
  }
  
  ### if detrending option is active, detrend the time-series of variable residuals with linear regression
  if(b_detrend){
    
    df_var <- f_detrend(df_var, var_i)
    if(b_save_intermediate_df) {save(df_var, file=paste0(output_path, 'df_', var_i, '_deseason_detrended.RData' )    )}
    # keep only relevant columns and rename var_gs with var_i name
    df_var <- df_var[,c("x", "y", "date", "detrended")]
    names(df_var)[4] <- var_i
    
  } 
  
  # clean 
  df_var <- na.omit(df_var) # ; dim(df_var) # names(df_var)[4] <- var_i
  
  ###################################################
  ###### extract statistics: TAC & RESID VAR    #####
  ###################################################
  
  # calculate the temporal autocorrelation using designated function
  # first shift create a df of shifted indices
  df_shifted   <- f_index_shift(df_var, v_dates, var_i) # first create a df of lag-1 shifted values
  
  # calculate the TAC via:
  # original method (correlation)
  df_stats_tac <- f_calc_tac(df_shifted, var_i)
  
  # Alternative TAC method for kNDVI - via lm model

  if(var_i =='kndvi'){

    # Apply the function to estimate TAC, var(residuals) and LAMBDA values
    # all calculated via linear model construction
    # to each pixel (x,y combination)
    df_lm_TAC <- df_shifted %>%
      group_by(x, y) %>%           # filter(x >1.01 & x < 1.05 & y > 41.4 & y < 41.42 ) %>% # test with single pixel
      do(f_calc_lm_tac(., b_robust = F ))
    
    # count total number of values in timeseries
    # this is not the total number of kndvi points in timeseries but the number of data points taht can be used in the fit
    # i.e. the number of consecutive measurements of kndvi (not necessarily in the same sequence)
    df_n_ts    <- data.frame(df_shifted) %>%  count(x, y, name = 'n_ts_entries')
    df_shifted <- full_join(df_shifted, df_n_ts)
    
    # rerun with robust lm model - to see if more robust to outliers
    df_lm_TAC_robust <- df_shifted %>% filter( n_ts_entries > 3) %>%
      group_by(x, y) %>%
      do(f_calc_lm_tac(.)) #, b_robust = T ))
    head(df_lm_TAC_robust)
    
    # save(df_lm_TAC_robust, file=paste0(output_path, 'df_', var_i, '_df_lm_TAC_robust.RData' )    )
    colnames(df_lm_TAC_robust)[3:length(df_lm_TAC_robust)] <- paste0('rob_', colnames(df_lm_TAC_robust)[3:length(df_lm_TAC_robust)])   # paste(colnames(df_stats)[3:length(df_stats)], var_name, sep="_")
      
    # take also the sd/variance of the residuals
    df_stats_var <- df_var %>% dplyr::group_by( x, y ) %>%
      dplyr::summarise(
        VAR_resid = var(!!as.symbol(var_i), na.rm = T),
        SD_resid  = sd( !!as.symbol(var_i), na.rm = T)
      )
    
  
    df_stats_tac <- full_join( df_stats_tac, df_n_ts)
    df_stats_tac <- full_join( df_stats_tac, df_lm_TAC)
    df_stats_tac <- full_join( df_stats_tac, df_lm_TAC_robust)
    df_stats_tac <- full_join( df_stats_tac, df_stats_var)
    
  } #end alt TAC method for kndvi
  
  rm(df_var)  
  
  ###################################################
  ###### FULL VALUES: PRE-PROCESSING AND TESTING  #####
  ###################################################
  ### now run over and take statistics of the values time-series
  
  # open df
  load( paste0(input_dir, 'df_', var_i, '_baseVar_full.RData'  ) ) # head(df_var) # dim(df_var)

  ### if var_i is kndvi and if outliers masking option is active, 
  ### create a df with binary index whether kndvi value is below
  ### a defined threshold and mask kndvi accordingly
  if(var_i =='kndvi' & b_mask_outliers){
    # merge with the raw values df
    df_var <- left_join(df_var, df_outliers)
    # mask the df if value is outlier
    df_var <- df_var %>% mutate(var_outlier = ifelse(is_outlier == 1, NaN, !!as.symbol(var_i)))
    # retain relevant column
    df_var <- df_var[,c("x", "y", "date", "var_outlier")]
    # change column name
    names(df_var)[4] <- var_i
  }
    
  ### if growing season masking option is active, mask dataframe of time-series of variable and variable residuals for growing season
  if(b_mask_gs){
    df_var <- f_mask_gs(df_var, phenology_df, var_i)
    if(b_save_intermediate_df) save(df_var, file=paste0(output_path, 'df_', var_i, '_baseVar_full_gs_masked.RData' )    )
    # keep only relevant columns and rename var_gs with var_i name
    df_var <- df_var[, c("x", "y", "date", "var_gs")]
    names(df_var)[4] <- var_i
  }
  
  # clean 
  df_var <- na.omit(df_var) # ; dim(df_var) # names(df_var)[4] <- var_i
  
  ###################################################
  ###### extract statistics: FULL VAL MEAN SD   #####
  ###################################################
  
  # group by x,y and calculate count, mean and coefficient of variation for that pixel
  df_stats <- df_var %>% dplyr::group_by( x, y ) %>%
    dplyr::summarise(
      n = n() ,
      mean  = mean( !!as.symbol(var_i), na.rm = T),
      SD  = sd( !!as.symbol(var_i), na.rm = T),
      CV  = raster::cv( !!as.symbol(var_i), na.rm = T),
      # cv_calc_var = sd_var/mu_var *100    # calculate by hand as check
    )
  # dim(df_stats) ; summary(df_stats) ; head(df_stats)  
  
  ### join the statistics of the variable values with the autocorrelation of the residuals
  df_stats <- full_join(df_stats, df_stats_tac)
  # head(df_stats) ; dim(df_stats) ; summary(df_stats)
  
  # save individual dataframes and join to single dataframe
  save(df_stats, file=paste0(output_path, 'df_', var_i , '_muTACcov.RData' )    ) # df_stats
  
  # output timings
  f_time_update(t_start_time)
  
  
} # end var loop

# output timings
print('end of script')
# output timings
f_time_update(t_start_time)

#########
## END ##
#########

