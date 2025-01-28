# ########################################################
# Title         : f_phenology.R
# Description   : mask dataframes according to a phenology 
#                 
#                 
# Date          : 20/06/23
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################

###################################################
######     MASK FOR PHENOLOGY                  #####
###################################################

# function takes a df and a phenology df, check if the date at each pixel in the df are within the growing
# season (greenup and dormancy) in the phenology df and return a new dataframe with an additional column
# retaining the value the date is within the growing season
# GS check by 1) join input and phenology dfs, 2) convert date to correct format
# 3) check if the greenup occurs before dormancy (usual case) 
# 4) check the date is within GS bounded by greenup->dormancy if greenup before dormancy
#    or check the date is outside the bounds if greenup is after dormancy (i.e. inside dormancy->greenup bound)
# 5) create a value for the greenup
f_mask_gs <- function(df_i, phenology_df, var_name) {

  # join greenup and dormancy DOY to the df
  df_i <- inner_join(df_i, phenology_df)
  
  # create new columns with date in doy and checking if doy_greenup>doy_dormancy and if date is between greenup and dormancy
  df_i <- df_i %>% 
    mutate(doy_date            = as.numeric(format((as.Date(date, format = "%Y-%m-%d")), format = "%j"))) %>% 
    mutate(greenup_gt_dormancy = ifelse(doy_greenup < doy_dormancy,1,0)) %>% 
    mutate(date_within_gs      = ifelse( (  (greenup_gt_dormancy==1 & (doy_date>=doy_greenup & doy_date<=doy_dormancy)) |
                                            (greenup_gt_dormancy==0 & (doy_date>=doy_greenup | doy_date<=doy_dormancy))), 1 , 0)) %>%
    mutate(var_gs              = ifelse(date_within_gs == 1, !!as.symbol(var_name), NA)) # instead of masking make a new column
  
  # keep only relevant columns
  # df_i <- df_i[,c(1:8, 11)]
  
  return(df_i)
}

###################################################
######     APPLY LINEAR DETRENDING            #####
###################################################

# function takes a df of deseasonalised time-series and apply a linear detrending
f_detrend <- function(df_i, var_i) {
  
  df_i <- df_i %>% dplyr::group_by( x, y ) %>%
    filter(any(!is.na( !!as.symbol(var_i)))) %>% # remove all x,y groups where all values are na at all timesteps
    mutate(fitted = fitted.values(lm( !!as.symbol(var_i)~as.Date(date, format = "%Y-%m-%d"), na.action='na.exclude'))) # na.exclude allows to retain in the final df the na values
  df_i <- df_i %>% mutate(detrended = !!as.symbol(var_i)-fitted)
  
  return(df_i)
}

# # function takes a df of deseasonalised time-series and apply a linear detrending and return 
# # the coefficients of the linear regression
# detrend_coeff <- function(df_i) {
#   
#   df_coefficients <- df_var %>% dplyr::group_by( x, y ) %>%
#     filter(any(!is.na(var_gs))) %>%
#     mutate(coeff = coefficients(lm( var_gs~as.Date(date, format = "%Y-%m-%d"), na.action='na.exclude'))[2])
#   
#   # create dataframe with only unique pair of x,y,coefficients
#   df_coefficients <- df_coefficients[!duplicated(df_coefficients[ , c("x", "y")]), ] #https://statisticsglobe.com/unique-rows-of-data-frame-based-on-selected-columns-in-r
#   df_coefficients <- df_coefficients[,c(1:2, 10)]
#   
#   return(df_coefficients)
# }