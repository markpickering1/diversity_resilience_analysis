# ########################################################
# Title         : f_statistical_functions.R
# Description   : statistical functions to apply to R dataframes
#                 
#                 
# Date          : 21/05/23
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################

###################################################
######     CALCULATE TEMPORAL AUTOCORRELATION #####
###################################################

# function takes a df, df_i, with x,y pixels, and values of column var_name for each date
# creates a copy of the index of these dates shifted by 1 index
f_index_shift <- function(df_i, v_dates, var_name ){
  
  # take index of dates: clone and shift by one index
  df_dates_t1 <- data.frame(v_dates, 1:length(v_dates) ) ; names(df_dates_t1)[2] <- 'index'
  names(df_dates_t1) <- c('date', 't_i')
  df_dates_t2 <- data.frame(v_dates, 2:(length(v_dates) + 1 ) )
  names(df_dates_t2) <- c('date', 't_i')
  # head(df_dates_t1) ; tail(df_dates_t1) ; head(df_dates_t2) ; tail(df_dates_t2)
  
  # create two dfs of var_name with shifted index and values
  df_t1 <-  left_join(df_i, df_dates_t1)
  df_t2 <-  left_join(df_i, df_dates_t2)
  # names(df_t1)[4] <- paste0(var_name, '_t1') ; names(df_t2)[4] <- paste0(var_name, '_t2')
  names(df_t1)[4] <- 'value_t1' ; names(df_t2)[4] <- 'value_t2'
  # df_t1$date <- NULL ; 
  df_t2$date <- NULL
  
  # join the shifted
  df_o <- full_join(df_t1, df_t2)
  df_o <- na.omit(df_o)
  
  # calculate the 1-step lag
  # df_o <- df_o %>% dplyr::group_by( x, y ) %>%
    # dplyr::summarise( TAC = cor( !!as.symbol(paste0(var_name, '_t1')) , !!as.symbol(paste0(var_name, '_t2') )) )
  
  return(df_o)
}

# calculates the autocorrelation of the var_name value shifted by 1 index
f_calc_tac <- function(df_i, var_name ){
  # calculate the 1-step lag via correlation
  df_o <- df_i %>% dplyr::group_by( x, y ) %>%
    dplyr::summarise( TAC = cor( !!as.symbol(paste0('value_t1')) , !!as.symbol(paste0('value_t2') )) )
  
  # df_o <- df_i %>% dplyr::group_by( x, y ) %>%
  #   dplyr::summarise( TAC = cor( !!as.symbol(paste0(var_name, '_t1')) , !!as.symbol(paste0(var_name, '_t2') )) )
  return(df_o)
  
}

# take a dataframe with two offset time-series value_t1 and value_t2. 
# two methods assess the autocorrelation via regression: 
# 1) fitting dx between timesteps (ref. kappa) 2) fitting t2 ~ t1 (ref. xt)
# I guess the output should be the same - check
# Get the difference between the timeseries dx = t2-t1
# fit a linear model (either robust or basic) for t2 (to calculate TAC as slope_xt) 
# and dx (for later calculating variance of driving noise )
f_calc_lm_tac <- function(df_i,  b_robust = T ){
  
  # alternative TAC method: calculate difference between two points in timeseries
  df_i <- df_i %>% mutate(
    dx = !!as.symbol(paste0('value_t2')) - !!as.symbol(paste0('value_t1')) )
  
  if (b_robust) {
    fit_kappa <- robustbase::lmrob(dx ~ value_t1, data = df_i)
    fit_xt    <- robustbase::lmrob(value_t2 ~ value_t1, data = df_i)
  } else {
    fit_kappa <- lm(dx ~ value_t1, data = df_i)
    fit_xt    <- lm(value_t2 ~ value_t1, data = df_i)
  }

  slope_kappa <- coef(fit_kappa)[2] # select the slope coefficient
  slope_xt    <- coef(fit_xt)[2]    # select the slope coefficient

  # convert the coef to lambda
  lambda_kappa <- log(slope_kappa + 1)
  lambda_xt    <- log(slope_xt)
  
  # Compute residuals and sigma for kappa
  sigma_kappa <- sd( residuals(fit_kappa) )
  
  # Compute residuals and sigma for xt
  sigma_xt <- sd( residuals(fit_xt) )
  
  # Now compute lambda from variance
  lt_var <- var(df_i$value_t1, na.rm = TRUE)
  ex <- 1 - (sigma_xt^2 / lt_var)
  lambda_variance <- log(ex) * 0.5

  
  # return( data.frame(slope_xt))
  return(  data.frame(slope_kappa, slope_xt, lambda_kappa, lambda_xt, sigma_kappa, sigma_xt, lt_var, lambda_variance) )
}

###################################################
######    CALCULATE Z-SCORES                  #####
###################################################

f_z_scores <- function(df_i, var_name) {
  
  # group by x,y and calculate std
  df_sd <- df_i %>% dplyr::group_by( x, y ) %>% dplyr::summarise(SD=sd(!!as.symbol(var_name), na.rm = T))
  
  # remove na (is this necessary?)
  #df_i <- na.omit(df_i)
  
  # join sd to original df
  df_i <- left_join(df_i, df_sd)
  
  # add z_score as the kndvi anomaly at a pixel at a timestep divided by the sd of the kndvi anomalies in that pixel
  df_i <- df_i %>% mutate(z_score=!!as.symbol(var_name)/SD)
  
  # return the outliers df
  return(df_i)
}

###################################################
######    IDENTIFY OUTLIERS                   #####
###################################################

f_find_outliers <- function(df_i, var_name, z_threshold) {
  
  # run function for finding z-score
  df_i <- f_z_scores(df_i, var_name)
  
  # add column is_outlier equal 1 if the z_score is below the threshold
  df_i <- df_i %>% mutate(is_outlier = ifelse(z_score <= z_threshold, 1, 0))
  
  
  # define the outliers df
  df_outliers <- df_i[, c("x", "y", "date", "SD", "z_score", "is_outlier")]
  
  # return the outliers df
  return(df_outliers)
}

###################################################
######    CALCULATE % OF DATA REMOVED         #####
###################################################

f_perc_data_removed <- function(count_na, count_na_mo) {
  
  # join the two counts datasets
  df_perc_data_removed <- left_join(count_na, count_na_mo)
  
  # calc % of data removed as outliers of the original total non-na values
  df_perc_data_removed <- df_perc_data_removed %>% mutate(perc_data_removed = (((non_na_count - non_na_count_mo)/non_na_count)*100)) 
  
  # return the outliers df
  return(df_perc_data_removed)
}

###################################################
######     MASK FOR PHENOLOGY                 #####
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

