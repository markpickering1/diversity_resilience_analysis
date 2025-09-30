# ########################################################
# Title         : input_plotByBGR.R
# Description   : This text script acts as a user input to all the scripts plotting 1d PDPs by bgr
#                 By setting variables in this file, the user should not need to edit the main code
#                 this script does not contain plotting or styles themes: see initialse_figs.R
# Date          : 23/09/25
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################

###################################################
######     I/O                                #####
###################################################

# set output path name - with date this will link output (variables) to next script
script_output_ext <- 'plotByBGR'                           # new name

# input dataset containing the dataframes used for training the rf models and the respective rf models
input_dir_rf <- 'createRF_model/'
input_all_data <- 'df_all.RData'
input_rf_common_name <- 'list_rf_model_pdp_results_boot_parallel_nIter-20_div-'

# input file containing the BGR
input_bgr <- 'ancillary/EEA_biogeographic_regions/europe/EEA_biogeographical_regions_df/df_bgr_baseVar_full_merged_bgr.RData'   #  with continental, atlantic, steppe and pannonian bgr (7, 4, 12, 11) merged in continental (7) df_var$BiogeoRegions2016 <- ifelse(df_var$BiogeoRegions2016 %in% c(4, 7, 11, 12), 7, df_var$BiogeoRegions2016)

# identify unique KG classes to loop over
bgr <- c(1, 7, 9)

# identify bgr classes for plots
colors <- c("#D55E00", "#5BB99F","#F0C442")
regions <- c("alpine", "temperate", "mediterranean")

#####################################################
###### SELECT VARIABLES OF INTEREST IN ANALYSIS #####
#####################################################
v_target <- c('kndvi_lambda_xt') #,'kndvi_lambda_variance')
v_identifiers <- c('x', 'y')

# variables to go as predictor in every model
v_predictors <- c( 'kndvi_mean', 
                   'socc30cm', 
                   'forestcover', 
                   'topology_elevation_std', 
                   'ssr_mean', 'ssr_CV', 'ssr_TAC',
                   't2m_mean', 't2m_CV', 't2m_TAC', 
                   'tp_mean', 'tp_CV', 'tp_TAC',
                   'VPD_mean', 'VPD_CV', 'VPD_TAC', 'Ndep'
)

# add biodiversity variables to loop over and add to separate models
# v_optional_predictors <- c("mu_kurt", "sd_rh98", "shannon_entropy")
v_optional_predictors <- c("Kurtosis") #, "Canopy_heights_sd", "Shannon")

t2m_mean_pdp <- FALSE

stats <- 'median'

l_seed <- c(99) #c(98, 99, 100, 101, 102)

# for paralellisations number of cores to use
n_cores <- 15

###################################################
######     SET RF PARAMETERS                  #####
###################################################
# need to do the test train split by each diversity group and removing NAs

# only use the full dataframe for all variables
b_completeCases_for_fullDF <- T

# choose which dataset to use in pdp figures - training recommended, though good to check similarity with test
s_train_test_all <- 'train' # test, # all
  
#######################################
##### SET MAP/HIST LIMITS         #####
#######################################
# set the hist and map min/max
# probably better to set as universal values rather than dynamically coding them

## default limits for those not set
# set as a percentage of the min max span
l_h_default  <-  0.02   # default histogram percentile
l_m_default  <-  0.15   # default map percentile

#######################################
##### SET MAP/HIST LIMITS KNDVI   #####
#######################################

## fixed limits for time-series metrics
kndvi_label <- 'kNDVI' # kndvi
# histogram limits               # map limits                    # units                          # rescale factor
l_kndvi_n_hist   <- c(0, 874 ) ; l_kndvi_n_map   <- c(0, 874 ) ; l_kndvi_n_unit  <- '# entries' ; l_kndvi_n_rescale   <- NA
l_kndvi_mu_hist  <- c(0, 0.6 ) ; l_kndvi_mu_map  <- c(0.2, 0.5 ); l_kndvi_mu_unit <- 'mean'      ; l_kndvi_mu_rescale  <- NA
l_kndvi_SD_hist  <- c(0, 0.25) ; l_kndvi_SD_map  <- c(0, 0.2 ) ; l_kndvi_SD_unit <- 'S.D.'      ; l_kndvi_SD_rescale  <- NA
l_kndvi_CV_hist  <- c(0, 150 ) ; l_kndvi_CV_map  <- c(0, 60 )  ; l_kndvi_CV_unit <- 'C.V.'      ; l_kndvi_CV_rescale  <- NA
# alt res metrics
l_kndvi_TAC_hist       <- c(-0.1, 1) ; l_kndvi_TAC_map       <- c(0, 0.6 )   ; l_kndvi_TAC_unit       <- 'TAC'              ; l_kndvi_TAC_rescale       <- NA
l_kndvi_VAR_resid_hist <- c(0, 0.01) ; l_kndvi_VAR_resid_map <- c(0, 0.005 ) ; l_kndvi_VAR_resid_unit <- 'var[resid]'       ; l_kndvi_VAR_resid_rescale <- NA
l_kndvi_SD_resid_hist  <- c(0, 0.15) ; l_kndvi_SD_resid_map  <- c(0.03, 0.06); l_kndvi_SD_resid_unit  <- 'sd[resid]'        ; l_kndvi_SD_resid_rescale  <- NA
l_kndvi_lambda_kappa_hist    <- c(-4, 0.1) ; l_kndvi_lambda_kappa_map    <- c(-2.5, 0 )  ; l_kndvi_lambda_kappa_unit    <- 'lambda kappa'     ; l_kndvi_lambda_kappa_rescale     <- NA
l_kndvi_lambda_xt_hist       <- c(-4, 0.1) ; l_kndvi_lambda_xt_map       <- c(-2.5, 0 )  ; l_kndvi_lambda_xt_unit       <- 'lambda xt'        ; l_kndvi_lambda_xt_rescale        <- NA
l_kndvi_lambda_variance_hist <- c(-4, 0.1) ; l_kndvi_lambda_variance_map <- c(-2.5, 0 )  ; l_kndvi_lambda_variance_unit <- 'lambda var'       ; l_kndvi_lambda_variance_rescale  <- NA
# slope and input metrics
l_kndvi_n_ts_entries_hist <- c(0, 800) ; l_kndvi_n_ts_entries_map  <- c(0 , 500 ); l_kndvi_n_ts_entries_unit  <- '# entries'  ; l_kndvi_n_ts_entries_rescale <- NA
l_kndvi_slope_kappa_hist  <- c(-0.1, 1); l_kndvi_slope_kappa_map   <- c(0, 0.6 ) ; l_kndvi_slope_kappa_unit   <- 'slope kappa'; l_kndvi_slope_kappa_rescale  <- NA
l_kndvi_slope_xt_hist     <- c(-0.1, 1); l_kndvi_slope_xt_map      <- c(0, 0.6 ) ; l_kndvi_slope_xt_unit      <- 'slope xt'   ; l_kndvi_slope_xt_rescale     <- NA
l_kndvi_rob_slope_xt_hist <- c(-0.1, 1); l_kndvi_rob_slope_xt_map  <- c(0, 0.6 ) ; l_kndvi_rob_slope_xt_unit  <- 'rob slope xt'; l_kndvi_rob_slope_xt_rescale<- NA
l_kndvi_sigma_kappa_hist  <- c(0, 0.2) ; l_kndvi_sigma_kappa_map  <- c(0, 0.08 ) ; l_kndvi_sigma_kappa_unit   <- 'sig kappa'  ; l_kndvi_sigma_kappa_rescale  <- NA
l_kndvi_sigma_xt_hist     <- c(0, 0.2) ; l_kndvi_sigma_xt_map     <- c(0, 0.08 ) ; l_kndvi_sigma_xt_unit      <- 'sig xt'     ; l_kndvi_sigma_xt_rescale     <- NA
l_kndvi_lt_var_hist       <- c(0, 0.01); l_kndvi_lt_var_map       <- c(0, 0.005 ); l_kndvi_lt_var_unit        <- 'LT var'     ; l_kndvi_lt_var_rescale <- NA
# rob res metrics
l_kndvi_rob_lambda_kappa_hist    <- c(-4, 0.1) ; l_kndvi_rob_lambda_kappa_map    <- c(-2.5, 0 )  ; l_kndvi_rob_lambda_kappa_unit    <- 'rob lambda kappa'   ; l_kndvi_rob_lambda_kappa_rescale <- NA
l_kndvi_rob_lambda_xt_hist       <- c(-4, 0.1) ; l_kndvi_rob_lambda_xt_map       <- c(-2.5, 0 )  ; l_kndvi_rob_lambda_xt_unit       <- 'rob lambda xt'      ; l_kndvi_rob_lambda_xt_rescale    <- NA
l_kndvi_rob_lambda_variance_hist <- c(-4, 0.1) ; l_kndvi_rob_lambda_variance_map <- c(-2.5, 0 )  ; l_kndvi_rob_lambda_variance_unit <- 'rob lambda var'     ; l_kndvi_rob_lambda_variance_rescale  <- NA



# Create a named list of the selected parameters - first entry is hist second entry is map
# time-series datasets
l_kndvi <- list(  label = kndvi_label,
                  n     = list(l_kndvi_n_hist,   l_kndvi_n_map,   l_kndvi_n_unit  , l_kndvi_n_rescale  ) ,
                  mean  = list(l_kndvi_mu_hist,  l_kndvi_mu_map,  l_kndvi_mu_unit , l_kndvi_mu_rescale ) ,
                  SD    = list(l_kndvi_SD_hist,  l_kndvi_SD_map,  l_kndvi_SD_unit , l_kndvi_SD_rescale ) ,
                  CV    = list(l_kndvi_CV_hist,  l_kndvi_CV_map,  l_kndvi_CV_unit , l_kndvi_CV_rescale ) ,
                  TAC   = list(l_kndvi_TAC_hist, l_kndvi_TAC_map, l_kndvi_TAC_unit, l_kndvi_TAC_rescale) ,
                  # alt res metrics
                  VAR_resid   = list(l_kndvi_VAR_resid_hist , l_kndvi_VAR_resid_map , l_kndvi_VAR_resid_unit , l_kndvi_VAR_resid_rescale )         ,
                  SD_resid    = list(l_kndvi_SD_resid_hist  , l_kndvi_SD_resid_map  , l_kndvi_SD_resid_unit  , l_kndvi_SD_resid_rescale  )         ,
                  lambda_kappa    = list(l_kndvi_lambda_kappa_hist    , l_kndvi_lambda_kappa_map    , l_kndvi_lambda_kappa_unit    , l_kndvi_lambda_kappa_rescale )    ,
                  lambda_xt       = list(l_kndvi_lambda_xt_hist       , l_kndvi_lambda_xt_map       , l_kndvi_lambda_xt_unit       , l_kndvi_lambda_xt_rescale )       ,
                  lambda_variance = list(l_kndvi_lambda_variance_hist , l_kndvi_lambda_variance_map , l_kndvi_lambda_variance_unit , l_kndvi_lambda_variance_rescale ) ,
                  # slope and input metrics
                  n_ts_entries = list(l_kndvi_n_ts_entries_hist , l_kndvi_n_ts_entries_map , l_kndvi_n_ts_entries_unit , l_kndvi_n_ts_entries_rescale )   ,
                  slope_kappa  = list(l_kndvi_slope_kappa_hist  , l_kndvi_slope_kappa_map  , l_kndvi_slope_kappa_unit  , l_kndvi_slope_kappa_rescale  )   ,
                  slope_xt     = list(l_kndvi_slope_xt_hist     , l_kndvi_slope_xt_map     , l_kndvi_slope_xt_unit     , l_kndvi_slope_xt_rescale )       ,
                  sigma_kappa  = list(l_kndvi_sigma_kappa_hist  , l_kndvi_sigma_kappa_map  , l_kndvi_sigma_kappa_unit  , l_kndvi_sigma_kappa_rescale )    ,
                  sigma_xt     = list(l_kndvi_sigma_xt_hist     , l_kndvi_sigma_xt_map     , l_kndvi_sigma_xt_unit     , l_kndvi_sigma_xt_rescale )       ,
                  lt_var       = list(l_kndvi_lt_var_hist       , l_kndvi_lt_var_map       , l_kndvi_lt_var_unit       , l_kndvi_lt_var_rescale )         ,
                  # rob res metrics
                  rob_lambda_kappa    = list(l_kndvi_rob_lambda_kappa_hist    , l_kndvi_rob_lambda_kappa_map    , l_kndvi_rob_lambda_kappa_unit    , l_kndvi_rob_lambda_kappa_rescale )    ,
                  rob_lambda_xt       = list(l_kndvi_rob_lambda_xt_hist       , l_kndvi_rob_lambda_xt_map       , l_kndvi_rob_lambda_xt_unit       , l_kndvi_rob_lambda_xt_rescale )       ,
                  rob_lambda_variance = list(l_kndvi_rob_lambda_variance_hist , l_kndvi_rob_lambda_variance_map , l_kndvi_rob_lambda_variance_unit , l_kndvi_rob_lambda_variance_rescale ) 
                  
)

#######################################
##### SET MAP/HIST LIMITS climate #####
#######################################

t2m_label <- '2m temperature' # t2m
# head_df_comb_var['tp_mean'] <- eval(f_rescale, head_df_comb_var['tp_mean'] )
l_t2m_n_hist   <- c(0, 874 )   ; l_t2m_n_map   <- c(0, 874 )  ; l_t2m_n_unit  <- '# entries'    ; l_t2m_n_rescale    <- NA
# Kelvin old l_t2m_mu_hist  <- c(250, 300 ) ; l_t2m_mu_map  <- c(275, 295 ); l_t2m_mu_unit <- 'mean [C]'     ; l_t2m_mu_rescale   <- parse( text = "t2m_mean -273.15" )  # rescaling factor to celcius
l_t2m_mu_hist  <- c(-25, 26 )  ; l_t2m_mu_map  <- c(0, 22 )   ; l_t2m_mu_unit <- 'mean [C]'     ; l_t2m_mu_rescale   <- parse( text = "t2m_mean -273.15" )  # rescaling factor K to C
l_t2m_SD_hist  <- c(0, 14)     ; l_t2m_SD_map  <- c(5, 11)    ; l_t2m_SD_unit <- 'S.D. [C]'     ; l_t2m_SD_rescale   <- NA
l_t2m_CV_hist  <- c(0, 5)      ; l_t2m_CV_map  <- c(1, 4)     ; l_t2m_CV_unit <- 'C.V.'         ; l_t2m_CV_rescale   <- NA
l_t2m_TAC_hist <- c(-0.1, 1)   ; l_t2m_TAC_map <- c(0.2, 0.5) ; l_t2m_TAC_unit<- 'TAC'          ; l_t2m_TAC_rescale  <- NA

VPD_label <- 'Vapour pressure deficit' # VPD
l_VPD_n_hist   <- c(0, 874 )   ; l_VPD_n_map   <- c(0, 874 )  ; l_VPD_n_unit  <- '# entries'    ; l_VPD_n_rescale   <- NA    
l_VPD_mu_hist  <- c(0, 3 )     ; l_VPD_mu_map  <- c(0, 1 )    ; l_VPD_mu_unit <- 'mean [kPa]'   ; l_VPD_mu_rescale  <- NA
l_VPD_SD_hist  <- c(0, 2.5)    ; l_VPD_SD_map  <- c(0, 0.6)   ; l_VPD_SD_unit <- 'S.D. [kPa]'   ; l_VPD_SD_rescale  <- NA
l_VPD_CV_hist  <- c(0, 120)    ; l_VPD_CV_map  <- c(30, 110)  ; l_VPD_CV_unit <- 'C.V.'         ; l_VPD_CV_rescale  <- NA
l_VPD_TAC_hist <- c(-0.1, 1)   ; l_VPD_TAC_map <- c(0.2, 0.6) ; l_VPD_TAC_unit<- 'TAC'          ; l_VPD_TAC_rescale <- NA

ssr_label <- 'Surface net solar radiation' # ssr
l_ssr_n_hist   <- c(0, 874 )    ; l_ssr_n_map   <- c(0, 874 )    ; l_ssr_n_unit  <- '# entries'   ; l_ssr_n_rescale    <- NA       
# old sum l_ssr_mu_hist  <- c(0, 1.2e+07) ; l_ssr_mu_map  <- c(5e+06,1e+07); l_ssr_mu_unit <- 'mean [W/m2]' ; l_ssr_mu_rescale   <- NA
l_ssr_mu_hist  <- c(0, 200)     ; l_ssr_mu_map  <- c(50,200)     ; l_ssr_mu_unit <- 'mean [W/m2]' ; l_ssr_mu_rescale   <- parse( text = "ssr_mean/(24*60*60)") # rescale from Joules per day per m2 to flux
l_ssr_SD_hist  <- c(0, 100)     ; l_ssr_SD_map  <- c(50, 90)     ; l_ssr_SD_unit <- 'S.D. [W/m2]'; l_ssr_SD_rescale    <- parse( text = "ssr_SD/(24*60*60)")  # could also convert from base units 
l_ssr_CV_hist  <- c(0, 120)     ; l_ssr_CV_map  <- c(30, 100)    ; l_ssr_CV_unit <- 'C.V.'        ; l_ssr_CV_rescale   <- NA  
l_ssr_TAC_hist <- c(-0.1, 1)    ; l_ssr_TAC_map <- c(0.1, 0.3)   ; l_ssr_TAC_unit<- 'TAC'         ; l_ssr_TAC_rescale  <- NA 


tp_label <- 'Total precipitation' # tp
l_tp_n_hist   <- c(0, 874 )   ; l_tp_n_map   <- c(0, 874 )  ; l_tp_n_unit  <- '# entries'        ; l_tp_n_rescale    <- NA       
l_tp_mu_hist  <- c(0, 3200)   ; l_tp_mu_map  <- c(500, 1200); l_tp_mu_unit <- 'mean annual [mm]' ; l_tp_mu_rescale   <- parse( text = "tp_mean*365.25") # rescale to annual total
# keeping original units - mm/day (rough limts as was overestimate)
# l_tp_mu_hist  <- c(0, 1)      ; l_tp_mu_map  <- c(0, 0.4)   ; l_tp_mu_unit <- 'daily mean [mm]'; l_tp_mu_rescale   <- parse( text = "tp_mean*365.25") # rescale to annual total   
l_tp_SD_hist  <- c(0, 5)      ; l_tp_SD_map  <- c(1, 3)     ; l_tp_SD_unit <- 'S.D. (daily) [mm]'; l_tp_SD_rescale   <- NA   
l_tp_CV_hist  <- c(0,200)     ; l_tp_CV_map  <- c(60, 140)  ; l_tp_CV_unit <- 'C.V.  (daily)'    ; l_tp_CV_rescale   <- NA   
l_tp_TAC_hist <- c(-0.1, 1)   ; l_tp_TAC_map <- c(0, 0.2)   ; l_tp_TAC_unit<- 'TAC'              ; l_tp_TAC_rescale  <- NA 


l_t2m <- list(    label = t2m_label,
                  n     = list(l_t2m_n_hist,   l_t2m_n_map,   l_t2m_n_unit,   l_t2m_n_rescale  ) ,
                  mean  = list(l_t2m_mu_hist,  l_t2m_mu_map,  l_t2m_mu_unit,  l_t2m_mu_rescale ) ,
                  SD    = list(l_t2m_SD_hist,  l_t2m_SD_map,  l_t2m_SD_unit,  l_t2m_SD_rescale ) ,
                  CV    = list(l_t2m_CV_hist,  l_t2m_CV_map,  l_t2m_CV_unit,  l_t2m_CV_rescale ) ,
                  TAC   = list(l_t2m_TAC_hist, l_t2m_TAC_map, l_t2m_TAC_unit, l_t2m_TAC_rescale) )
l_VPD <- list(    label = VPD_label,
                  n     = list(l_VPD_n_hist,   l_VPD_n_map,   l_VPD_n_unit  , l_VPD_n_rescale  ) ,
                  mean  = list(l_VPD_mu_hist,  l_VPD_mu_map,  l_VPD_mu_unit , l_VPD_mu_rescale ) ,
                  SD    = list(l_VPD_SD_hist,  l_VPD_SD_map,  l_VPD_SD_unit , l_VPD_SD_rescale ) ,
                  CV    = list(l_VPD_CV_hist,  l_VPD_CV_map,  l_VPD_CV_unit , l_VPD_CV_rescale ) ,
                  TAC   = list(l_VPD_TAC_hist, l_VPD_TAC_map, l_VPD_TAC_unit, l_VPD_TAC_rescale) )
l_ssr <- list(    label = ssr_label,
                  n     = list(l_ssr_n_hist,   l_ssr_n_map,   l_ssr_n_unit  , l_ssr_n_rescale  ) ,
                  mean  = list(l_ssr_mu_hist,  l_ssr_mu_map,  l_ssr_mu_unit , l_ssr_mu_rescale ) ,
                  SD    = list(l_ssr_SD_hist,  l_ssr_SD_map,  l_ssr_SD_unit , l_ssr_SD_rescale ) ,
                  CV    = list(l_ssr_CV_hist,  l_ssr_CV_map,  l_ssr_CV_unit , l_ssr_CV_rescale ) ,
                  TAC   = list(l_ssr_TAC_hist, l_ssr_TAC_map, l_ssr_TAC_unit, l_ssr_TAC_rescale) )
l_tp <- list(    label = tp_label,
                 n     = list(l_tp_n_hist,   l_tp_n_map  , l_tp_n_unit     , l_tp_n_rescale  ),
                 mean  = list(l_tp_mu_hist,  l_tp_mu_map , l_tp_mu_unit    , l_tp_mu_rescale ),
                 SD    = list(l_tp_SD_hist,  l_tp_SD_map , l_tp_SD_unit    , l_tp_SD_rescale ),
                 CV    = list(l_tp_CV_hist,  l_tp_CV_map , l_tp_CV_unit    , l_tp_CV_rescale ),
                 TAC   = list(l_tp_TAC_hist, l_tp_TAC_map, l_tp_TAC_unit   , l_kndvi_TAC_rescale) )

#######################################
##### SET MAP/HIST LIMITS STATIC  #####
#######################################

# limits for static metrics
forestcover_label <- 'Forest cover' # forestcover
l_forestcover_hist <- c(-0.05, 1 )   ; l_forestcover_map <- c(0.5, 1 )   ; l_forestcover_unit  <- ''
# l_forestcover_hist <- c(-0.05, 1 )   ; l_forestcover_map <- c(0, 1 )   ; l_forestcover_unit  <- ''  # previous forest definition

socc30cm_label <- 'Soil organic carbon content 30cm' # socc30cm
l_socc30cm_hist <- c(0, 5 )   ; l_socc30cm_map <- c(0, 3 )   ;  l_socc30cm_unit  <- 'Mg/ha'

# KG5 biome
KG5_label   <- 'Climate zone (5)' # KG5
l_KG5_hist  <- c(1, 5 )   ; l_KG5_map <- c(1, 5 )   ;  l_KG5_unit  <- '1 TRO, 2 ARI, 3 TEM, 4 CON'
# KG16 biome
KG16_label  <- 'Climate zone (16)' # KG5
l_KG16_hist <- c(1, 16 )   ; l_KG16_map <- c(1, 16 )   ;  l_KG16_unit  <- ''
# topology_elevation_mean
elevation_mean_label  <- 'Elevation' # topology_elevation_mean
l_elevation_mean_hist <- c(-20, 1980 ) ; l_elevation_mean_map <- c(0, 1500 ) ;  l_elevation_mean_unit  <- 'mean [m]'
# topology_elevation_std
elevation_std_label   <- 'Elevation' # topology_elevation_std
l_elevation_std_hist  <- c(0, 300 )    ; l_elevation_std_map  <- c(0, 100)   ;  l_elevation_std_unit   <- 'S.D. [m]'
# topology_slope_mean
slope_mean_label      <- 'Slope' # topology_slope_std
l_slope_mean_hist     <- c(0, 40  )    ; l_slope_mean_map     <- c(0, 10 )   ;  l_slope_mean_unit  <- 'mean'
# topology_slope_std
slope_std_label       <- 'Slope' # topology_slope_std
l_slope_std_hist      <- c(0, 20 )     ; l_slope_std_map      <- c(0, 10)   ;  l_slope_std_unit  <- 'S.D.'



# static datasets
l_forestcover    <- list(label         = forestcover_label,
                         forestcover  = list(l_forestcover_hist,   l_forestcover_map, l_forestcover_unit)  )
l_socc30cm       <- list(label         = socc30cm_label,
                         socc30cm     = list(l_socc30cm_hist,   l_socc30cm_map, l_socc30cm_unit)  )
l_KG5            <- list(label         = KG5_label,
                         KG5          = list(l_KG5_hist,   l_KG5_map, l_KG5_unit)  )
l_KG16           <- list(label         = KG16_label,
                         KG16         = list(l_KG16_hist,   l_KG16_map, l_KG16_unit)  )
l_elevation_mean <- list(label         = elevation_mean_label,
                         topology_elevation_mean  = list(l_elevation_mean_hist,   l_elevation_mean_map, l_elevation_mean_unit)  )
l_elevation_std  <- list(label         = elevation_std_label,
                         topology_elevation_std  = list(l_elevation_std_hist,   l_elevation_std_map, l_elevation_std_unit)  )
l_slope_mean     <- list(label         = slope_mean_label,
                         topology_slope_mean     = list(l_slope_mean_hist,   l_slope_mean_map, l_slope_mean_unit)  )
l_slope_std      <- list(label         = slope_std_label,
                         topology_slope_std      = list(l_slope_std_hist,   l_slope_std_map, l_slope_std_unit)  )

#######################################
##### SET MAP/HIST LIMITS DIVER   #####
#######################################

# count of GEDI points in each pixel
div_count_label    <- 'GEDI count'          
l_div_count_hist   <- c(0, 500  )      ;   l_div_count_map   <- c(0, 120   )      ; l_div_count_unit  <- ''
l_div_count <- list(    label        = div_count_label,
                        div_count    = list(l_div_count_hist,   l_div_count_map, l_div_count_unit)  )

# relative height of pixel metrics
mu_rh25_label    <- 'RH-25'          # mu_rh25 rh50_mean
l_mu_rh25_hist   <- c(0, 30  )       ;   l_mu_rh25_map   <- c(0, 7   )      ; l_mu_rh25_unit  <- ' [m]'
mu_rh50_label    <- 'RH-50'          # mu_rh50 mu_rh50
l_mu_rh50_hist   <- c(0, 30  )       ;   l_mu_rh50_map   <- c(0, 15  )      ; l_mu_rh50_unit  <- ' [m]'
mu_rh75_label    <- 'RH-75'          # mu_rh75 rh75_mean
l_mu_rh75_hist   <- c(0, 30  )       ;   l_mu_rh75_map   <- c(5, 20  )      ; l_mu_rh75_unit  <- ' [m]'
mu_rh98_label    <- 'RH-98'          # mu_rh98 rh98_mean
l_mu_rh98_hist   <- c(0, 40  )       ;   l_mu_rh98_map   <- c(5, 30  )      ; l_mu_rh98_unit  <- ' [m]'

l_mu_rh25 <- list(    label        = mu_rh25_label,
                      mu_rh25      = list(l_mu_rh25_hist,   l_mu_rh25_map, l_mu_rh25_unit)  )
l_mu_rh50 <- list(    label        = mu_rh50_label,
                      mu_rh50      = list(l_mu_rh50_hist,   l_mu_rh50_map, l_mu_rh50_unit)  )
l_mu_rh75 <- list(    label        = mu_rh75_label,
                      mu_rh75      = list(l_mu_rh75_hist,   l_mu_rh75_map, l_mu_rh75_unit)  )
l_mu_rh98 <- list(    label        = mu_rh98_label,
                      mu_rh98      = list(l_mu_rh98_hist,   l_mu_rh98_map, l_mu_rh98_unit)  )

# relative height (sd)
sd_rh25_label    <- 'RH-25 (SD)'          # sd_rh25 
l_sd_rh25_hist   <- c(0, 15  )       ;   l_sd_rh25_map   <- c(2, 8   )      ; l_sd_rh25_unit  <- ' [m]'
sd_rh50_label    <- 'RH-50 (SD)'          # sd_rh50 
l_sd_rh50_hist   <- c(0, 15  )       ;   l_sd_rh50_map   <- c(2, 8   )      ; l_sd_rh50_unit  <- ' [m]'
sd_rh75_label  <- 'RH-75 (SD)'          # sd_rh75 
l_sd_rh75_hist   <- c(0, 15  )       ;   l_sd_rh75_map   <- c(2, 8   )      ; l_sd_rh75_unit  <- ' [m]'
sd_rh98_label    <- 'RH-98 (SD)'          # sd_rh98 
l_sd_rh98_hist   <- c(0, 15  )       ;   l_sd_rh98_map   <- c(2, 8   )      ; l_sd_rh98_unit  <- ' [m]'

l_sd_rh25 <- list(    label        = sd_rh25_label,
                      sd_rh25      = list(l_sd_rh25_hist,   l_sd_rh25_map, l_sd_rh25_unit)  )
l_sd_rh50 <- list(    label        = sd_rh50_label,
                      sd_rh50      = list(l_sd_rh50_hist,   l_sd_rh50_map, l_sd_rh50_unit)  )
l_sd_rh75 <- list(    label        = sd_rh75_label,
                      sd_rh75      = list(l_sd_rh75_hist,   l_sd_rh75_map, l_sd_rh75_unit)  )
l_sd_rh98 <- list(    label        = sd_rh98_label,
                      sd_rh98      = list(l_sd_rh98_hist,   l_sd_rh98_map, l_sd_rh98_unit)  )

# vertical mean sd cv (first and second moments) and spatial mu sd cv
mu_mean_label    <- 'Vertical mean (mu_mean)'      # mu_mean 
l_mu_mean_hist   <- c(0, 30  )       ;   l_mu_mean_map   <- c(0, 15   )      ; l_mu_mean_unit  <- ' [m]'
mu_sd_label      <- 'Vertical SD (mu_sd)'          # mu_sd 
l_mu_sd_hist     <- c(0, 15  )       ;   l_mu_sd_map     <- c(2, 8   )       ; l_mu_sd_unit    <- ' [m]'
mu_cv_label      <- 'Vertical CV (mu_cv)'          # mu_cv 
l_mu_cv_hist     <- c(0, 1.5 )       ;   l_mu_cv_map     <- c(0.4, 1   )     ; l_mu_cv_unit    <- ''

l_mu_mean <- list(    label        = mu_mean_label,
                      mu_mean      = list(l_mu_mean_hist,   l_mu_mean_map, l_mu_mean_unit)  )
l_mu_sd   <- list(    label        = mu_sd_label,
                      mu_sd        = list(l_mu_sd_hist,   l_mu_sd_map, l_mu_sd_unit)  )
l_mu_cv   <- list(    label        = mu_cv_label,
                      mu_cv        = list(l_mu_cv_hist,   l_mu_cv_map, l_mu_cv_unit)  )


sd_mean_label    <- 'Spat SD of Vert mean (sd_mean)'          # sd_mean 
l_sd_mean_hist   <- c(0, 10  )       ;   l_sd_mean_map   <- c(2, 7   )      ; l_sd_mean_unit  <- ' [m]'
sd_sd_label      <- 'Spat SD of Vert SD (sd_sd)'          # sd_sd 
l_sd_sd_hist     <- c(0, 10 )        ;   l_sd_sd_map     <- c(0, 3   )      ; l_sd_sd_unit    <- ' [m]'
sd_cv_label      <- 'Spat SD of Vert CV (sd_cv)'          # sd_cv 
l_sd_cv_hist     <- c(0, 1  )        ;   l_sd_cv_map     <- c(0, 0.3   )    ; l_sd_cv_unit    <- ''

l_sd_mean <- list(    label        = sd_mean_label,
                      sd_mean      = list(l_sd_mean_hist,   l_sd_mean_map, l_sd_mean_unit)  )
l_sd_sd   <- list(    label        = sd_sd_label,
                      sd_sd        = list(l_sd_sd_hist,   l_sd_sd_map, l_sd_sd_unit)  )
l_sd_cv   <- list(    label        = sd_cv_label,
                      sd_cv        = list(l_sd_cv_hist,   l_sd_cv_map, l_sd_cv_unit)  )

# foliage diversity and cover
mu_fhd_normal_label     <- 'Foliage height div'          # fhd_mean
l_mu_fhd_normal_hist    <- c(0, 4  )        ;   l_mu_fhd_normal_map   <- c(2.2, 3.2  ); l_mu_fhd_normal_unit   <- ''
mu_pai_label       <- 'PAI'          # mu_pai
l_mu_pai_hist      <- c(0, 5  )        ;   l_mu_pai_map     <- c(0, 4  )    ; l_mu_pai_unit     <- ''
mu_cover_label     <- 'Cover'          # mu_cover
l_mu_cover_hist    <- c(0, 1  )        ;   l_mu_cover_map   <- c(0.2, 0.8 ) ; l_mu_cover_unit   <- ''

l_mu_fhd_normal  <- list(    label        = mu_fhd_normal_label,
                             mu_fhd_normal= list(l_mu_fhd_normal_hist,   l_mu_fhd_normal_map, l_mu_fhd_normal_unit)  )
l_mu_pai    <- list(    label        = mu_pai_label,
                        mu_pai       = list(l_mu_pai_hist,   l_mu_pai_map, l_mu_pai_unit)  )
l_mu_cover  <- list(    label        = mu_cover_label,
                        mu_cover     = list(l_mu_cover_hist,   l_mu_cover_map, l_mu_cover_unit)  )

# skewnewss and kurtosis
mu_skew_label    <- 'Skewness'          # mu_skew # skew_mean
l_mu_skew_hist   <- c(-2, 2  )       ;   l_mu_skew_map   <- c(-0.6, 0.6  )  ; l_mu_skew_unit  <- ''
mu_kurt_label    <- 'Kurtosis'          # mu_kurt # kurt_mean
l_mu_kurt_hist   <- c(-2, 2  )       ;   l_mu_kurt_map   <- c(-0.7,0.7  )      ; l_mu_kurt_unit  <- ''
sd_skew_label    <- 'Skewness (S.D.)'          # sd_skew # skew_mean
l_sd_skew_hist   <- c(0, 2  )       ;   l_sd_skew_map   <- c(0.25, 1 )       ; l_sd_skew_unit  <- ''
sd_kurt_label    <- 'Kurtosis (S.D.)'          # sd_kurt # kurt_mean
l_sd_kurt_hist   <- c(0, 2  )       ;   l_sd_kurt_map   <- c(0.25, 1  )      ; l_sd_kurt_unit  <- ''

l_mu_skew   <- list(    label        = mu_skew_label,
                        mu_skew      = list(l_mu_skew_hist,   l_mu_skew_map, l_mu_skew_unit)  )
l_mu_kurt   <- list(    label        = mu_kurt_label,
                        mu_kurt      = list(l_mu_kurt_hist,   l_mu_kurt_map, l_mu_kurt_unit)  )
l_sd_skew   <- list(    label        = sd_skew_label,
                        sd_skew      = list(l_sd_skew_hist,   l_sd_skew_map, l_sd_skew_unit)  )
l_sd_kurt   <- list(    label        = sd_kurt_label,
                        sd_kurt      = list(l_sd_kurt_hist,   l_sd_kurt_map, l_sd_kurt_unit)  )   

# updated limits for static metrics - diversity structural horizontal
shannon_entropy_label   <- 'Shannon entropy'   # shannon_entropy
l_shannon_entropy_hist  <- c(0, 8  )         ;   l_shannon_entropy_map   <- c(2, 6  )      ; l_shannon_entropy_unit   <- ''
simpson_index_label     <- 'Simpson index'     # simpson_index
l_simpson_index_hist    <- c(0, 1  )         ;   l_simpson_index_map     <- c(0.95, 1  )    ; l_simpson_index_unit   <- ''
rao_quadratic_entropy_label     <- 'Rao quad entropy'   # rao_quadratic_entropy
l_rao_quadratic_entropy_hist    <- c(0, 15  );   l_rao_quadratic_entropy_map   <- c(2, 10  ); l_rao_quadratic_entropy_unit   <- ''
euclidean_distances_mean_label  <- 'euc distance'   # euclidean_distances_mean
l_euclidean_distances_mean_hist <- c(0, 40 ) ;   l_euclidean_distances_mean_map   <- c(0, 20  )   ; l_euclidean_distances_mean_unit   <- ''
convex_hull_volume_label     <- 'Convex hull vol'   # convex_hull_volume
l_convex_hull_volume_hist    <- c(0, 60000  );   l_convex_hull_volume_map   <- c(0, 20000   )    ; l_convex_hull_volume_unit   <- ''

l_shannon_entropy          <- list(    label          = shannon_entropy_label,
                                       shannon_entropy        = list(l_shannon_entropy_hist,   l_shannon_entropy_map, l_shannon_entropy_unit)  )
l_simpson_index            <- list(    label            = simpson_index_label,
                                       simpson_index          = list(l_simpson_index_hist,   l_simpson_index_map, l_simpson_index_unit)  )
l_rao_quadratic_entropy    <- list(    label    = rao_quadratic_entropy_label,
                                       rao_quadratic_entropy  = list(l_rao_quadratic_entropy_hist,   l_rao_quadratic_entropy_map, l_rao_quadratic_entropy_unit)  )
l_euclidean_distances_mean <- list(    label    = euclidean_distances_mean_label,
                                       euclidean_distances_mean  = list(l_euclidean_distances_mean_hist,   l_euclidean_distances_mean_map, l_euclidean_distances_mean_unit)  )
l_convex_hull_volume       <- list(    label        = convex_hull_volume_label,
                                       div_hull      = list(l_convex_hull_volume_hist,   l_convex_hull_volume_map, l_convex_hull_volume_unit)  )



# # OLD limits for static metrics - diversity structural

#######################################
##### SELECT OPTIONS TO LOOP OVER #####
#######################################
# here chose which variables to run over in this particular analysi

# Create a named list of the variables containing the parameters, format:  l_vars_ts[[var_name]][[stat_name/label]][[hist/map/unit]]
l_vars    <- list(# time varying metrics
  'kndvi' = l_kndvi ,
  't2m' = l_t2m, 'VPD' = l_VPD, 'ssr' = l_ssr, 'tp' = l_tp,
  
  # fixed metrics
  'forestcover' = l_forestcover, 'socc30cm' = l_socc30cm,
  'KG5' = l_KG5, 'KG16' = l_KG16,
  'topology_elevation_mean' = l_elevation_mean, 'topology_elevation_std' = l_elevation_std,
  'topology_slope_mean' = l_slope_mean, 'topology_slope_std' = l_slope_std,
  
  ## diversity metrics
  'div_count' = l_div_count ,
  #  vertical structural profile
  'mu_rh50' = l_mu_rh50   , 'mu_rh98'  = l_mu_rh98  ,  'mu_rh75' = l_mu_rh75  ,
  'mu_rh25' = l_mu_rh25   ,
  'mu_pai' = l_mu_pai     , 'mu_cover' = l_mu_cover ,
  #  vertical structural diversity
  'mu_skew' = l_mu_skew   , 'Kurtosis'  = l_mu_kurt  ,
  'mu_sd'   = l_mu_sd     , 'mu_mean'  = l_mu_mean  ,  'mu_cv'   = l_mu_cv    ,
  'mu_fhd_normal' = l_mu_fhd_normal ,

  #  spatial diversity in vertical profile
  'sd_rh50' = l_sd_rh50   , 'Canopy_heights_sd'  = l_sd_rh98  ,  'sd_rh75' = l_sd_rh75  ,
  'sd_rh25' = l_sd_rh25   ,
  'sd_pai' = NULL         , 'sd_cover' = NULL       ,
  #  spatial diversity in vertical diversity
  'sd_skew' = l_sd_skew   , 'sd_kurt'  = l_sd_kurt  ,
  'sd_sd'   = l_sd_sd     , 'sd_mean'  = l_sd_mean  ,  'sd_cv'   = l_sd_cv    ,
  'sd_fhd_normal' = NULL  ,
  #  spatial diversity  via entropy index of rh50 rh75 rh98 cover
  'Shannon' = l_shannon_entropy, 'simpson_index' = l_simpson_index,
  'rao_quadratic_entropy' = l_rao_quadratic_entropy,
  'euclidean_distances_mean' = l_euclidean_distances_mean
)


#  END
