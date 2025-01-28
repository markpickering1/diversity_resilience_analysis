# ########################################################
# Title         : input_plotRFl.R
# Description   : This text script acts as a user input to plotRF.R
#                 By setting variables in this file, the user should not need to edit the main code
#                 This file should not change once initialised, except when running full separate
#                 analysis after major changes
# Date          : 9/07/23
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################


###################################################
######     I/O                                #####
###################################################

# set output path extension - with date this will link output (variables) to next script
script_output_ext <- 'plotRF_newDiv_selectionsCTT_shanUpdate_kurtInvert'           # output dir

# input dataset containing the random forest objects to plot
# date_production <- 'createRF_2023-07-08_mntry7_ntree500'   # updated tp/ssr post EGU presentation (12/07)
# date_production <- 'createRF_2023-08-18'          # edits following the (12/07) presentation
# date_production <- 'createRF_altResilMetrics_2023-10-26_VAR_resid'       # variation of residuals
# date_production <-  'createRF_lambda_2023-11-02_lambda_tac' # 'lambda_2023-11-02_lambda_variance' #  #   # lambda_2023-11-02_lambda_tac # 'lambda_2023-11-02'
# date_production <-  'createRF_parallelDiv_metrics_2023-11-23' # new Div metricss + new res metrics
# date_production <-  'createRF_parallelDiv_metrics_selections_2024-01-23' #new div metrics with selections - 1st attempt
date_production <-  'createRF_parallelDiv_metrics_selectionsCommonTestTrain_2024-02-22' #new div metrics with selections - and common testTrain split
# date_production <- 'createRF_bootDiv_metrics_preselectTestTrain_shanUpdate_2024-11-12_resampleNullHyp'   # null hypothesis test


input_dir <- paste0(root_data_proce, date_production, '/')

# for paralellisations
# number of cores to use
n_cores <- 20

# input dataset containing 1d pdp data to plot (can try for 2d also)
# data_production_pdp <- 'plotRF_newDiv_2023-11-29_partialPlot/df_rf_model_partDep_diversity_data-train.RData'
data_production_pdp <- 'plotRF_newDiv_selections_2024-01-25_partialPlot/df_rf_model_partDep_diversity_data-both.RData'
input_plot_pdp <- paste0(root_data_figs, data_production_pdp )


#####################################################
###### SELECT VARIABLES OF INTEREST IN ANALYSIS #####
#####################################################
# these should be the same as the input variables to the RF model
# choose identifier, target and predictor variables
# some predictor variables are used in all RF models, other 'optional' variables
# are cycled over an included in only one RF model each time (this applies to the 
# diversity metrics, including 'no_diversity' metric)

# target variable to predict 
# v_target <- 'kndvi_VAR_resid' #'kndvi_CV' # 'kndvi_TAC'  # kndvi_CV
v_target <- c( 'kndvi_lambda_xt' , 'kndvi_lambda_variance') #, 'kndvi_TAC') # , 'kndvi_TAC') # kndvi_lambda_xt' ) #, 'kndvi_TAC', 'kndvi_lambda_variance') # if this doesn't work in some scripts put only one until script updated
v_identifiers <- c('x', 'y', 'train_sample')

# variables to split and run RF over 
# v_split_RF <- c('KG5')

# variables to go as predictor in every model
# v_predictors <- c('t2m_mean')

v_predictors <- c( 'kndvi_mean',
                   'socc30cm', # soil carbon content
                   'forestcover', # previously: 'mu_treecover',
                   'topology_elevation_std', # topology metric # topology_elevation_mean topology_slope_mean topology_slope_std
                   'ssr_mean', 'ssr_CV', 'ssr_TAC',
                   't2m_mean', 't2m_CV', 't2m_TAC',
                   'tp_mean', 'tp_CV', 'tp_TAC',
                   'VPD_mean', 'VPD_CV', 'VPD_TAC'
                   # 'mu_spei', 'cv_spei', 'tac_resid_spei' # shouldn't really use this as predictor - use VPD instead
)

# add biodiversity variables to loop over and add to separate models
# v_optional_predictors <- c( "no_diversity")
v_optional_predictors <- c( 
  # "mu_rh98"                 , #
  # "mu_rh50"                 , "mu_rh75"                 , # "mu_rh25",
  # "mu_skew"                  # ,
   "mu_kurt"                  ,
  # "mu_sd"                   ,
  "sd_rh98"             ,    #  "sd_rh75"                 , "sd_rh50"                 , # "sd_rh25"                  ,
  "shannon_entropy"    # ,    # ,
  # "simpson_index"          #  ,
  # "rao_quadratic_entropy"   , "euclidean_distances_mean",
  # 
  # "mu_fhd_normal"             #, "mu_pai"                  , "mu_cover"  ,
  # "mu_mean"                   , "mu_cv"       
  # "mu_skew_negativevalues"    "mu_kurt_negativevalues"    "mu_sd_negativevalues"      "mu_mu_negativevalues"     
  # "mu_cv_negativevalues"      
  # "sd_fhd_normal"             "sd_pai"                   
  # "sd_cover"                  "sd_skew_negativevalues"    "sd_kurt_negativevalues"    "sd_sd_negativevalues"     
  # "sd_mu_negativevalues"      "sd_cv_negativevalues"      "sd_skew"                   "sd_kurt"                  
  # "sd_sd"                     "sd_mean"                   "sd_cv"                       "euclidean_distances_stdev"
)

# # OLD biodiversity variables to loop over and add to separate models
# v_optional_predictors <- c( # "no_diversity",
#                             "rh50_mean", "rh98_mean", "fhd_mean", "skew_mean",  "kurt_mean",
#                             "shannon_entropy", "simpson_index", "rao_quadratic_entropy", "euclidean_distances_mean", "convex_hull_volume"
# )


#################################################
##### SELECT FIGURES TO PRODUCE FOR EACH RF #####
#################################################

# toggle whether to use the '|abs|' restoration rates - this inverts the resilience metric so that higher = more resilience
b_useAbs_RestRate <- T
b_invert_mu_kurt  <- T # invert the kurtosis so that postive is associated with more diversity
b_t2m_in_C        <- T # only set for the 2d plots so far

# select which analyses to plot
b_run_performance <- F    # check performance metrics of RF
b_run_importance  <- F    # create importance ranking for RF variables
b_run_partialplot <- T    # create partial plots of RF diversity (and other?) variables
b_run_partialplot_nonDiv <- F # create partial plots of non-diversity variables
b_run_partialplot_2d     <- T # run 2d pdp
b_run_partialplot_boot   <- F # run bootstrapped partial plots from pre-created dfs
# run and plot dice? honestly the dice takes so long that it is better to run over each variable individuallyand adjust the lims and values as needed
# - I don't recommend running via the function - just step through the function
b_run_dice        <- T    # create (actual) derivative of individual conditional expectation (dice) figs for diversity metrics

#################################################
##### PLOTTING OPTIONS FOR SUMMARY FIGS     #####
#################################################

######################################
###### PERFORMANCE PLOTS        ###### 
######################################
# run the performance on the train dataset (instead of the normal test)
b_run_on_train <- F

y_lims_TAC_perf    <- c(-0.1, 1)
y_lims_lambda_perf <- c(-3, 0)   # var for kurt both lambda

y_lims_pdp_perf    <- list( "kndvi_TAC" = y_lims_TAC_perf , 
                       # "kndvi_lambda_xt" = y_lims_lambda_xt , "kndvi_lambda_variance" = y_lims_lambda_var )
                       "kndvi_lambda_xt" = y_lims_lambda_perf , "kndvi_lambda_variance" = y_lims_lambda_perf )


######################################
###### IMPORTANCE PLOTS         ######
######################################

lims_importance <- c(0,200)

# should the importance estimation, calculated from mean MSE change of permutation of predictors on OOB
# be scaled by the standard deviation of this MSE change
b_scale_importance <- TRUE

 input_dir_boot_imp <- 'createRF_bootDiv_metrics_preselectTestTrain_2024-03-03_noXYTS/list_of_rfs/' # seed 98-102
# input_dir_boot_imp <- 'createRF_bootDiv_metrics_preselectTestTrain_shanUpdate_2024-11-12_resampleNullHyp/'   # null hypothesis test
######################################
###### ICE FIGURES              ######
######################################

#input
# f_ice_input <- '/mnt/eos_rw/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_selectionsCommonTestTrain_2024-02-27_createICE/'  # old unupdated shannon
f_ice_input <- '/mnt/eos_rw/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_selectionsCTT_shanUpdate_kurtInvert_2024-08-10_createICE/'  # old un-updated shannon

# plot derivative vs predictor scatters
b_create_deriv_vs_predictor_figs <- F

## DERIVATIVE LIMITS
# l_dice_lims_h_in <- c(-1, 1) ; l_dice_lims_m_in <- c(-0.25, 0.25) # mu_kurt/shannon & lambda_xt
# l_dice_lims_h_in <- c(-1, 1) ; l_dice_lims_m_in <- c(-0.3, 0.5) # mu_kurt/shannon & lambda_xt
l_dice_lims_h_in <- c(-1, 1) ; l_dice_lims_m_in <- c(-0.5, 0.5) # mu_kurt/shannon & lambda_xt
# l_dice_lims_h_in <- c(-1.5, 1.5)
# l_dice_lims_h_in <- c(-0.1, 0.1) ; l_dice_lims_m_in <- c(-0.04, 0.04) # sd_98 & lambda_xt
l_dice_lims <- list(l_dice_lims_h_in, l_dice_lims_m_in)

#OLD LIMS
# lims_h_in <- c(-0.05, 0.05) ; lims_m_in <- c(-0.004, 0.004)
# lims_h_in <- c(-0.1, 0.1) ; lims_m_in <- c(-0.02, 0.02)
# lims_h_in <- c(-0.3, 0.3) ; lims_m_in <- c(-0.08, 0.08)
# lims_h_in <- c(-40, 40) ; lims_m_in <- c(-10, 10)
# lims_h_in <- c(-0.02, 0.02) ; lims_m_in <- c(-0.005, 0.005)
# if(var_name_i == "fhd_avg_diversity" | var_name_i == "kurt_avg_diversity")  { lims_h_in <- c(-0.2, 0.2) ; lims_h_in <- c(-0.1, 0.1) }


######################################
###### PARTIAL DEPENDENCE ALL   ######
######################################
# this is for the non-parallelised, non-bootstrapped method of plotting pdps (plot_RF_partialPlot_original.R)

# option on whether to do the test-train split within the createRF_model script, or load directly a dataframe
# which the selection script has already determined a consistent test-train split
# true uses a shared test-train split common to different resilience and diversity metrics
b_do_common_testTrainSplit <- T

# testing to see the separate results of all the different iterations and seeds of the bootstrapping
b_plot_all_RF_PDPs <- F

# choose which dataset to use in pdp for creating df - training recommended, though good to check similarity with test
s_use_pdp_dataset <- 'train' # test, # all
# choose which dataset to use in pdp for plotting df - assuming already created
s_plot_pdp_dataset <-'train' # train' test, # all
# add a histogram to the plots of pdp
b_plot_hist        <- T 
# diversity rf.model to use to produce the non-diversity variable partial plots if(b_run_partialplot_nonDiv)
var_i_nonDiv_partPlot <- 'no_diversity' 

# select the number of points to put into the pdp (i.e. how many marks on the x-axis of the pdp plots)
n_pdp_points <- 200

# limits for TAC
# y_lims_pdp <- c(0.24, 0.33) # diversity metric coverage
# y_lims_pdp  <- c(0.245, 0.31) # diversity metric coverage small
# y_lims_pdp <- c(0.25, 0.30)  # diversity metric too small
# y_lims_TAC <- c(0.22, 0.36) # other predictor metric coverage
# y_lims_TAC <- c(0.2, 0.5) # other predictor metric coverage

y_lims_TAC <- c(0.25, 0.4) # to cover all vars
# y_lims_TAC <- c(0.28, 0.34) # for mu_kurt
# y_lims_TAC <- c(0.26, 0.34) # for shannon

# limits for variance of base
# y_lims_pdp <- c(23, 25.2) # other predictor metric coverage

# limits for variance of residuals
# y_lims_pdp <- c(0.00248, 0.0027) # other predictor metric coverage

# limits for lambda
# y_lims_lambda_xt <- c(-1.51, -1.25)  # xt  for mu_kurt;
# y_lims_lambda_var <- c(-1.38, -1.12) # var for mu_kurt;
# y_lims_lambda_xt <- c(-1.72, -1.25)  # xt  for shannon; 
# y_lims_lambda_var <- c(-1.5, -1.12)  # var for shannon; 
# y_lims_lambda <- c(-1.45, -1.05)   # other predictor metric coverage
# y_lims_lambda <- c(-1.75, -1.1)    # covers the 1d pdps

# y_lims_lambda <- c(-1.55, -1.12)  # var for shannon both lambda - old
# y_lims_lambda <- c(-1.6, -1.12)  # var for shannon both lambda
# y_lims_lambda <- c(-1.4, -1.12)   # var for kurt both lambda
# y_lims_lambda <- c(-1.3, -1.12)   # var for sd_rh98 both lambda

# y_lims_lambda <- c(-1.8, -1.1)       # covers all res metrics & vars
y_lims_lambda <- c(1.1, 1.6)           #  abs values -  covers all res metrics & vars
# y_lims_lambda <- c(-1.72, -1.12)       # covers all res metrics & vars better
# y_lims_lambda <- c(-1.51, -1.1)       # for mu_kurt
y_lims_lambda_2d <- c(1.05, 1.8)           #  abs values -  covers all res metrics & vars


y_lims_pdp    <- list( "kndvi_TAC" = y_lims_TAC , 
                       # "kndvi_lambda_xt" = y_lims_lambda_xt , "kndvi_lambda_variance" = y_lims_lambda_var )
                    "kndvi_lambda_xt" = y_lims_lambda , "kndvi_lambda_variance" = y_lims_lambda )

# colors_resilience_metrics <- setNames(c("red", "blue", "green"), c("kndvi_lambda_variance", "kndvi_lambda_xt", "kndvi_TAC") )

# set the axis range for the pdp figures and 2d pdps
# if you don't want to put in, then just leave empty or null - hists may not align though
lim_sd_rh50     <- c(1,12)
lim_sd_rh75     <- c(1,12)
lim_sd_rh98     <- c(1,12)
# lim_sd_rh98     <- c(3,10) # updated limits

lim_mu_rh50     <- c(2,20)
lim_mu_rh75     <- c(5,28)
lim_mu_rh98     <- c(5,35)

# lim_shannon_entropy  <- c(3.5  , 8) # 3.5, 8
# lim_shannon_entropy  <- c(3.75 , 7.5) # 3.5, 8 # updated limits
lim_shannon_entropy  <- c(3,5) # 3.5, 8 # updated limits - fixed shannon
lim_simpson_index    <- c(0.97, 1)
lim_rao_quadratic_entropy <- c(2,12)
lim_euclidean_distances_mean <- c(2,25)

lim_mu_kurt     <- c(-1.25, 0)
lim_mu_skew     <- c(-1,1)
lim_mu_fhd_normal   <- c(2, 3.2)


lim_kndvi_mean <- c(0.2 ,  0.55)
lim_socc30cm   <- c(0.3 ,  1.3) 
lim_forestcover<- c(0.5 ,  1)
lim_topology_elevation_std <- c(0,300)
lim_t2m_mean   <- c(285 ,  292)         ; lim_t2m_CV   <- c(1.2 ,  2.2)       ; lim_t2m_TAC   <- c(0.2 ,  0.45)
lim_tp_mean    <- c(0   ,  6)           ; lim_tp_CV    <- c(50   ,  200)
lim_VPD_mean   <- c(0.25,  1.25)
lim_ssr_mean   <- c(10000000, 20000000) ; lim_ssr_CV   <- c(20, 45) # c(50  ,  200)

l_lims_in <- list('mu_rh50' = lim_mu_rh50, 'mu_rh75' = lim_mu_rh75, 'mu_rh98' = lim_mu_rh98,
                  'sd_rh50' = lim_sd_rh50, 'sd_rh75' = lim_sd_rh75, 'sd_rh98' = lim_sd_rh98,
                  "shannon_entropy" = lim_shannon_entropy, "simpson_index" = lim_simpson_index,
                  'rao_quadratic_entropy' = lim_rao_quadratic_entropy,
                  'euclidean_distances_mean' = lim_euclidean_distances_mean,
                  "mu_skew" = lim_mu_skew, "mu_kurt" = lim_mu_kurt,
                  'mu_fhd_normal'  = lim_mu_fhd_normal ,
                  
                  'kndvi_mean' = lim_kndvi_mean,  'socc30cm' = lim_socc30cm, 
                  'forestcover' = lim_forestcover, 'topology_elevation_std' = lim_topology_elevation_std,
                  't2m_mean'   = lim_t2m_mean, 't2m_CV'   = lim_t2m_CV,     't2m_TAC' = lim_t2m_TAC,
                  'tp_mean'    = lim_tp_mean,  'tp_CV'    = lim_tp_CV,
                  'VPD_mean'   = lim_VPD_mean,    
                  'ssr_mean'   = lim_ssr_mean, 'ssr_CV'   = lim_ssr_CV
                  
)

# l_lims_in_pred <-  list( 'kndvi_mean' = lim_kndvi_mean, 't2m_mean'   = lim_t2m_mean,    'tp_mean'  = lim_tp_mean,
#                          'VPD_mean'   = lim_VPD_mean,    'ssr_mean' = lim_ssr_mean) 


# create groupings for displaying some pdps together
var_group_1 <- c("mu_skew" , "mu_kurt" , "mu_fhd_normal" )                 # c("skew_mean" , "kurt_mean" , "fhd_mean" )
var_group_2 <- c("mu_rh50" , "mu_rh98" , "euclidean_distances_mean" )     # c("rh50_mean" , "rh98_mean" , "euclidean_distances_mean" )
var_group_3 <- c("shannon_entropy",  "rao_quadratic_entropy")             # c("shannon_entropy",  "rao_quadratic_entropy")
var_group_4 <- c("simpson_index")                                         # c("simpson_index")
var_group_5 <- c("convex_hull_volume")
var_group_6 <- c('sd_rh50',     'sd_rh98', 'sd_rh75', 'mu_sd' )                 # c("skew_mean" , "kurt_mean" , "fhd_mean" )

pdp_groups  <- list(var_group_1, var_group_2, var_group_3, var_group_4, var_group_5, var_group_6)
# pdp_groups <- v_optional_predictors

##### non-diversity partial plots

# just run over the individual predictors
# pdp_groups  <- v_predictors


# or build groups (prob too complicated) 
# var_group_1_nonDiv <- c("kndvi_mean" , "forestcover"  ) # 0-1
# var_group_2_nonDiv <- c("socc30cm"  )   # 0-5
# var_group_3_nonDiv <- c("topology_elevation_std"  )   # 0-600
# var_group_5_nonDiv <- c("ssr_mean"  )   # 10^6, 10^7
# var_group_6_nonDiv <- c("ssr_CV"  )     # 0-60
# var_group_4_nonDiv <- c("ssr_TAC"  )    # -0.5 - 0.66 (but most in middle)
# var_group_4_nonDiv <- c("t2m_mean"  )   # 275-305
# var_group_4_nonDiv <- c("t2m_CV"  )   # 0-3
# var_group_4_nonDiv <- c("t2m_TAC"  )    # -0.51 - 0.78 (but variation really 0.3-0.5)
# var_group_4_nonDiv <- c("tp_mean"  )   # 0-9
# var_group_4_nonDiv <- c("tp_CV"  )   # 0-600 (but variabtion between like 50-120)
# var_group_4_nonDiv <- c("tp_TAC"  )   # -0.38-0.88 (variation 0-0.3)

# run over predictor groups to group similar range values together for plotting
# pdp_groups  <- list(var_group_1, var_group_2, var_group_3, var_group_4, var_group_5)


############### 2d pdp ####

# vars to run 2d visualisation with
pdp_2d_extra_vars <- c('t2m_mean') #, 'tp_mean'), 'VPD_mean', 'ssr_mean') # c('t2m_mean) c('VPD_mean') # c('tp_mean')

# location to load output from (if collating from different output files)
# dir_input_2d_pdps <- paste0(root_data_figs, 'plotRF_2023-09-07_partialPlot/') 
# dir_input_2d_pdps <- paste0(root_data_figs, 'plotRF_newDiv_selections_2024-01-26_partialPlot_2d/')  # 3rd diversity, with selections
# dir_input_2d_pdps <- paste0(root_data_figs, 'plotRF_newDiv_selectionsCommonTestTrain_2024-03-07_partialPlot_2d/') # 
dir_input_2d_pdps <- paste0(root_data_figs, 'plotRF_newDiv_selectionsCTT_shanUpdate_kurtInvert_2024-09-06_partialPlot_2d/')  # updated shannon

######################################
###### PARTIAL DEPENDENCE PARA  ######
######################################
# in this method the parallel pdps have their own separate file location and are stored differently 
# there is also no pdp creation involved - the pdps are already created in a separate step and just require plotting

# data_production_pdp_para <-  'createRF_2023-08-24_noDiv_bootstrap'
# data_production_pdp_para <-  'createRF_bootDiv_metrics_2024-02-02' # 'createRF_bootDiv_metrics_2024-02-02'
# data_production_pdp_para <-  'createRF_bootDiv_metrics_2024-02-02_working' # 'createRF_bootDiv_metrics_2024-02-02'
# data_production_pdp_para <-  'createRF_bootDiv_metrics_4_2024-02-19' # 'createRF_bootDiv_metrics_2024-02-02'
# data_production_pdp_para <-  'createRF_2023-08-26_diversity_kurtShanSimpSkew' # 'createRF_2023-08-28_diversity_fhdRH' 'createRF_2023-08-26_diversity_kurtShanSimpSkew' #'createRF_2023-08-24_noDiv_bootstrap'
# data_production_pdp_para <-  'createRF_bootDiv_metrics_preselectTestTrain_2024-02-23' # new div metrics with selections - 2nd attempt - common testTrain splot - but accidentally included x,y,testtrain in model
data_production_pdp_para <-  'createRF_bootDiv_metrics_preselectTestTrain_2024-03-03_noXYTS'
# data_production_pdp_para <- 'createRF_bootDiv_metrics_preselectTestTrain_shanUpdate_2024-11-12_resampleNullHyp'   # null hypothesis test

input_dir_pdp_para <- paste0(root_data_proce, data_production_pdp_para, '/')

# select predictors - use as above in non para
# v_plot_pdp_para <- v_optional_predictors
# select the non diversity predictors
# v_plot_pdp_para <- v_predictors

# limits for TAC z-axis
# z_lims_pdp_2d <- c(0.2, 0.37) # diversity metric coverage small
# z_lims_pdp_2d <- c(0.24, 0.34) #y_lims_pdp

# use random seeds:
l_seeds <- c(98,99,100,101,102)




