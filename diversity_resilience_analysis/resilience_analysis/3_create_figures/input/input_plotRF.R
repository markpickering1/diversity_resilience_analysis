# ########################################################
# Title         : input_plotRFl.R
# Description   : This text script acts as a user input to the RF plotting scripts
#                 By setting variables in this file, the user should not need to edit the main code
#                 This file should not change once initialised, except when running full separate
#                 analysis after major changes
# Date          : 2025-02-19
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################


###################################################
######     I/O                                #####
###################################################

# set output path extension - with date this will link output (variables) to next script
script_output_ext <- 'plotRF'           # output dir

# input dataset containing the random forest objects to plot
date_production <-  'createRF_model'
input_dir <- paste0(root_data_proce, date_production, '/')

# input dataset containing dataframe used in model
input_dataframe <-  '4_selections'
input_df <- paste0(root_data_proce, input_dataframe, '/')

# output subdir of partial plots - links partial plot outputs with future inputs
pdp_dir <- 'plotRF_partialPlot/'

# Input dataset containing 1d pdp data to plot (this takes the df_partDep_div of
# partial plots for multiple metric combinations from plotRF_partialPlot_createDF.R and 
# links it to the plotting, plotRF_partialPlot_plotDF.R)
data_production_pdp <- 'df_rf_model_partDep_diversity_data-train.RData'
input_plot_pdp <- paste0(root_data_figs, pdp_dir, '/', data_production_pdp )


#####################################################
###### SELECT VARIABLES OF INTEREST IN ANALYSIS #####
#####################################################
# these should be the same as the input variables to the RF model
# choose identifier, target and predictor variables
# some predictor variables are used in all RF models, other 'optional' variables
# are cycled over an included in only one RF model each time (this applies to the 
# diversity metrics, including 'no_diversity' metric)

# target variable to predict 
v_target <- c( 'kndvi_lambda_xt' , 'kndvi_lambda_variance')  # if this doesn't work in some scripts put only one unlisted variable, e.g.: v_target <- 'kndvi_lambda_xt'
v_identifiers <- c('x', 'y', 'train_sample')

# variables to go as predictor in every model
v_predictors <- c( 'kndvi_mean', 'forestcover',
                   'socc30cm', 'Ndep', 
                   'topology_elevation_std', # topology metric 
                   'ssr_mean', 'ssr_CV', 'ssr_TAC',
                   't2m_mean', 't2m_CV', 't2m_TAC',
                   'tp_mean', 'tp_CV', 'tp_TAC',
                   'VPD_mean', 'VPD_CV', 'VPD_TAC'
                   )

# add biodiversity variables to loop over and add to separate models
v_optional_predictors <- c(  "Shannon", "Kurtosis", "Canopy_heights_sd")
  # "mu_rh98",  "mu_rh50", "mu_rh75", "mu_rh25", "no_diversity"
  # "mu_skew", "mu_kurt", "mu_sd", "sd_rh98", "sd_rh75", "sd_rh50", # "sd_rh25"
  # "shannon_entropy", "simpson_index", "rao_quadratic_entropy", "euclidean_distances_mean",
  # "mu_fhd_normal", "mu_pai", "mu_cover", "mu_mean", "mu_cv"


#################################################
##### SELECT FIGURES TO PRODUCE FOR EACH RF #####
#################################################

# toggle whether to use the '|abs|' restoration rates - this inverts the resilience metric so that higher = more resilience
b_useAbs_RestRate <- T
# invert the kurtosis so that postive is associated with more diversity
b_invert_mu_kurt  <- T
# convert temperatures K->C only set for the 2d plots so far
b_t2m_in_C        <- T

# select which analyses to plot - run via individual scripts
# b_run_performance <- T    # check performance metrics of RF
# b_run_importance  <- T    # create importance ranking for RF variables
b_run_partialplot <- T    # create partial plots of RF diversity (and other?) variables
b_run_partialplot_nonDiv <- F # create partial plots of non-diversity variables
b_run_partialplot_2d     <- T # run 2d pdp
b_run_partialplot_boot   <- T # run bootstrapped partial plots from pre-created dfs

# To run and plot dice takes some time it is better to run over each variable individually and adjust the lims and values as needed - I don't recommend running via the function - just step through script
b_run_dice        <- T    # create (actual) derivative of individual conditional expectation (dice) figs for diversity metrics

######################################
###### PERFORMANCE PLOTS        ###### 
######################################
# run the performance on the train dataset (instead of the normal test)
b_run_on_train <- F

y_lims_lambda_perf <- c(-3, 0)   # var for kurt both lambda

y_lims_pdp_perf    <- list("kndvi_lambda_xt" = y_lims_lambda_perf , 
                           "kndvi_lambda_variance" = y_lims_lambda_perf )

######################################
###### IMPORTANCE PLOTS         ######
######################################

lims_importance <- c(0,200)

# should the importance estimation, calculated from mean MSE change of permutation of predictors on OOB
# be scaled by the standard deviation of this MSE change
b_scale_importance <- TRUE

# input model for bootstrapped importance
# input_dir_boot_imp <- 'createRF_model/' # seed 98-102

######################################
###### ICE FIGURES              ######
######################################

# input
dir_createICE_output <- 'plotRF_createICE/'  
f_ice_input <- paste0(root_data_figs, dir_createICE_output)

# plot derivative vs predictor scatters
b_create_deriv_vs_predictor_figs <- F

## DERIVATIVE LIMITS - for maps and histograms
l_dice_lims_h_in <- c(-1, 1) ; l_dice_lims_m_in <- c(-0.5, 0.5) 
l_dice_lims <- list(l_dice_lims_h_in, l_dice_lims_m_in)


######################################
###### PARTIAL DEPENDENCE ALL   ######
######################################
# this is for the non-parallelised, non-bootstrapped method of plotting pdps (plot_RF_partialPlot_original.R)

# option on whether to do the test-train split within the createRF_model script, or load directly a dataframe
# which the selection script has already determined a consistent test-train split
# true uses a shared test-train split common to different resilience and diversity metrics
# b_do_common_testTrainSplit <- F

# testing to see the separate results of all the different iterations and seeds of the bootstrapping
b_plot_all_RF_PDPs <- F

# choose which dataset to use in pdp for creating df - training recommended, though good to check similarity with test
s_use_pdp_dataset <- 'train' # test, # all
# choose which dataset to use in pdp for plotting df - assuming already created
s_plot_pdp_dataset <-'train' # train' test, # all
# add a histogram to the plots of pdp
b_plot_hist        <- F
# diversity rf.model to use to produce the non-diversity variable partial plots if(b_run_partialplot_nonDiv)
var_i_nonDiv_partPlot <- 'no_diversity' 

# select the number of points to put into the pdp (i.e. how many marks on the x-axis of the pdp plots)
n_pdp_points <- 200

# limits for lambda
y_lims_lambda <- c(1.1, 1.6)               #  abs values -  covers all res metrics & vars
y_lims_lambda_2d <- c(1.05, 1.8)           #  abs values -  covers all res metrics & vars
y_lims_pdp    <- list( "kndvi_lambda_xt" = y_lims_lambda , "kndvi_lambda_variance" = y_lims_lambda )

# set the axis range for the pdp figures and 2d pdps
# if you don't want to put in, then just leave empty or null - hists may not align though
lim_sd_rh50     <- c(1,12) ; lim_sd_rh75     <- c(1,12) ; lim_sd_rh98     <- c(1,12)
lim_mu_rh50     <- c(2,20) ; lim_mu_rh75     <- c(5,28) ; lim_mu_rh98     <- c(5,35)

lim_shannon_entropy  <- c(3,5) ; lim_simpson_index    <- c(0.97, 1)
lim_rao_quadratic_entropy <- c(2,12) ; lim_euclidean_distances_mean <- c(2,25)
lim_mu_kurt     <- c(-1.25, 0) ; lim_mu_skew     <- c(-1,1) ; lim_mu_fhd_normal   <- c(2, 3.2)


lim_kndvi_mean <- c(0.2 ,  0.55) ; lim_forestcover<- c(0.5 ,  1)
lim_socc30cm   <- c(0.3 ,  1.3)  ; lim_Ndep       <- c(0 ,  1000) 
lim_topology_elevation_std <- c(0,300)
lim_t2m_mean   <- c(285 ,  292)         ; lim_t2m_CV   <- c(1.2 ,  2.2)       ; lim_t2m_TAC   <- c(0.2 ,  0.45)
lim_tp_mean    <- c(0   ,  6)           ; lim_tp_CV    <- c(50   ,  200)      ; lim_VPD_mean   <- c(0.25,  1.25)
lim_ssr_mean   <- c(10000000, 20000000) ; lim_ssr_CV   <- c(20, 45) 

# configure list of limits for partial plots
l_lims_in <- list( "Shannon" = lim_shannon_entropy,  "Kurtosis" = lim_mu_kurt,  'Canopy_heights_sd' = lim_sd_rh98,
                  'kndvi_mean' = lim_kndvi_mean,  'socc30cm' = lim_socc30cm, 
                  'forestcover' = lim_forestcover, 'topology_elevation_std' = lim_topology_elevation_std,
                  't2m_mean'   = lim_t2m_mean, 't2m_CV'   = lim_t2m_CV,     't2m_TAC' = lim_t2m_TAC,
                  'tp_mean'    = lim_tp_mean,  'tp_CV'    = lim_tp_CV,
                  'VPD_mean'   = lim_VPD_mean, 'ssr_mean'   = lim_ssr_mean, 'ssr_CV'   = lim_ssr_CV
)  
                  # 'mu_rh50' = lim_mu_rh50, 'mu_rh75' = lim_mu_rh75, 'mu_rh98' = lim_mu_rh98,
                  # 'sd_rh50' = lim_sd_rh50, 'sd_rh75' = lim_sd_rh75, 'sd_rh98' = lim_sd_rh98,
                  # "shannon_entropy" = lim_shannon_entropy, "simpson_index" = lim_simpson_index,
                  # 'rao_quadratic_entropy' = lim_rao_quadratic_entropy,
                  # 'euclidean_distances_mean' = lim_euclidean_distances_mean,
                  # "mu_skew" = lim_mu_skew, "mu_kurt" = lim_mu_kurt, 'mu_fhd_normal'  = lim_mu_fhd_normal ,
                  

############### 2d pdp ####

# vars to run 2d visualisation with (this forms the x-axis of the 2d figure)
pdp_2d_extra_vars <- c('t2m_mean') 

# location to load output from (if collating from different output files)
dir_input_2d_pdps <- paste0(root_data_figs, pdp_dir)

######################################
###### PARTIAL DEPENDENCE BOOT  ######
######################################
# in this method the parallelly produced pdps have their own separate file location and are stored differently 
# there is also no pdp creation involved - the pdps are already created in a previous step (see 2_analysis/) and just require plotting

# for paralellisations - number of cores to use
n_cores <- 5
# directory of parallel RF models
input_dir_pdp_para <- paste0(root_data_proce, date_production, '/')
# use seeds:
l_seeds <- c(99) # e.g. (98,99,100,101,102)

# set up rescale function and percentile (for e.g. 5%-95%)
# this selects the percentile range of the diversity metric to display in the PDPs
# enables the plotting of multiple PDPs with the same (scaled diversity range) axis
percentile_alpha <- 0.025 

# set the limits of a multiple diversity (scaled) PDP
lims_in_jj <- c(0,1) ;        # x limits - ie the range of scaled diversity (0->1)
y_lims_lambda_jj <-c(1.1,1.5) # y limits for lambda




