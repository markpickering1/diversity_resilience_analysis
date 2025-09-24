# ########################################################
# Title         : plotByBGR_createPDP_byBGR_bs.R
# Description   : run 1d pdp by bgr with bs rf models and with current and increased temperature
# Aims          : confront 1d pdp by bgr and with current and increased temperature
# Inputs	      : bs rf models, bgr df, input training/testing df 
# Outputs	      : df of pdp by bgr
# Options	      : 
# Date          : 23/09/25
# Version       : 1 
# Authors       : Mark Pickering & Agata Elia
# Notes		      : 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())    

# set initialisation script names
main_initialisation_file   <- '0_main/initialise_R.R'
script_config_dir          <- '3_create_figures/input/'    ;   script_config_file <- 'input_plotByBGR.R'

# initialise code setup and repo building
source(main_initialisation_file)
# initialise figure common formatting for code base
source('0_main/initialise_figs.R')
# load common plotting functions
source('0_main/functions/plotting_functions.R')
# initialise user inputs (config) to script
source( paste0( script_config_dir, script_config_file) )
# load RF plotting functions
source('3_create_figures/functions/f_plotRF.R')

######     SET LIBRARIES                      #####
library(dplyr)        # use %>%
library(reshape)      # reshaping dataframes
require(ggplot2)      # for plotting
require(scales)       # for ggplot2 functions eg oob & trans
library(RColorBrewer) # colour palettes
library(sf)           # utilise shapefiles etc
library(cowplot)      # for ggdraw
library(ICEbox)  
library(terra)
library(raster)
library(randomForest)


###################################################
######       I/O                              #####
###################################################

# output location
# set/create output directories
output_path <- paste0(root_data_figs, script_output_ext, '_1d_pdp_bs_', full_date,  '/')
print(paste0('output_path is : ', output_path ))

# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for figures of 1d pdp at diff temp: ', output_path ) ) }

# copy configuration input variables to output for storage
if( ! file.exists( paste0(output_path, script_config_file ) ) ) { print('copy config file') 
  file.copy(from= paste0( script_config_dir, script_config_file) , to= paste0(output_path, script_config_file ), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
} else{ print('could not copy config file') }

# load BGR file
load(paste0(input_dir_bgr, input_file_bgr))

# round digit of bgr file
df_var[1:2] <- df_var[1:2] %>% round( digits = 3)

# load all data (since they are now all in one df)
load(paste0(input_dir_rf, input_all_data) )

# initialize train/test df from the all data
df_comb.train_i <- subset(df_comb, train_sample == T) # head(df_comb.train_i)
df_comb.test_i  <- subset(df_comb, train_sample == F) # head(df_comb.test_i)

# create the pdps using the training, testing or all data
if(s_train_test_all == 'train'){ df_pdp <-  df_comb.train_i[complete.cases(df_comb.train_i), ] ; print('using train data') }
if(s_train_test_all == 'test') { df_pdp <-  df_comb.test_i[complete.cases(df_comb.test_i), ]   ; print('using test data')  }
if(s_train_test_all == 'all')  { df_pdp <-  df_comb_i[complete.cases(df_comb_i), ]             ; print('using all data')   }

# add the bgr to the df
df_pdp_bgr <- left_join(df_pdp, df_var)
df_pdp_bgr <- df_pdp_bgr[complete.cases(df_pdp_bgr), ]

#############################################################
##### RUN 1D PDPS FOR CURRENT AND INCREASED TEMPERATURE #####
#############################################################
# this loops over the variables and create 1d pdp for current and increased temperature

# loop over diversity metrics and resilience metrics
for (i in 1:length(v_optional_predictors)){
  for (j in 1:length(v_target)){
    
    target_i <- v_target[j] ; print(target_i)
    var_name_i <- v_optional_predictors[i] ; print(var_name_i)
    var_i_full <- l_vars[[var_name_i]][['label']] ; print(var_i_full)
    
    # select only relevant predictors and the identifiers
    v_all_vars <- c(target_i, var_name_i, v_predictors)  ; print(v_all_vars)
    
    #### RUN PDPS BY BIOGEOGRAPHICAL REGION ####
  
    # run through every bgr and run pdp separately by classes
    for (r in 1:length(bgr)){
      
      # select bgr
      bgr_region <- bgr[r] ; print(bgr_region)
      
      # select from the df only the data points belonging to the selected bgr
      df_pdp_bgr_r <- df_pdp_bgr[df_pdp_bgr[['BiogeoRegions2016']] == bgr_region, ]
      
      # select only relevant predictors without the identifiers to run in model
      df_pdp_bgr_r <- df_pdp_bgr_r[, v_all_vars]
      
      # correct dataframe to have absolute value of resilience metrics
      df_pdp_bgr_r <- df_pdp_bgr_r %>% mutate(!!target_i := abs(.data[[target_i]]))
      
      # create df with temperatures increased of 1 deg
      df_pdp_bgr_r_tp1deg <- df_pdp_bgr_r %>% mutate(t2m_mean = t2m_mean + 1, !!target_i := abs(.data[[target_i]]))
      
      #### RUN 1D PDPS FOR EACH SEED AND EACH K OF THE RF ####
      
      # loop over the seeds 
      for(n in 1:length(l_seed)){
        
        #select seed
        n_seed <- l_seed[n] ; print(n_seed)
        
        # initialize the file containing rf models for the specific seed
        input_rf_file  <- paste0(input_dir_rf, input_rf_common_name, var_name_i, '_seed-', n_seed, '_targ-', target_i, '.RData')
        
        # load the rf models for the specific seed
        load(input_rf_file) 
        l_rf_boot <- results$rf.models
        length(l_rf_boot)
        
        for(k in 1:20) {
          
          # run the 1d pdp
          pp_i <- partialPlot(l_rf_boot[[k]], df_pdp_bgr_r, names(df_pdp_bgr_r)[2])
          
          # convert pdp to df and round digits
          pp_i <- as.data.frame(pp_i);
          
          # change names of pdp df
          names(pp_i) <- c(var_name_i, target_i)
          pp_i$t <- 0
          pp_i <- pp_i %>% mutate(!!target_i := abs(.data[[target_i]]))
          
          # save pdp df
          save(pp_i, file=paste0(output_path, 'df_pdp_1d-', var_name_i, '_targ-', target_i, '_tp0deg_bgr-', bgr_region, '_seed-', n_seed, '_k-', k, '.RData'))
          
          # run the 1d pdp
          pp_i <- partialPlot(l_rf_boot[[k]], df_pdp_bgr_r_tp1deg, names(df_pdp_bgr_r)[2])
          
          # convert pdp to df and round digits
          pp_i <- as.data.frame(pp_i);  
          
          # change names of pdp df
          names(pp_i) <- c(var_name_i, target_i)
          pp_i$t <- 1
          pp_i <- pp_i %>% mutate(!!target_i := abs(.data[[target_i]]))
          
          # save pdp df
          save(pp_i, file=paste0(output_path, 'df_pdp_1d-', var_name_i, '_targ-', target_i, '_tp1deg_bgr-', bgr_region, '_seed-', n_seed, '_k-', k, '.RData'))
          
        }
        
      }
      
    }
    
  }
  
}