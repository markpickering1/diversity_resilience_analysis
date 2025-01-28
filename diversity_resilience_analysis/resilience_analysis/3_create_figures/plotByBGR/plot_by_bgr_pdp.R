# ########################################################
# Title         : plot_by_bgr_pdp.R
# Description   : run pdp by bgr 
# Aims          : confront pdp by bgr  
# Inputs	      : rf models, bgr df, inout training/testing df 
# Outputs	      : df of pdp by bgr
# Options	      : 
# Date          : 25/01/24
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
script_config_dir          <- '3_create_figures/input/'    ;   script_config_file <- 'input_plot_by_bgr.R'

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
# library(ggpubr)       # for arranging ggplots together (ggarrange)
# library(ggExtra)      # more complex figures, marginal hists
# library(grid)         # for making multi-gird plots and tables
# library(gridExtra)    # for making multi-gird plots and tables
# library(lattice)      # for making multi-gird plots and tables
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
# set/create output directory
output_path <- paste0(root_data_figs, script_output_ext, '_pdp_', full_date,  '/')
print(paste0('output_path is : ', output_path ))
# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for figures of dataframe cols : ', output_path ) ) }

# copy configuration input variables to output for storage
if( ! file.exists( paste0(output_path, script_config_file ) ) ) { print('copy config file') 
  file.copy(from= paste0( script_config_dir, script_config_file) , to= paste0(output_path, script_config_file ), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
} else{ print('could not copy config file') }

# load input file to be used to merge x,y
# load(paste0(input_dir, input_file)) # head(df_comb)  ;  summary(df_comb)

# keep only complete cases
# if(b_completeCases_for_fullDF) df_comb <- df_comb[complete.cases(df_comb), ]

# add a column identifier of row identifier
# df_comb$row_id <- as.numeric(rownames(df_comb))

# load BGR file
load(paste0(input_dir_bgr, input_file_bgr))

# round digit of bgr file
df_var[1:2] <- df_var[1:2] %>% round( digits = 3)

#############################################################
##### IDENTIFY DIVERSITY FOR MAXIMUM LOCAL RESILIENCE   #####
#############################################################
# this loops over the variables and plots each of the diversity variables against their respective ICE
print('plotting')

# loop over 'diversity metrics' to load specific df and rf model
for (i in 1:length(v_optional_predictors)){
  for (j in 1:length(v_target)){
    
    target_i <- v_target[j] ; print(target_i)
    var_name_i <- v_optional_predictors[i] ; print(var_name_i)
    var_i_full <- l_vars[[var_name_i]][['label']] ; print(var_i_full)
    
    # select only relevant predictors and the identifiers
    v_all_vars <- c(target_i, var_name_i, v_predictors)  ; print(v_all_vars)
    
    # load rf model
    # load( paste0(input_dir_rf, 'rf_model_div-' ,var_name_i, '_targ-', target_i, '_seed-102.RData' ) ) # rf.model load the rf model for each div variable
    load( paste0(input_dir_rf, 'list_rf_model_results_parallelDiv_div-' ,var_name_i, '_targ-', target_i, '.RData' ) ) # after trimming gedi points
    
    # load all data
    load(paste0(input_dir_rf, 'df_all_div-', var_name_i, '_targ-', target_i, '.RData') )
    
    # # initialize train/test df from the all data
    df_comb.train_i <- subset(df_comb_i, train_sample == T) # head(df_comb.train_i)
    df_comb.test_i  <- subset(df_comb_i, train_sample == F) # head(df_comb.test_i)
    
    # create the pdps using the training testing or all data
    if(s_train_test_all == 'train'){ df_pdp <-  df_comb.train_i[complete.cases(df_comb.train_i), ] ; print('using train data') }
    if(s_train_test_all == 'test') { df_pdp <-  df_comb.test_i[complete.cases(df_comb.test_i), ]   ; print('using test data')  }
    if(s_train_test_all == 'all')  { df_pdp <-  df_comb_i[complete.cases(df_comb_i), ]             ; print('using all data')   }
    
    # add the bgr regions
    df_comb_i_bgr <- left_join(df_pdp, df_var)
    df_comb_i_bgr <- df_comb_i_bgr[complete.cases(df_comb_i_bgr), ]
    
    # add two bands with the x, y coordinates (needed to recover x,y potentially lost because of rounding in between df-raster-df conversion)
    # df_comb_i$lon <- df_comb_i$x
    # df_comb_i$lat <- df_comb_i$y
    
    #### RUN PDPS BY BIOGEOGRAPHICAL REGION ####
    
    # identify unique KG classes to loop over
    bgr <- sort(unique(df_comb_i_bgr[['BiogeoRegions2016']])) ; print(bgr)
    
    # run through every BGR and run PDP separately by classes
    for (r in 1:length(bgr)){
      
      # select bgr
      bgr_region <- bgr[r]  ; print(bgr_region)
      
      # select from the df only the data points belonging to the selected bgr
      df_comb_i_bgr_r <- df_comb_i_bgr[df_comb_i_bgr[['BiogeoRegions2016']] == bgr_region, ]
      
      # select only relevant predictors without the identifiers to run in model
      df_comb_i_bgr_r_pdp <- df_comb_i_bgr_r[, v_all_vars]
      
      # increase the t2m column of 1.5 deg
      # df_comb_i_bgr_r_pdp_p15t2m <- df_comb_i_bgr_r_pdp
      # df_comb_i_bgr_r_pdp_p15t2m$t2m_mean <- df_comb_i_bgr_r_pdp_p15t2m$t2m_mean + 1.5
      
      # run the pdp over the subselection of datapoints
      pp_i <- partialPlot(rf.model, df_comb_i_bgr_r_pdp, names(df_comb_i_bgr_r_pdp)[2])
      # pp_i_p15t2m <- partialPlot(rf.model, df_comb_i_bgr_r_pdp_p15t2m, names(df_comb_i_bgr_r_pdp_p15t2m)[2])
      
      # convert pdp to df and round digits
      pp_i <- as.data.frame(pp_i);  #pp_i[1:2] <- pp_i[1:2] %>% round( digits = 5)
      # pp_i_p15t2m <- as.data.frame(pp_i_p15t2m);  #pp_i_p15t2m[1:2] <- pp_i_p15t2m[1:2] %>% round( digits = 5)
      
      # change names of pdp df
      names(pp_i) <- c("variable", "resilience")
      # names(pp_i_p15t2m) <- c("variable", "resilience")
      
      # save pdp df
      save(pp_i, file=paste0(output_path, 'df_rf_model_partDep_div-', var_name_i, '_targ-', target_i, '_bgr-', bgr_region,  '.RData'))
      # save(pp_i_p15t2m, file=paste0(output_path, 'df_rf_model_partDep_div-', var_name_i, '_targ-', target_i, '_bgr-', bgr_region, '_p1.5t2m.RData'))
      
      # # select limits
      # if(target_i=='kndvi_TAC'){
      #   ylims <- c(0.2, 0.5)
      # } 
      # else {
      #   ylims <- c(-1.45, -1)
      # }
      # 
      # # plot pdp
      # ggp <- ggplot(pp_i, aes(x = variable, y = kNDVI_TAC)) +    
      #   geom_line(size = 1) +
      #   #xlab(var_i_full) + 
      #   #ylab('kNDVI TAC') +
      #   ylim(ylims) +
      #   labs(x = var_i_full, y = 'kNDVI TAC', title = paste0(var_i_full, ' PDP in BGR ', bgr_region)) 
      # 
      # # save plot of pdp
      # ggsave(filename = paste0('df_rf_model_partDep_', var_name_i, '_targ-', target_i, '_bgr-', bgr_region,  '.png'), plot = ggp, path = output_path, width = 5, height = 5)
      # 
    }
    
    #### AGGREGATE DATA AT COARSER RESOLUTION ####
    
    # # turn the df into a raster in order to aggregate
    # raster_i <- rasterFromXYZ(df_comb_i, res=c(0.05,0.05), crs="EPSG:4326")
    # 
    # # create a coarser version of the dataset
    # raster_i_coarse <- aggregate(raster_i, fact = 10, fun = mean)
    # 
    # # convert raster back to df and retrieve correct x,y copied previously as lon,lat
    # coarse_df <- terra::as.data.frame(raster_i_coarse, xy = T, na.rm = TRUE)
    # 
    # # remove not useful columns
    # coarse_df_ice <- coarse_df %>% select(-c("x", "y", "row_id", "lon", "lat"))
    # 
    # # load rf model
    # load( paste0(input_dir_rf, 'rf_model_div-' ,var_name_i, '.RData' ) ) # rf.model load the rf model for each div variable
    # 
    # # create an ice object from the rf model and the coarse df
    # rf.ice <- ice(object = rf.model, X = coarse_df_ice, y = coarse_df_ice[[v_target]], predictor = var_name_i,
    #               indices_to_build = ) #,
    #               # predictfcn = function(object, newdata){
    #               #   predict(object, newdata, type = "prob")[, 2]
    #               # })
    # 
    # # as a temporary precaution save these blocks of ice objects
    # save(rf.ice, file=paste0(output_path, 'df_ice-',var_name_i, '_coarse_iceObj.RData')) 
    # 
    # # create a derivative ice object (dice)
    # rf.dice <- dice(rf.ice)
    # 
    # # now bind the dice output of the derivative at the point (local derivative)
    # df_rf.dice <- as.data.frame(rf.dice$actual_deriv) 
    # names(df_rf.dice) <- 'actual_deriv'
    # df_rf.dice_xy <- cbind(coarse_df, df_rf.dice)
    # #df_rf.dice <- cbind(rf.ice$Xice, df_rf.dice)
    # save(df_rf.dice_xy, file=paste0(output_path, 'df_dice-',var_name_i, '_coarse_diceObj.RData')) 
    # 
    # # plot
    # lims_h_in <- c(-0.05, 0.02) ; lims_m_in <- c(-0.01, 0.01)
    # 
    # # make hist and map (save)
    # h_dist <- make_hist(df_rf.dice_xy, 'actual_deriv', 'ICE partial derivative', lims_h_in)
    # g_input <- make_map(df_rf.dice_xy, 'actual_deriv', 'ICE partial derivative (actual)', var_name_i, lims_m_in)
    # ggsave(plot = g_input, filename = paste0(output_path, 'm_', 'dICE' ,'_', var_name_i, '.png' ) ) # , width = wid, height = hei)
    # 
    # g_draw <- f_combine_map_hist(g_input, h_dist)
    # ggsave(plot = g_draw, filename = paste0(output_path, 'g_dICE_comb_', var_name_i, '.png' ) , width = fig_width, height = fig_height ) # , width = wid, height = hei)
  
  }
}