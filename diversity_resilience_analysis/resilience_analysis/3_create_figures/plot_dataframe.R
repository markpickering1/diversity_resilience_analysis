# ########################################################
# Title         : plot_dataframe.R
# Description   : plot the columns of the combined dataframe of ts and static variables
#                 in the input_plot_dataframe.R set the parameters of the plotting
# Aims          : plot selected columns of a dataframe with 
# Inputs	      : df_comb containing various columns of data to plot
# Outputs	      : figures to a specified directory
# Options	      : 
# Date          : 2025-02-19
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
script_config_dir          <- '3_create_figures/input/'    ;   script_config_file <- 'input_plot_dataframe.R'

# initialise code setup and repo building
source(main_initialisation_file)
# initialise figure common formatting for code base
source(path_figures_init)
# initialise user inputs (config) to script
source( paste0( script_config_dir, script_config_file) )

######     SET LIBRARIES                      #####
library(dplyr)        # use %>%
# library(reshape)      # reshaping dataframes
# require(ggplot2)      # for plotting
require(scales)       # for ggplot2 functions eg oob & trans
library(RColorBrewer) # colour palettes
library(cowplot)      # for ggdraw

###################################################
######       I/O                              #####
###################################################

# output location
# set/create output directory
output_path <- paste0(root_data_figs, script_output_ext,  '/')
print(paste0('output_path is : ', output_path ))
# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for figures of dataframe cols : ', output_path ) ) }

# copy configuration input variables to output for storage
if( ! file.exists( paste0(output_path, script_config_file ) ) ) { print('copy config file') 
  file.copy(from= paste0( script_config_dir, script_config_file) , to= paste0(output_path, script_config_file ), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
} else{ print('could not copy config file') }

# load input file
load( paste0(input_dir, input_file  ) ) # head(df_comb)  ;  summary(df_comb) ;  dim(df_comb) ;   names(df_comb) ; 

# filter all column variables for containing all entries across all other variables
if(b_completeCases_for_fullDF) df_comb <- df_comb[complete.cases(df_comb), ]

# optional filter for each group of variables
if(filter_NA_by_variable){  print( paste0('only include points which are available for variable: ', filter_NA_colname))
  df_comb <- df_comb[complete.cases(df_comb[[filter_NA_colname]]), ]
}

#######################################
##### CREATE FIGURES              #####
#######################################
# this loops over the variables listed above and plots each of the variables as map and histogram
print('plotting')

for (i in 1:length(l_vars)){ # names(l_vars)
  var_i <- names(l_vars)[i] ; print(var_i)
  # extract variable information: 
  var_i_full <- l_vars[[var_i]][['label']] ; print(var_i_full)
  
  
  # select only columns containing relevant variables as the last characters in the column
  df_comb_var <- df_comb %>%   dplyr::select(x, y, starts_with(paste0(var_i) ) ) 
  # head(df_comb_var) ; dim(df_comb_var)

  # filter for each group of variables - potentially unneccessary...
  df_comb_var <- na.omit(df_comb_var)
  
  # loop over the remaining columns of dataframe - run from the 3rd column (i.e. after X,Y)
  for(j in 3:ncol(df_comb_var)){ # names(df_comb_var)
    # extract column name 
    col_name <- names(df_comb_var)[j] ; print(col_name) # paste0( stat_j, '_', var_i) ; 
    # separate from column name the group and the 'statistic'
    stat_j   <- gsub( paste0(var_i, '_'), '', colnames(df_comb_var)[j] ) ; print(stat_j) 
    # extract the units to use in the bar legend
    var_unit <- l_vars[[var_i]][[stat_j]][[3]]
    
    # apply a rescale function to the column in order change the values (e.g. temperature, precipitation )
    # only apply to those that have the variable as a function
    if ( length ( l_vars[[var_i]][[stat_j]] )  == 4 ){
      if( ! is.na( l_vars[[var_i]][[stat_j]][4] )  ) { 
        df_comb_var[col_name] <- eval( l_vars[[var_i]][[stat_j]][[4]] , df_comb_var[col_name] ) } }
    
    ## extract limits for hist and map
    # extract histogram [[1]] limits from input file containing format: l_vars[[var_name]][[stat_name]][[hist/map]]
    lims_h_i <- l_vars[[var_i]][[stat_j]][[1]] # l_all[[j]][[1]][[i]]
    # extract map [[2]] limits from input file containing format: l_vars[[var_name]][[stat_name]][[hist/map]]
    lims_m_i <- l_vars[[var_i]][[stat_j]][[2]] #  l_all[[j]][[2]][[i]]
    s_direction_rr <- TRUE # set the direction of the color scheme legend
    
    # extract the long name (variable + units) to use in the histogram
    long_name <- paste0(var_i_full, ' ', var_unit)
    # long_name <- paste0( var_unit)
    
    # should use the absolute value of restoration rate?
    if( b_useAbs_RestRate &  ( col_name == 'kndvi_lambda_xt' | col_name == 'kndvi_lambda_variance' )   ){
      var_unit <- paste0( '|',  var_unit ,  '|' )
      long_name <- paste0( var_unit)
      df_comb_var[col_name] <- -1 * df_comb_var[col_name]  # invert the scale
      lims_h_i <- - lims_h_i[c(2, 1)]   # invert the range
      lims_m_i <- - lims_m_i[c(2, 1)]
      s_direction_rr <- TRUE              # invert the color scheme so more resilience is blue/green 
      print('absolute rest rate')
    }       
    # invert diversity metrics?
    if(b_invert_mu_kurt & col_name== 'mu_kurt'){
      df_comb_var[col_name] <- -1 * df_comb_var[col_name]
      lims_h_i <- - lims_h_i[c(2, 1)]   # invert the range
      lims_m_i <- - lims_m_i[c(2, 1)]
      var_unit <- paste0(var_unit, '_inv')
      print('invert mu_kurt')
    }
    
    # if the variable has no label/limits associated with it in config input_file the apply default vals
    if ( is.null(var_i_full) ) {
      long_name <- col_name # set the label to the name of the column
      var_i_full <- col_name # set the label to the name of the column
      # set a default limits for those columns with no preset limits
      lims_h_i <- find_cutpts_from_percentile(df_comb_var, col_name, l_h_default)
      lims_m_i <- find_cutpts_from_percentile(df_comb_var, col_name, l_m_default)
      # if the col_name has 'diff' in it, then ensure the lims are symmetric
      if ( grepl("diff_", col_name) ){ 
        lims_h_i <- f_symmetric_limits(lims_h_i)
        lims_m_i <- f_symmetric_limits(lims_m_i)
      }
    } # finish dealing with null limts

    ## make histogram and map      
    # create hist
    h_dist <- make_hist(df_comb_var, col_name, long_name , lims_h_i)
    
    # create the map with title var_i_full and legend title either statistic or var_i_full (if no stat) ifelse(stat_j == col_name, var_i_full, stat_j )
    # so var unit goes on the z/fill-axis, var_i_full goes as the title (if added)
    g_input <- make_map(df_comb_var, col_name, long_name , long_name, lims_m_i, col_palette = hcl.colors(12,"Terrain", rev = s_direction_rr)) # for res

    # save map only
    if(save_map_separately) ggsave(plot = g_input, filename = paste0(output_path, 'g_', col_name, '_absRR-', b_useAbs_RestRate, '.png' ) ) # , width = wid, height = hei)

    # combine map with hist
    g_draw <- f_combine_map_hist (g_input, h_dist, b_cut_title = 'T_short_europe')

    ggsave(plot = g_draw, filename = paste0(output_path, 'g_comb_', col_name, 
                                            '_absRR-', b_useAbs_RestRate,  '.png' ) , 
           width = fig_width, height = fig_height ) 
    
  
  } # end loop over cols
}


#######################################
##### END                         #####
#######################################
