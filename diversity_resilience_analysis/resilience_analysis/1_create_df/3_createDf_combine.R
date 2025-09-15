# ########################################################
# Title         : 3_createDf_combine.R
# Description   : Combine the separate dfs of the different variables
#                 
# Aims          : Merge dataframes of all data and RF predictors data for each X/Y
# Inputs	      : df_stats containing KNDVI CLIMATE (&FC) data (mu/CV/TAC) & static dataframes
# Outputs	      : single merged df
# Options	      : 
# Date          : 23/5/23
# Version       : 2 
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
script_config_dir          <- '1_create_df/input/'    
script_config_file <- 'input_createDf_combine.R'

# initialise code setup and repo building
source(main_initialisation_file)
# initialise user inputs (config) to script
source( paste0( script_config_dir, script_config_file) )

######     SET LIBRARIES                      #####
library(dplyr)   # use %>%
library(stringr)

###################################################
######       I/O                              #####
###################################################

# set/create output directory

# set/create output directory as script_output_ext if user input
output_path <- paste0(root_data_proce, script_output_ext, '/')
print(paste0('output_path is : ', output_path ))

# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for dataframes of analysis inputs : ', output_path ) ) }

# copy configuration input variables to output for storage
if( ! file.exists( paste0(output_path, script_config_file ) ) ) { print('copy config file') 
  file.copy(from= paste0( script_config_dir, script_config_file) , to= paste0(output_path, script_config_file ), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
} else{ print('could not copy config file') }
  

#######################################
##### COMBINE DATAFRAMES          #####
#######################################
# run over the selected static variables and ts statistical averages

# initialise final dataframe
df_comb   <- data.frame()

# Loop through the temporal RData objects, load them, 
# rename column names adding the variable as a prefix and join with kndvi RData object
for (i in 1:length(v_variables)){
  var_i <- v_variables[i] ; print(var_i)
  
  load( paste0(input_dir_ts, 'df_', var_i , '_muTACcov.RData' )    ) # head(df_stats)
  
  # add the name  of the variable
  df_stats <- as.data.frame(df_stats)
  colnames(df_stats)[3:length(df_stats)] <- paste0( var_i, '_',  colnames(df_stats)[3:length(df_stats)])
  df_stats[1:2] <- df_stats[1:2] %>% round( digits = 3) # this only works for 0.05deg resolution check rounding
  
  # create df or join to existing dfs
  if(i == 1){df_comb <- df_stats
  } else{ 
    df_comb <- full_join(df_comb, df_stats)    # full join and later filter
    # df_comb <- inner_join(df_comb, df_stats) # old - only include points in all variables
  }
  
  # print out the number of pixels we have
  print(dim(df_stats)) 
}

print(dim(df_comb)) 

# Loop through the static RData objects, load them, 
# rename column names adding the variable as a prefix and join with kndvi RData object
for (j in 1:length(v_variables_static)){

  var_j <- v_variables_static[j] ; print(var_j)
  load( paste0(input_dir_static, 'df_', var_j , '_baseVar_full.RData' )    ) 
  
  # remove those with zero forest cover (as this is full of NAs and unneccessarily inflates dfs)
  if ( var_j == forestcover_name ) { print('forest cover: remove 0 values')
    df_var[3][df_var[3] == 0] <- NA ; df_var <- na.omit(df_var) }
  
  # drop the rounding to lonlat rounding - probably shouldn't need this step
  df_var[1:2] <- df_var[1:2] %>% round( digits = 3) # this only works for 0.05deg check rounding
  
  df_comb <- full_join(df_comb, df_var) # df_comb_test <- inner_join(df_comb, df_var)  
  
  # print out the number of pixels we have
  print(dim(df_var)) 
}

# remove points with missing tac_kndvi variable as this is predictor
# df_comb <- df_comb %>% filter(! is.na(tac_resid_kndvi))
print(dim(df_comb)) 
head(df_comb) ; summary(df_comb) ;  names(df_comb)
# dim(na.omit(df_comb)) # test

# Save merged output - avoid overwriting by assigning random number if already present
if( ! file.exists( paste0(output_path, f_name_output_comb , ".RData")) ){ 
  save(df_comb, file=paste0(output_path, f_name_output_comb , ".RData"))
} else{ s_suffix <-  sample(1:100, 1) ; print('create new') ;
        save(df_comb, file=paste0(output_path, f_name_output_comb , "_", s_suffix ,".RData")) }

# time count
f_time_update(t_start_time)


