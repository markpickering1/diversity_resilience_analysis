# ########################################################
# Title         : plot_dataframe.R
# Description   : plot the columns of the combined dataframe of ts and static variables
#                 in the input_plot_dataframe.R set the parameters of the plotting
# Aims          : plot selected columns of a dataframe with 
# Inputs	      : df_comb containing various columns of data to plot
# Outputs	      : figures to a specified directory
# Options	      : 
# Date          : 2025-02-19
# Version       : 3
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
# # load common plotting functions
# source('0_main/functions/plotting_functions.R')
# initialise user inputs (config) to script
source( paste0( script_config_dir, script_config_file) )

######     SET LIBRARIES                      #####
library(dplyr)        # use %>%
library(reshape)      # reshaping dataframes
# require(ggplot2)      # for plotting
require(scales)       # for ggplot2 functions eg oob & trans
# library(ggpubr)       # for arranging ggplots together (ggarrange)
# library(ggExtra)      # more complex figures, marginal hists
# library(grid)         # for making multi-gird plots and tables
# library(gridExtra)    # for making multi-gird plots and tables
# library(lattice)      # for making multi-gird plots and tables
library(RColorBrewer) # colour palettes
# library(sf)           # utilise shapefiles etc
library(cowplot)      # for ggdraw


###################################################
######       I/O                              #####
###################################################

# output location
# set/create output directory
output_path <- paste0(root_data_figs, script_output_ext, '_', full_date,  '_corMat/')
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
##### SELECT COLUMNS TO COR       #####
#######################################
# here select only the columns that we want to check the correlations between

# only run on training data
df_comb_cor  <- subset(df_comb, train_sample == T) ; print('using train data') 

# Subset the dataframe using these valid column names
df_comb_cor <- df_comb_cor[, l_vars_cor]

dim(df_comb_cor) ; # head(df_comb_cor)

names(df_comb_cor) <- unlist(l_lables_metrics)[names(df_comb_cor)]

# # Create a logical vector where TRUE indicates a column name contains any of the strings from l_vars
# columns_to_select <- sapply(names(l_vars), function(v) {
#   grepl(v, names(df_comb))
# })
# # Since the above results in a matrix, use apply to reduce to a single logical vector with any TRUE
# columns_to_select <- apply(columns_to_select, 1, any)
# # Subset df_comb to only include columns that have a match
# df_comb_cor <- df_comb[, columns_to_select]

#######################################
##### CREATE CORRELATION MATRIX   #####
#######################################

cor_matrix <- cor(df_comb_cor[, names(df_comb_cor)])  # assuming df_train is your training dataset

melted_cor_matrix <- melt(cor_matrix)
names(melted_cor_matrix) <- c('Var1', 'Var2', 'value')

# melted_cor_matrix$Var1 <- unlist(l_lables_metrics)[as.character(melted_cor_matrix$Var1)]
# melted_cor_matrix$Var2 <- unlist(l_lables_metrics)[as.character(melted_cor_matrix$Var2)]

# reverse order
# melted_cor_matrix <- melted_cor_matrix %>% map_df(rev)

head(melted_cor_matrix)

# Plotting
g_cor <- ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
      geom_tile() +  # Create the tiles
      # geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 3) +  # Add text labels
      geom_text(aes(label = ifelse(abs(value) > 0.4, sprintf("%.2f", value), "")), color = "black", size = 4.5) + # add text if larger 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1, 1), space = "Lab", 
                           name="Correlation") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 13),  # Adjust x-axis text size and rotation
        axis.text.y = element_text(size = 13),  # Adjust y-axis text size
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 14),  # Adjust legend title size
        legend.text = element_text(size = 13) ,  # Adjust legend text size
        panel.background = element_rect(fill = "white", colour = "white"),  # Ensures the plot panel background is white
        plot.background = element_rect(fill = "white", colour = "white")    # Ensures the outer background is white
            ) +
      coord_fixed()  # Ensure the tiles are square

ggsave(plot = g_cor, filename = paste0(output_path, 'g_corMat_train_predictors_presentable.png' ) , 
       width = 20, height = 20 ) # width = fig_width_wide, height = fig_width_wide ) # 


