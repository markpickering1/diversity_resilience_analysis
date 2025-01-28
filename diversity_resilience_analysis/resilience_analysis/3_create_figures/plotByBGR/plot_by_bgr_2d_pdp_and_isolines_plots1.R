# ########################################################
# Title         : plot_by_bgr_2d_pdp_and_isolines_plots1.R
# Description   : plot results of plot_by_bgr_2d_pdp_and_isolines.R
#                 specifically the 2d pdp by bgr and the inidividual isoline by bgr
# Aims          :
# Inputs	      : 
# Outputs	      : 
# Options	      : 
# Date          : 
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
library(pdp)
library(tidyr)
library(ggplot2)
library(gridExtra)


###################################################
######       I/O                              #####
###################################################

# output location
# set/create output directory
# full_date <- '2024-01-31'
output_path <- paste0(root_data_figs, script_output_ext, '_2d_pdp_and_isolines_', full_date,  '/')
output_path_plots <- paste0(root_data_figs, script_output_ext, '_2d_pdp_and_isolines_', full_date,  '/plots/')

# create output if not present
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T) ; 
  print( paste0( 'creating output dir for figures of dataframe cols : ', output_path ) ) }
print(paste0('output_path is : ', output_path ))

# create output if not present
if(! dir.exists(output_path_plots)) {dir.create(paste0(output_path_plots),recursive=T) ; 
  print( paste0( 'creating output dir for figures of dataframe cols : ', output_path_plots ) ) }
print(paste0('output_path_plots is : ', output_path_plots ))

# copy configuration input variables to output for storage
if( ! file.exists( paste0(output_path, script_config_file ) ) ) { print('copy config file') 
  file.copy(from= paste0( script_config_dir, script_config_file) , to= paste0(output_path, script_config_file ), 
            overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
} else{ print('could not copy config file') }

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
    #load( paste0(input_dir_rf, 'rf_model_div-' ,var_name_i, '_targ-', target_i, '_seed-102.RData' ) ) # rf.model load the rf model for each div variable
    #load( paste0(input_dir_rf, 'list_rf_model_results_parallelDiv_div-' ,var_name_i, '_targ-', target_i, '.RData' ) ) # after trimming gedi points
    
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
    df_pdp_bgr <- left_join(df_pdp, df_var)
    df_pdp_bgr <- df_pdp_bgr[complete.cases(df_pdp_bgr), ]
    
    #### PLOT PDPS BY BIOGEOGRAPHICAL REGION ####
    
    # select labels
    if(target_i=='kndvi_TAC'){
      tlabs <- 'kNDVI TAC'
    } else if (target_i =='kndvi_lambda_variance') {
      tlabs <- "kNDVI lambda var"
    } else {
      tlabs <- 'kNDVI lambda xt'
    }
    
    # identify unique KG classes to loop over
    # bgr <- c(1, 4, 7, 9, 11, 12)
    # colors <- c("blue", "darkgreen", "cyan", "red", "lightgreen", "orange")
    # regions <- c("alpine", "atlantic", "continental", "mediterranean", "pannonian", "steppic")

    # identify unique KG classes to loop over
    bgr <- c(1, 7, 9)
    colors <- c("blue", "cyan", "red")
    regions <- c("alpine", "continental-atlantic", "mediterranean")
    
    # run through every BGR and run PDP separately by classes
    for (r in 1:length(bgr)){
      
      # select bgr
      bgr_region <- bgr[r]  ; print(bgr_region)
      
      # select from the df only the data points belonging to the selected bgr
      df_pdp_bgr_r <- df_pdp_bgr[df_pdp_bgr[['BiogeoRegions2016']] == bgr_region, ]
      
      # create histograms
      h_dist_t2m <- ggplot(df_pdp_bgr_r, aes_string(x = df_pdp_bgr_r$t2m_mean )) +
        geom_histogram( bins = 100/2, colour = "black", fill='black') + 
        labs( y= paste0( 'Frequency'), 
              x= paste0(  't2m (mean)'    ) ) + 
        basic_hist_theme 

      h_dist_div <- ggplot(df_pdp_bgr_r, aes_string(x = df_pdp_bgr_r[[var_name_i]] )) +
        geom_histogram( bins = 100/2, colour = "black", fill='black') + 
        labs( y= paste0( 'Frequency'), 
              x= paste0(  var_i_full    ) ) + 
        basic_hist_theme 
      
      # save histograms
      ggsave(plot = h_dist_t2m, filename = paste0(output_path_plots, 'df_pdp_hist-t2m_mean',  '_targ-', target_i, '_bgr-', bgr_region,  '.png'), width = 10, height = 5)
      ggsave(plot = h_dist_div, filename = paste0(output_path_plots, 'df_pdp_hist-', var_name_i, '_targ-', target_i, '_bgr-', bgr_region,  '.png'), width = 10, height = 5)

      # load df with averages temperature and diversity
      load(file=paste0(output_path, 'df_all_div-', var_name_i, '_targ-', target_i, '_bgr-', bgr_region, '_mean_t2m_div_res.RData'))
      
      # identify mean t2m and mean div
      df_pdp_bgr_r_mean
      mean_t2m_mean <- df_pdp_bgr_r_mean[1, 1]; mean_t2m_mean
      mean_div <- df_pdp_bgr_r_mean[1, 3]; mean_div
      mean_res <- df_pdp_bgr_r_mean[1, 2]; mean_res
      median_t2m_mean <- df_pdp_bgr_r_mean[1, 4]; median_t2m_mean
      median_div <- df_pdp_bgr_r_mean[1, 6]; median_div
      median_res <- df_pdp_bgr_r_mean[1, 5]; median_res
      
      # define increase
      if (bgr_region==9){
        t_upper_inc <- 1
      } else {
        t_upper_inc <- 0.5
      }
      
      # create a list of temperature and diversity values between based on the median to plot 2d pdp (it will include the mean as well)
      t2m_list <- seq(df_pdp_bgr_r_mean[1, 4]-0.5, df_pdp_bgr_r_mean[1, 8]+t_upper_inc, by = 0.1); t2m_list; length(t2m_list)
      div_list <- seq(df_pdp_bgr_r_mean[1, 6]-5, df_pdp_bgr_r_mean[1, 6]+10, by = 0.1); div_list; length(div_list); length(t2m_list)*length(div_list)        
      
      # run through the extra vars and create 2d pdp
      for (l in 1:length(pdp_2d_extra_vars)){
        
        # select second independent var for 2d pdp
        pdp_2d_var_l <- pdp_2d_extra_vars[l] 
        print(paste0('2d pdp plot between ', var_name_i, ' and ', pdp_2d_var_l) )

        # load df with 2d pdp correspondingto the median (just to plot)
        load(paste0(output_path, 'df_pdp_2d-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '_median.RData'))
        
        # create partial plot
        # g_pdp1 <- plotPartial(pdp_2d_i, contour=TRUE); plot(g_pdp1)
        # pdf(paste0(output_path_plots, 'g_pdp_2d-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region,  '.pdf'))  
        # print(g_pdp1)
        # dev.off()
        
        #### TRY PLOT 2D PDP PLUS HISTOGRAMS ####
        
        g_pdp <- ggplot(pdp_2d_i, aes(x = t2m_mean, y = !!sym(var_name_i), fill = yhat)) +
          geom_raster(interpolate = TRUE) +
          geom_contour(aes(z = yhat), color = "white") +
          scale_fill_viridis_c(option = "H", limits = c(min(pdp_2d_i$yhat), max(pdp_2d_i$yhat))) +
          theme_minimal() +
          scale_x_continuous(limits = c(min(pdp_2d_i$t2m_mean), max(pdp_2d_i$t2m_mean)), breaks=waiver(), n.breaks=10)  +
          scale_y_continuous(limits = c(min(pdp_2d_i[[var_name_i]]), max(pdp_2d_i[[var_name_i]])), breaks=waiver(), n.breaks=10)  +
          theme(legend.position = "bottom", legend.direction = "horizontal",
                legend.key.size = unit(legend_size, 'cm') ) +
          guides(fill = guide_colorbar(title.position = "top")
          ) +
          labs(#title = paste0('2D PDP of t2m (mean) and ', var_i_full, ' in ', regions[r], ' region'),
            x = 't2m (mean)',
            y = var_i_full,
            fill = tlabs) 
        
        # create df corresponding to mean/median t2m and div of bgr in the 2d pdp to add as points
        pdp_2d_i_target_res_mean <- data.frame(col1 = df_pdp_bgr_r_mean[1, 1], col2 = df_pdp_bgr_r_mean[1, 3], col3 = 0)
        colnames(pdp_2d_i_target_res_mean) <- c('t2m_mean', var_name_i, 'yhat')
        pdp_2d_i_target_res_median <- data.frame(col1 = df_pdp_bgr_r_mean[1, 4], col2 = df_pdp_bgr_r_mean[1, 6], col3 = 0)
        colnames(pdp_2d_i_target_res_median) <- c('t2m_mean', var_name_i, 'yhat')
    
        # add point to plot
        g_pdp <- g_pdp +
          geom_point(data = pdp_2d_i_target_res_mean, aes(x = t2m_mean, y = !!sym(var_name_i)), color = "white", size = 1) +
          geom_text(data = pdp_2d_i_target_res_mean, aes(x = t2m_mean, y = !!sym(var_name_i), label = 'MEAN'), color = "white", vjust = -0.5, hjust = 0.5, size = 3) 
  
        g_pdp <- g_pdp +
          geom_point(data = pdp_2d_i_target_res_median, aes(x = t2m_mean, y = !!sym(var_name_i)), color = "white", size = 1) +
          geom_text(data = pdp_2d_i_target_res_median, aes(x = t2m_mean, y = !!sym(var_name_i), label = 'MEDIAN'), color = "white", vjust = -0.5, hjust = 0.5, size = 3) 
        
        g_pdp
        
        # extract the legend and remove
        legend_grob <- cowplot::get_legend(g_pdp)
        g_pdp <- g_pdp + theme(legend.position = "none")
        
        # create histograms
        df_pdp_bgr_r_subset <- subset(df_pdp_bgr_r, t2m_mean >= t2m_list[1] & t2m_mean <= t2m_list[length(t2m_list)])
        h_dist_t2m <- ggplot(df_pdp_bgr_r_subset, aes_string(x = df_pdp_bgr_r_subset$t2m_mean)) +
          geom_histogram( bins = 100/2) + 
          labs( y= paste0('Frequency'), 
                x= paste0('t2m (mean)')) + 
          #theme_classic() +
          theme(axis.text.y = element_text(size = 8)) +
          #scale_y_continuous(labels = fixed_width_labels(12)) +
          scale_x_continuous(limits = c(min(pdp_2d_i$t2m_mean), max(pdp_2d_i$t2m_mean)), oob=squish, breaks=waiver(), n.breaks=10)
        
        df_pdp_bgr_r_subset <- subset(df_pdp_bgr_r, get(var_name_i) >= div_list[1] & get(var_name_i) <= div_list[length(div_list)]) 
        h_dist_div <- ggplot(df_pdp_bgr_r_subset, aes_string(x = var_name_i)) +
          geom_histogram( bins = 100/2) + 
          labs( y= paste0( 'Frequency'), 
                x= paste0(var_i_full)) + 
          #theme_classic() +
          theme(axis.text.y = element_text(size = 8)) +
          #scale_y_continuous(labels = fixed_width_labels(12)) +
          scale_x_continuous(limits = c(min(pdp_2d_i[[var_name_i]]), max(pdp_2d_i[[var_name_i]])), oob=squish, breaks=waiver(), n.breaks=10)
        h_dist_div <-h_dist_div + coord_flip() 
        
        g_pdp_hist <- grid.arrange(h_dist_div, g_pdp, legend_grob, h_dist_t2m, nrow = 2, ncol = 2, widths = c(1, 3), heights = c(3, 1), top = paste0('2D PDP of t2m (mean) and ', var_i_full, ' in ', regions[r], ' region'))
        ggsave(filename = paste0('g_pdp_2d_hist-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region, '.png'), plot = g_pdp_hist, path=output_path_plots, width = 10, height = 10)
        
        #### PLOT THE ISOLINE
        
        # load the isoline df
        load(file=paste0(output_path, 'df_pdp_2d_isoline-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region, '_', 'median', '.RData'))
        
        # identify target res
        target_res_median <- isoline_df[1, 3]; target_res_median
        
        # identify limits temperature and diversity
        lims_t <- c(min(isoline_df$t2m_mean), max(isoline_df$t2m_mean)+0.2); lims_t
        lims_d <- c(min(isoline_df[[var_name_i]]), max(isoline_df[[var_name_i]])+0.5); lims_d
        
        # create plot
        png(paste0(output_path_plots, 'df_pdp_2d_isoline-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region, '.png'), width = 800, height = 800)
        
        # create a layout
        layout(mat = matrix(c(3, 0, 1, 2),
                            nrow = 2,
                            ncol = 2),
               heights = c(1, 1),
               widths = c(3, 3))
        
        # create a new blank plot
        par(mar = c(5, 4, 2, 1))
        plot(1, type = "n", xlim = lims_t, ylim = lims_d, xlab = 't2m (mean)', ylab = var_i_full, main = paste0("Isoline of ", tlabs, " in ", regions[r], ' region'))
        
        # select a color for plotting
        color <- colors[r]
        
        # plot the isoline
        lines(x=isoline_df$t2m_mean, y=isoline_df[[var_name_i]], type='l', col = color, lwd = 2, lty=1)
        
        # load the isoline df
        load(file=paste0(output_path, 'df_pdp_2d_isoline-', var_name_i, '-', pdp_2d_var_l, '_targ-', target_i, '_bgr-', bgr_region, '_', 'mean', '.RData'))
        
        # identify target res
        target_res_mean <- isoline_df[1, 3]; target_res_mean
        
        # plot the isoline
        lines(x=isoline_df$t2m_mean, y=isoline_df[[var_name_i]], type='l', col = color, lwd = 2, lty=2)
        
        # create legend item
        legend_item = c(paste0('MEDIAN \nmedian t2m = ', round(median_t2m_mean, digits = 3), '\nmedian ', var_i_full, ' = ', round(median_div, digits=3), '\nmedian ', tlabs, ' = ', round(median_res, digits=3) , '\nmodelled ', tlabs, ' = ', round(target_res_median, digits=3)),
                        paste0('MEAN \nmean t2m = ', round(mean_t2m_mean, digits = 3), '\nmean ', var_i_full, ' = ', round(mean_div, digits=3), '\nmean ', tlabs, ' = ', round(mean_res, digits=3), '\nmodelled ', tlabs, ' = ', round(target_res_mean, digits=3)))
                             
        
        # add legend
        if (r==1){
          legend("topright", legend=legend_item, col=colors[r], lty=c(1, 2), cex=1, bty = "n", y.intersp = 3)
        } else {
          legend("topleft", legend=legend_item, col=colors[r], lty=c(1, 2), cex=1, bty = "n", y.intersp = 3)
        }
        
        # create histograms
        par(mar = c(2, 4, 0, 1))
        df_pdp_bgr_r_subset <- subset(df_pdp_bgr_r, t2m_mean >= lims_t[1] & t2m_mean <= lims_t[2])
        hx <- hist(df_pdp_bgr_r_subset$t2m_mean,
             xlab = 't2m (mean)',
             ylab = "Frequency",
             col = "grey",
             border = 'grey',
             breaks = 100/2,
             xlim = lims_t,
             main = '')

        par(mar = c(5, 2, 1, 0))
        df_pdp_bgr_r_subset <- subset(df_pdp_bgr_r, get(var_name_i) >= lims_d[1] & get(var_name_i) <= lims_d[2])
        hy <- hist(df_pdp_bgr_r_subset[[var_name_i]],
                   xlab = var_i_full,
                   ylab = "Frequency",
                   col = "grey",
                   border = 'grey',
                   breaks = 100/2,
                   xlim = lims_d,
                   main= '') 
        
        layout(1)
        
        # save the plot
        dev.off()
        
      }
      
    } 

  }
  
}