# ########################################################
# Title         : plotRF_partialPlot_plotDF_original.R
# Description   : plot the partial plot dataframes created by plotRF_partialPlot_createDF..R
#                 plot RF output to analyse performance and, in particular, variability of the 
#                 model to different diversity metrics 
# Aims          : analyse performance and diversity metrics
# Inputs	      : df of partial plot values
# Outputs	      : 
#                 figures for each diversity: 1) performance 2) Importance 3) dice 
#                 summary figures: 1) partial plot of diversity 2) ranked average Importance 3) Importance of diversity metrics
# Options	      : 
# Date          : 21/12/23
# Version       : 1
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : Could extend with other metrics as well as partial plots of other variables
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())            

script_subtitle <- 'partialPlot'
# load RF plotting functions
source('3_create_figures/functions/f_plotRF_initialise.R')
library(purrr)  # if using map()
# load the pdp file to plot
# load(input_plot_pdp) # summary(df_partDep_div)

###################################################
###### PARTIAL DEPENDENCE GROUP PLOTTING 1D   #####
###################################################

# we have the dataframes of partial dependence 
### Now plot the diversity figs
# load('/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_2023-11-29_partialPlot/df_rf_model_partDep_diversity_data-train.RData')
# if(b_run_partialplot_nonDiv) pdp_plot <- df_partDep_nondiv
# if(b_run_partialplot       ) pdp_plot <- df_partDep_div

# load dataframes of variables containing test/train split
# load df containing all test train data
load( paste0(input_dir, 'df_all', '.RData') )        #summary( df_comb )      head(df_comb)

# # initialise train/test df
df_hists <- f_plotRF_load_RFDF(df_comb, s_plot_pdp_dataset) ;
# put temperature in C 
if (b_t2m_in_C  ) {
  df_hists$t2m_mean <- df_hists$t2m_mean - 273.15 
}
if(b_invert_mu_kurt ){
  df_hists$mu_kurt <- -1 * df_hists$mu_kurt
  print('invert mu_kurt')
}

###################################################
###### PLOT ALL THE DIV METRICS - BOTH LAMBDA   #####
###################################################

###################################################
######     PARTIAL DEPENDENCE LOADING 2D      #####
###################################################
# first we ensure we have the df containing all 2d pdps for all variable combinations saved

# initialise a new 2d pdp df if not present or empty
if ( !exists("df_pdp_2d") || (exists("df_pdp_2d") && dim(df_pdp_2d)[1] == 0) ) {
  df_pdp_2d <- data.frame() ; print('create df_pdp_2d')

  # loop over 'resilience target metrics' 
  for (k in 1:length(v_target)){ # k <- 2
    target_name_k <- v_target[k] ; print(target_name_k)
  
  # loop over the different diversity metrics & climate combinations and if present add to df
    for (x in 1:length(v_predictors)) {        # x <-  8
    var_name_x <- v_predictors[x] ; print(var_name_x)  
    for (y in 1:length(v_optional_predictors)) { # y <- 1
      var_name_y <- v_optional_predictors[y] ; print(var_name_y)

      # Construct the filename and check if the file exists
      # file_path <- paste0(dir_input_2d_pdps, "df_pdp_2d-", var_name_y, "-", var_name_x, ".RData")
      file_path <- paste0(dir_input_2d_pdps, "df_pdp_2d-", var_name_y, "-", var_name_x, "_targ-", target_name_k , ".RData")
      
      if( file.exists(file_path) ) {
        # Load the file and add the loaded data frame to the list
        load(file_path) # head(pdp_2d)
        
        # # load data for histogram
        # # load dataframes of variables containing test/train split
        # s_name_df_comb <-  paste0(input_dir, 'df_all_div-', var_name_y, '_targ-', target_name_k, '.RData')
        # load( s_name_df_comb )        # df_comb_i      head(df_comb_i)
        # # # initialise train/test df and df to use in analysis - select only those needed
        # df_hists <- f_plotRF_load_RFDF(df_comb_i, s_plot_pdp_dataset) ;
        # # head(df_hists)
        
        # align dataframe with others
        pdp_2d$diversity_metric <- var_name_y ; pdp_2d$predictor_metric <- var_name_x
        pdp_2d$resilience_metric <- target_name_k
        pdp_2d$diversity_metric <- as.factor(pdp_2d$diversity_metric)
        pdp_2d$predictor_metric <- as.factor(pdp_2d$predictor_metric)
        pdp_2d$resilience_metric <- as.factor(pdp_2d$resilience_metric)
        names(pdp_2d)[1] <- 'x_vals' ;  names(pdp_2d)[2] <- 'y_vals'
        names(pdp_2d)[3] <- 'yhat'
        
        # join to combined df
        df_pdp_2d <- rbind(df_pdp_2d, pdp_2d) # head(df_pdp_2d)
        
        ###################################################
        ######     PARTIAL DEPENDENCE PLOTTING 2D     #####
        ###################################################
        # head(pdp_2d) ; 
        summary(pdp_2d)
        # g_pp <- plotPartial(pdp_2d_old, contour = T, levelplot = T, rug = T, train = df_pdp) # g_pp
        # manually set colors
        # col.regions <- grDevices::terrain.colors() # hcl.colors(1000)
        
        # set the x-limits of the variable
        lims_y <- l_lims_in[[var_name_y]]
        lims_x <- l_lims_in[[var_name_x]] # l_lims_in_pred - previously a separate list
        lims_z <- y_lims_lambda_2d  # y_lims_pdp[[target_name_k]]
        z_lab_i_lambda <- l_lables_metrics[[target_name_k]] 
        x_lab_i        <- l_lables_metrics[[var_name_x]] 
        y_lab_i        <- l_lables_metrics[[var_name_y]] 
        var_name_i_filename <- var_name_y
        
        # put temperature in C 
        if (b_t2m_in_C & var_name_x == "t2m_mean"  ) {
          pdp_2d$x_vals <- pdp_2d$x_vals - 273.15 
          lims_x <- lims_x  - 273.15 - 0.85
          x_lab_i        <- str_replace( l_lables_metrics[[var_name_x]] , 'K',  '°C')
          # df_hists$t2m_mean <- df_hists$t2m_mean - 273.15 
        }
        # invert the restoration rate
        if( b_useAbs_RestRate  ){
          print('invert rest rate')
          # pdp_2d$yhat <- -1 * pdp_2d$yhat 
          # pdp_2d$resilience_metric <- -1 * pdp_2d$resilience_metric
          s_direction_rr <- -1              # invert the color scheme so more resilience is blue 
          z_lab_i_lambda <- paste0( '|', l_lables_metrics[[target_name_k]] , '|' )
          pdp_2d <- pdp_2d %>% mutate(
            yhat = if_else(resilience_metric %in% c("kndvi_lambda_variance", "kndvi_lambda_xt"), yhat * -1, yhat)
            # res_name = if_else(resilience_metric %in% c("kndvi_lambda_variance", "kndvi_lambda_xt"), paste0( '|',  l_lables_metrics[[resilience_metric]], '|') , l_lables_metrics[[resilience_metric]])
            # res_name = map_chr(resilience_metric, ~ if_else(. %in% c("kndvi_lambda_variance", "kndvi_lambda_xt"),
                                                            # l_lables_metrics[[resilience_metric]], "0"))
              # if_else(resilience_metric %in% c("kndvi_lambda_variance", "kndvi_lambda_xt"), as.character(l_lables_metrics[resilience_metric]), 0)
          )
        }
        if(b_invert_mu_kurt & var_name_y== 'mu_kurt'){
          pdp_2d$y_vals <- -1 * pdp_2d$y_vals
          var_name_i_filename <- paste0(var_name_y, '_inv')
          # y_lab_i <- paste0(l_lables_metrics[[var_name_y]], ' (inverse)')
          lims_y <- - lims_y[c(2, 1)]
          print('invert mu_kurt')
        }
        
        
        g_pdp <- ggplot(pdp_2d, aes(x = x_vals, y = y_vals, fill = yhat)) +
          geom_raster(interpolate = TRUE) +
          geom_contour(aes(z = yhat), color = "white") +     # add contour lines
          # scale_fill_gradientn(colors = col.regions) +     # set own color scale
          scale_fill_viridis_c(name = z_lab_i_lambda, option = "H", limits = lims_z, direction = s_direction_rr) + # H for turbo rainbow cols
          theme_minimal() +
          scale_y_continuous(limits = lims_y  , labels = fixed_width_labels(12) )  +  # fixed_width_labels fixes y-margin    # if(length(lims_x) ==2)
          scale_x_continuous(limits = lims_x  )  +         # if(length(lims_x) ==2)
          # basic_fig_theme  +
          basic_graph_theme +
          theme(legend.position = "bottom", legend.direction = "horizontal",
                legend.background = element_blank(),  # Remove legend background
                legend.box.background = element_blank(),  # Remove legend box background
                legend.key = element_blank(),  # Remove the boxes around the symbols/keys in the legend                # axis.text.y = element_text(size = 10),
                legend.key.width = unit(legend_width*1.8, 'cm'), #change legend key width
                legend.key.size = unit(legend_size*1.2, 'cm') ) + #change legend key size
          # theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),     # if aligned might be able to remove ticks
          # axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
          guides(fill = guide_colorbar(title.position = "top")
                 # hjust = 0.5 centres the title horizontally
                 # title.hjust = 0.5,
                 # label.position = "bottom")
          ) +
          labs( #title = "2D Partial Dependence Plot",
            x = x_lab_i, # "", # x = "t2m_mean",
            y = y_lab_i, # "", # y = "fhd_mean",
            fill = target_name_k
            # fill = l_lables_metrics[["kndvi_lambda" ]]
            ) # +
        # theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
         g_pdp
        
        # Extract the legend and remove
        g_pdp <- g_pdp + theme(legend.position = "top")
        legend_grob <- cowplot::get_legend(g_pdp)
        legend_grob = cowplot::get_plot_component(g_pdp, 'guide-box-top', return_all = TRUE)
        
        g_pdp <- g_pdp + theme(legend.position = "none")
        
        ggsave(filename = paste0( 'g_pdp_2d_', var_name_i_filename, '-',  var_name_x , '_targ-', target_name_k, '_absRR-', b_useAbs_RestRate , '.png') , 
               plot = g_pdp, path = output_path, width = fig_width_wide, height = fig_height_wide)
        ggsave(filename = paste0( 'g_legend_', var_name_i_filename, '-',  var_name_x , '_targ-', target_name_k, '_absRR-', b_useAbs_RestRate , '.png') , 
               plot = legend_grob, path = output_path, width = fig_width_wide, height = fig_height_wide/3)
        
        
        g_hist_y <- f_plot_partial_hist(df_hists, var_name_y, n_bins_x = n_bins/2, lims_x = lims_y, coord_flip = T )
        g_hist_x <- f_plot_partial_hist(df_hists, var_name_x, n_bins_x = n_bins/2, lims_x = lims_x )
        
        # save 1d marginal
        # g_pdp_hist <- grid.arrange(g_pdp, g_hist_x, ncol = 1, heights = c(3, 1))
        
        g_pdp_hist <- grid.arrange(g_hist_y, g_pdp, legend_grob, g_hist_x, nrow = 2, ncol = 2, widths = c(1, 3), heights = c(3, 1))
        ggsave(filename = paste0( 'g_pdp_2d_', var_name_i_filename, '-',  var_name_x , '_targ-', target_name_k, '_absRR-', b_useAbs_RestRate , '_hist.png') , plot = g_pdp_hist, path = output_path, width = fig_width_wide, height = fig_height_wide)
      
      }
    } # close loop over predictors x
  } # close loop over diversity vars, y
  
  # df_pdp_2d$diversity_metric <- as.factor(df_pdp_2d$diversity_metric)
  # df_pdp_2d$predictor_metric <- as.factor(df_pdp_2d$predictor_metric)
  head(df_pdp_2d) ; summary(df_pdp_2d)
  # head( df_pdp_2d %>% filter(predictor_metric == 't2m_mean') )
  
} # end loop over k res metrics

# save the final df
save(df_pdp_2d, file=paste0(dir_input_2d_pdps, 'df_pdp_2d-all', '_targ-', target_name_k, '.RData' )    )

} # close initial if statement



#######
# ggExtra::ggMarginal(p, type = "histogram")
# # from documentation
# form <- stats::as.formula(    paste("yhat ~", paste(names(pdp_2d)[1L:2L], collapse = "*")  )  )
# col.regions <- grDevices::terrain.colors() # hcl.colors(1000)
# levelplot(
#   form, data = pdp_2d, col.regions = col.regions)
# 
# # my attempt
# p <- ggplot(pdp_2d, aes(x = var_name_x, y = var_name_y, z = yhat) ) + 
#   # scale_fill_gradient(low = "green", high = "red") +
#   scale_fill_viridis_c() +
#   # scale_fill_gradientn(colors = col.regions) +
#   scale_y_continuous(limits = lim_fhd_mean ) + # scale_y_continuous(limits = lims_y ) +
#   # geom_point() +
#   geom_tile() +
#   # geom_raster() +
#   # geom_contour() +
#   theme_classic()
# p


