# ########################################################
# Title         : plotRF_partialPlot_plotDF_2d.R
# Description   : plot the partial plot dataframes created by plotRF_partialPlot_createDF..R
# Aims          : create partial plots
# Inputs	      : df of partial plot values
# Outputs	      : partial plot of diversity (2D)
# Options	      : 
# Date          : 2025-02-19
# Authors       : Mark Pickering & Agata Elia
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


###################################################
###### PARTIAL DEPENDENCE GROUP PLOTTING 1D   #####
###################################################


# load dataframes of variables containing test/train split
load( paste0(input_df, 'df_all', '.RData') )        # summary( df_comb )      head(df_comb)

# # initialise train/test df
df_hists <- f_plotRF_load_RFDF(df_comb, s_plot_pdp_dataset) ;
# put temperature in C 
if (b_t2m_in_C  ) {
  df_hists$t2m_mean <- df_hists$t2m_mean - 273.15 
}
if(b_invert_mu_kurt ){
  df_hists$Kurtosis <- -1 * df_hists$Kurtosis
  print('invert Kurtosis')
}

###################################################
######     PARTIAL DEPENDENCE LOADING 2D      #####
###################################################
# first we ensure we have the df containing all 2d pdps for all variable combinations saved

# initialise a new 2d pdp df if not present or empty
if ( !exists("df_pdp_2d") || (exists("df_pdp_2d") && dim(df_pdp_2d)[1] == 0) ) {
  df_pdp_2d <- data.frame() ; print('create df_pdp_2d')

  # loop over 'resilience target metrics' 
  for (k in 1:length(v_target)){ 
    target_name_k <- v_target[k] ; print(target_name_k)
  
  # loop over the different diversity metrics & climate combinations and if present add to df
    # loop over the x-axis metrics (e.g. t2m_mean)
    for (x in 1:length(v_predictors)) {        
    var_name_x <- v_predictors[x] ; print(var_name_x)  
    for (y in 1:length(v_optional_predictors)) { 
      var_name_y <- v_optional_predictors[y] ; print(var_name_y)

      # Construct the filename and check if the file exists
      file_path <- paste0(dir_input_2d_pdps, "df_pdp_2d-", var_name_y, "-", var_name_x, "_targ-", target_name_k , ".RData")
      
      if( file.exists(file_path) ) {
        # Load the file and add the loaded data frame to the list
        load(file_path) # head(pdp_2d)
        
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
        summary(pdp_2d)

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
          )
        }
        if(b_invert_mu_kurt & var_name_y== 'Kurtosis'){
          pdp_2d$y_vals <- -1 * pdp_2d$y_vals
          var_name_i_filename <- paste0(var_name_y, '_inv')
          # y_lab_i <- paste0(l_lables_metrics[[var_name_y]], ' (inverse)')
          lims_y <- - lims_y[c(2, 1)]
          print('invert Kurtosis')
        }
        
        
        g_pdp <- ggplot(pdp_2d, aes(x = x_vals, y = y_vals, fill = yhat)) +
          geom_raster(interpolate = TRUE) +
          geom_contour(aes(z = yhat), color = "white") +     # add contour lines
          scale_fill_viridis_c(name = z_lab_i_lambda, option = "H", limits = lims_z, direction = s_direction_rr) + 
          theme_minimal() +
          scale_y_continuous(limits = lims_y  , labels = fixed_width_labels(12) )  + 
          scale_x_continuous(limits = lims_x  )  +    
          basic_graph_theme +
          theme(legend.position = "bottom", legend.direction = "horizontal",
                legend.background = element_blank(),  # Remove legend background
                legend.box.background = element_blank(),  # Remove legend box background
                legend.key = element_blank(),  # Remove the boxes around the symbols/keys in the legend                
                legend.key.width = unit(legend_width*1.8, 'cm'), #change legend key width
                legend.key.size = unit(legend_size*1.2, 'cm') ) + #change legend key size
          guides(fill = guide_colorbar(title.position = "top") ) +
          labs( #title = "2D Partial Dependence Plot",
            x = x_lab_i, y = y_lab_i, fill = target_name_k
            ) 
        
        # Extract the legend and remove
        g_pdp <- g_pdp + theme(legend.position = "top")
        legend_grob <- cowplot::get_legend(g_pdp)
        legend_grob = cowplot::get_plot_component(g_pdp, 'guide-box-top', return_all = TRUE)
        
        g_pdp <- g_pdp + theme(legend.position = "none")
        
        ggsave(filename = paste0( 'g_pdp_2d_', var_name_i_filename, '-',  var_name_x , 
                                  '_targ-', target_name_k, '_absRR-', b_useAbs_RestRate , '.png') , 
               plot = g_pdp, path = output_path, width = fig_width_wide, height = fig_height_wide)
        ggsave(filename = paste0( 'g_legend_', var_name_i_filename, '-',  var_name_x , 
                                  '_targ-', target_name_k, '_absRR-', b_useAbs_RestRate , '.png') , 
               plot = legend_grob, path = output_path, width = fig_width_wide, height = fig_height_wide/3)
        
        
        g_hist_y <- f_plot_partial_hist(df_hists, var_name_y, n_bins_x = n_bins/2, lims_x = lims_y, coord_flip = T )
        g_hist_x <- f_plot_partial_hist(df_hists, var_name_x, n_bins_x = n_bins/2, lims_x = lims_x )
        
      }
    } # close loop over predictors x
  } # close loop over diversity vars, y
  
  # df_pdp_2d$diversity_metric <- as.factor(df_pdp_2d$diversity_metric)
  # df_pdp_2d$predictor_metric <- as.factor(df_pdp_2d$predictor_metric)
  head(df_pdp_2d) ; summary(df_pdp_2d)
  # head( df_pdp_2d %>% filter(predictor_metric == 't2m_mean') )
  
} # end loop over k res metrics

# save the final df containing multiple vars
save(df_pdp_2d, file=paste0(dir_input_2d_pdps, 'df_pdp_2d-all', '_targ-', target_name_k, '.RData' )    )

} # close initial if statement


