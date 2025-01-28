# ########################################################
# Title         : plotRF_createICE.R
# Description   : plot RF output to analyse performance and, in particular, variability of the 
#                 model to different diversity metrics
#                 In this script we create the Individual Conditional Expectation
# Aims          : analyse performance and diversity metrics
# Inputs	      : rf models with different diversity metrics in each 
# Outputs	      : figures for each diversity: 1) performance 2) Importance 3) dice 
#                 summary figures: 1) partial plot of diversity 2) ranked average Importance 3) Importance of diversity metrics
# Options	      : 
# Date          : 29/11/23
# Version       : 1
# Authors       : Mark Pickering & Agata Elia
# Maintainer    : Mark Pickering 
# Notes		      : 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

# remove previously loaded objects
rm(list = ls())            

script_subtitle <- 'createICE'
# load RF plotting functions
source('3_create_figures/functions/f_plotRF_initialise.R')


###################################################
######     LOOP OVER RESILIENCE METRICS       #####
###################################################


if(b_run_dice){ 
  
  # loop over 'resilience target metrics' producing importance figure and adding each to an div importance dataframe
  for (k in 1:length(v_target)){ # k <- 1
    target_name_k <- v_target[k] ; print(target_name_k)
    
    # loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
    for (i in 1:length(v_optional_predictors)){ # i <- 3
      var_name_i <- v_optional_predictors[i] ; print(var_name_i)# extract individual diversity predictor
    
      # time count
      f_time_update(t_start_time)
      
      ###################################################
      ######    LOAD DICE                           #####
      ###################################################
      
      df_dice_direct_load <- paste0( f_ice_input ,  'df_dice-', var_name_i , '_targ-', target_name_k , '.RData' )  # df_dice-mu_kurt_targ-kndvi_lambda_xt.RData
      load(df_dice_direct_load)
      summary(df_dice)

      l_label_res <- paste0( l_lables_metrics[[target_name_k]] )
      l_label_div <- paste0( l_lables_metrics[[var_name_i]]    )
      # should use the absolute value of restoration rate?
      if( b_useAbs_RestRate & ( target_name_k == 'kndvi_lambda_xt' | target_name_k == 'kndvi_lambda_variance' ) ){
        df_dice$actual_deriv <- -1 * df_dice$actual_deriv 
        l_label_res <- paste0( '|', l_lables_metrics[[target_name_k]], '|' )
        print('invert rest rate')
      }
      # invert diversity metrics?
      if(b_invert_mu_kurt & var_name_i== 'mu_kurt'){
        df_dice$actual_deriv <- -1 * df_dice$actual_deriv 
        df_dice$mu_kurt      <- -1 * df_dice$mu_kurt
        var_name_i <- paste0(var_name_i, '_inv')
        print('invert mu_kurt')
      }
      
      # calculate total positive and negative relationship forestarea 
      tot_neg_area <- sum(df_dice$forestarea[df_dice$actual_deriv < 0]) 
      tot_pos_area <- sum(df_dice$forestarea[df_dice$actual_deriv > 0]) 
      tot_area     <- sum(df_dice$forestarea) 
      
      perc_neg_area <- round( 100 * tot_neg_area / (tot_pos_area + tot_neg_area) , digits = 1)
      perc_pos_area <- round( 100 * tot_pos_area / (tot_pos_area + tot_neg_area) , digits = 1)
      
      tot_neg_area <- round( tot_neg_area , digits = 0)
      tot_pos_area <- round( tot_pos_area , digits = 0)
      tot_area     <- round( tot_area     , digits = 0)
      print(paste('tot_area ', tot_area))
      
      # now can make a plot
      # first select the limits 
      # hist(df_dice$actual_deriv) ; summary(df_dice$actual_deriv)
      lims_h_in <- l_dice_lims[[1]] ; lims_m_in <- l_dice_lims[[2]]
      
      # if no limits provided then use quantiles
      # if(lims_m_i == F)  lims_m_i <- quantile( df_ice_i[['actual_deriv']] , probs = c(0.15, 0.85)) ; if(lims_h_i == F)  lims_h_i <- quantile( df_ice_i[['actual_deriv']] , probs = c(0.05, 0.95))
      #paste0('ICEpartial derivative\n', l_label_res ,              ' ~ ', l_lables_metrics[[var_name_i]])
      
      axis_labels <- 'Local relationship strength' # 'ICE partial derivative'
      s_hist_annotation <- paste0('Forest area ', '\n',
                                  'Positive : ', perc_pos_area, '%\n',
                                  'Negative : ', perc_neg_area, '%')
      
      # make hist 
      h_dist <- make_hist(df_dice, 'actual_deriv', axis_labels , lims_h_in)
      # add text to the histogram
      # Add text to the top-left corner
      h_dist <- h_dist + annotate("text", x = -Inf, y = Inf, label = s_hist_annotation, 
                   hjust = -0.1, vjust = 1.2, size = 6, color = "black")
      
      # make map
      # g_input <- make_map(df_dice, 'actual_deriv', 'ICE partial derivative (actual)', var_name_i, lims_m_in)
      g_input <- make_map(df_dice, 'actual_deriv', axis_labels, 
                          paste0( l_label_res , ' ~ ', l_label_div ), 
                          lims_m_in,  col_palette = hcl.colors(12,"Geyser", rev = T ) ) # hcl.colors(12,"YlGnBu" ) )
                          # lims_m_in,  s_direction = 1)
      # increase legend size
      g_input <- g_input + theme(legend.key.width = unit(2, 'cm'))
      g_input <- g_input +     labs( title = paste0( l_label_res , ' ~ ', l_label_div ) ) #+ #, '_', gsub('_',' ', stat_j) ) ) + # label axes and title
        
      # ggsave(plot = g_input, filename = paste0(output_path, 'm_', 'ICE' ,'_', var_name_i, '_targ-', target_name_k, '_absRR-', b_useAbs_RestRate, '.png' ) ) # , width = wid, height = hei)

      g_draw <- f_combine_map_hist(g_input, h_dist, b_cut_title = 'F_short_europe')
      # combine map and histogram and save
      # g_draw <- ggdraw( clip = 'off') +
      #   draw_plot(g_input, x = 0.23, y = 0.23, width = 0.77, height = 0.77, hjust = 0.3) +
      #   draw_plot(h_dist , x = 0, y = 0,    width = 0.77, height = 0.24, hjust = 0 )
      ggsave(plot = g_draw, filename = paste0(output_path, 'g_ICE_comb_', var_name_i, '_targ-', target_name_k, 
                                              '_absRR-', b_useAbs_RestRate, '.png' ) ,
                                              # '_absRR-', b_useAbs_RestRate, '_tightLims.png' ) ,
             width = fig_width, height = fig_height ) # , width = wid, height = hei)

      
      print( paste0('diversity variable : ', var_name_i , ' model plotting complete for res variable : ', target_name_k) )
      f_time_update(t_start_time)
      
      
      ###################################################
      ######    NOW PLOT DERIVATIE WITH OTHER METRIC#####
      ###################################################
      
      if(b_create_deriv_vs_predictor_figs){
      print('create plots of derivative vs different predictors')
      # v_all_vars <- v_predictors
      v_all_vars <- c( v_optional_predictors, v_target)
      # cycle over different predictor metrics and plot derivative
      for (m in 1:length( v_all_vars ) ){ # m <- 3
        predictor_m <- v_all_vars[m] ; print(predictor_m)
        
        # set lims and labels
        l_label_predictor <- l_lables_metrics[[predictor_m]]
        l_lims_predictor  <- l_lims_in[[predictor_m]]
        if(is.null(l_lims_predictor)){ l_lims_predictor <- quantile( df_dice[[predictor_m]] , probs = c(0.15, 0.85))}
        s_title <- paste0( 'ICE partial derivative \n', l_label_res , ' ~ ', l_label_div )
        
        # create a scatter fig
        g_scat <- ggplot( df_dice , aes( !!as.symbol( predictor_m ), actual_deriv )) +    # Draw ggplot2 plot with one axis
          # geom_point(alpha = 0.01, size = 0.0005, color = 'blue') + # to show datapoints
          stat_bin2d( bins= c(n_bins,n_bins) , aes(fill = after_stat(ndensity)), na.rm = TRUE) + # ..ndensity.. ..count..
          # stat_bin2d( bins= c(n_bins,n_bins) , aes(fill = ..ndensity..), na.rm = TRUE) + # ..ndensity.. ..count..                  
          scale_fill_distiller(name = 'Relative \ndensity', direction = 1 ) +
          # geom_abline(slope=1, intercept=0, linetype = "dashed" ) +
          geom_abline(slope=0, intercept=0, linetype = "dashed" ) +
          geom_smooth(method = "lm", color = "red", se = T) +  # Adding the line of best fit
          xlim( l_lims_predictor ) + ylim( lims_m_in ) + 
          labs(title = s_title,
               y = "ICE Derivative", 
               x = l_label_predictor  ) +
          basic_graph_theme
        
        v_cor    <- round( cor(df_dice[[ predictor_m ]] , df_dice[['actual_deriv']]), digits = 3)
        s_label_stats <- paste0( 
          "N = ", dim(df_dice)[1], '\n',
          paste0("R = ", v_cor, '\n') ) # rho = \u03C1
        
        g_scat   <- g_scat + annotate("text" , x = -Inf, y = Inf, vjust = 1, # place text relative to top left
                                      # x=-0.05, y=0.8, # - relative to exact posiiton
                                      size = text_size/2, hjust = -0.05,
                                      label = s_label_stats  ) # 
        
        # print(g_scat)
        ggsave(plot = g_scat, filename = paste0(output_path, 'g_dice_scat_', var_name_i, '_targ-', target_name_k, '_absRR-', 
                                                b_useAbs_RestRate, '_derivVspredic-', predictor_m, '.png' ) , 
               width = fig_width_wide, height = fig_height_wide ) # , width = wid, height = hei)
        
          # f_obs_vs_mod_density(df_comb_predictVobs, s_title = paste0(y_lab_i, ' for ', x_lab_i), 
          #                              b_cor = T, b_mse = T, b_rmse = T,  b_mae = F, b_pbias = T,
          #                              lims_x = c_lims, lims_y = c_lims)
          # 
        
      } # end loop over v_predictors
      } # create figs vs predictors
      
    } # end loop over div metrics
  } # end loop over res metrics
  
} # end run ICE