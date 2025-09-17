# ########################################################
# Title         : plotRF_createICE_plotICE.R
# Description   : In this script we create the Individual Conditional Expectation
# Aims          : Create ICE dataset
# Inputs	      : df_dice dataframe of derivative of ICE values from plotRF_createICE.R
# Outputs	      : derivative of ICE maps
# Date          : 2025-02-19
# Authors       : Mark Pickering & Agata Elia
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
  for (k in 1:length(v_target)){ 
    target_name_k <- v_target[k] ; print(target_name_k)
    
    # loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
    for (i in 1:length(v_optional_predictors)){ 
      var_name_i <- v_optional_predictors[i] ; print(var_name_i)
      # time count
      f_time_update(t_start_time)
      
      ###################################################
      ######    LOAD DICE                           #####
      ###################################################
      
      df_dice_direct_load <- paste0( f_ice_input ,  'df_dice-', var_name_i , '_targ-', target_name_k , '.RData' ) 
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
      if(b_invert_mu_kurt & var_name_i== 'Kurtosis'){
        df_dice$actual_deriv <- -1 * df_dice$actual_deriv 
        df_dice$Kurtosis     <- -1 * df_dice$Kurtosis
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
      # add text to the histogram top-left corner
      h_dist <- h_dist + annotate("text", x = -Inf, y = Inf, label = s_hist_annotation, 
                   hjust = -0.1, vjust = 1.2, size = 6, color = "black")
      
      # make map
      g_input <- make_map(df_dice, 'actual_deriv', axis_labels, 
                          paste0( l_label_res , ' ~ ', l_label_div ), 
                          lims_m_in,  col_palette = hcl.colors(12,"Geyser", rev = T ) ) 
                          
      # increase legend size
      g_input <- g_input + theme(legend.key.width = unit(2, 'cm'))
      g_input <- g_input +     labs( title = paste0( l_label_res , ' ~ ', l_label_div ) ) 
      
      # ggsave(plot = g_input, filename = paste0(output_path, 'm_', 'ICE' ,'_', var_name_i, '_targ-', target_name_k, '_absRR-', b_useAbs_RestRate, '.png' ) ) 

      # combine map and histogram and save
      g_draw <- f_combine_map_hist(g_input, h_dist, b_cut_title = 'F_short_europe')
      ggsave(plot = g_draw, filename = paste0(output_path, 'g_ICE_comb_', var_name_i, '_targ-', target_name_k, 
                                              '_absRR-', b_useAbs_RestRate, '.png' ) ,
             width = fig_width, height = fig_height ) 

      print( paste0('diversity variable : ', var_name_i , ' model plotting complete for res variable : ', target_name_k) )
      f_time_update(t_start_time)

    } # end loop over div metrics
  } # end loop over res metrics
  
} # end run ICE