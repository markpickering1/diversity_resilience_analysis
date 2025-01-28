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

# location of iterations to loop over
f_ice_input <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_selectionsCTT_shanUpdate_kurtInvert_2024-11-13_createICE/iter_shannon_xt/'
# join the forest area file as I forgot...
file_to_add     <- '/eos/jeodpp/data/projects/FOREST-RESILIENCE/data_processing/version_3_Aug23/2023-11-08_alignment/1_inputDataframes_sv1_2023-11-09/df_forestpixelcount_baseVar_full.RData'
load(file_to_add) # ; summary(df_var) ; dim(df_var)
df_var[1:2] <- df_var[1:2] %>% round( digits = 3) # this only works for 0.05deg check rounding
# iterate [20] over the different rfs produced for each seed to create the ICE (for now run on one seed 98)
n_iter <- 20

###################################################
######     LOOP OVER RESILIENCE METRICS       #####
###################################################


if(b_run_dice){ 
  
  # loop over 'resilience target metrics' producing importance figure and adding each to an div importance dataframe
  for (k in 1:length(v_target)){ # k <- 1
    target_name_k <- v_target[k] ; print(target_name_k)
    
    # loop over 'diversity metrics' producing importance figure and adding each to an div importance dataframe
    for (i in 1:length(v_optional_predictors)){ # i <- 1
      var_name_i <- v_optional_predictors[i] ; print(var_name_i)# extract individual diversity predictor
    
      # time count
      f_time_update(t_start_time)
      
      list_areas <- list()   # list the area, pos %, neg %
        
      # loop over iterations done of dice (maybe 20 or so)
      for (i_iter in 1:n_iter){ # i_iter <- 1
      print( paste0( 'iteration :', i_iter))
      ###################################################
      ######    LOAD DICE                           #####
      ###################################################
      
      df_dice_direct_load <- paste0( f_ice_input , 'i_iter_' ,i_iter, '/', 'df_dice-', var_name_i , '_targ-', target_name_k , 'iter-', i_iter, '.RData' )  # df_dice-mu_kurt_targ-kndvi_lambda_xt.RData
      load(df_dice_direct_load)
      summary(df_dice)
      
      # join to forest area file
      df_dice <- left_join(df_dice, df_var) # summary(df_dice)
      
      

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
      
      perc_neg_area <- round( 100 * tot_neg_area / (tot_pos_area + tot_neg_area) , digits = 2)
      perc_pos_area <- round( 100 * tot_pos_area / (tot_pos_area + tot_neg_area) , digits = 2)
      
      tot_neg_area <- round( tot_neg_area , digits = 0)
      tot_pos_area <- round( tot_pos_area , digits = 0)
      tot_area     <- round( tot_area     , digits = 0)
      print(paste('tot_area ', tot_area))
      
      list_areas[[i_iter]] <- c(tot_area, perc_pos_area, perc_neg_area) # add to list the area, pos %, neg %
      
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
                          lims_m_in,  s_direction = 1)
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
                                              '_absRR-', b_useAbs_RestRate, '_iter-', i_iter, '.png' ) ,
                                              # '_absRR-', b_useAbs_RestRate, '_tightLims.png' ) ,
             width = fig_width, height = fig_height ) # , width = wid, height = hei)

      
      print( paste0('diversity variable : ', var_name_i , ' model plotting complete for res variable : ', target_name_k) )
      f_time_update(t_start_time)

      
      } # end loop over i_iterations
      
      save(list_areas,  file= paste0(output_path, 'list_areas_', var_name_i, '_targ-', target_name_k, 
                                     '_absRR-', b_useAbs_RestRate, '.RDATA' ))
      # stats of upper
      mean_second_item <- mean(sapply(list_areas, function(x) x[2]))
      sd_second_item <- sd(sapply(list_areas, function(x) x[2]))
      c_prob <- c(0.025, 0.975) # quantiles
      quantiles <- quantile(sapply(list_areas, function(x) x[2]), probs = c_prob)
      print( paste0('mean: ', mean_second_item , ' sd: '  , round(sd_second_item, digits = 3), '; quantiles : ' )) #, paste0( round(quantiles, digits = 3) , collapse = ", " ) ) )
      print(quantiles) 
      print( quantiles - c(mean_second_item,mean_second_item)) 
      
      # stats of upper
      mean_second_item <- mean(sapply(list_areas, function(x) x[3]))
      sd_second_item <- sd(sapply(list_areas, function(x) x[3]))
      c_prob <- c(0.025, 0.975) # quantiles
      quantiles <- quantile(sapply(list_areas, function(x) x[3]), probs = c_prob)
      print( paste0('mean: ', mean_second_item , ' sd: '  , round(sd_second_item, digits = 3), ' ; quantiles : ', paste0( round(quantiles, digits = 3) , collapse = "," ) ) )
      print( quantiles - c(mean_second_item,mean_second_item)) 
      
    } # end loop over div metrics
  } # end loop over res metrics
  
} # end run ICE
