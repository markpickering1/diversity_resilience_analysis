# ########################################################
# Title         : plotRF_functions.R
# Description   : This script holds the plotting functions of the RF
#                 
#                 
#                 Themes
# Date          : 9/07/23
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################



###################################################
####### LOAD DF                          ##########
###################################################


# load the df required for the analysis (e.g. test train etc)
f_plotRF_load_RFDF <- function(df_in, s_train_test_all = s_use_pdp_dataset){
  # load df containing all test train data
  # load( paste0(input_dir, 'df_all_div-',var_name_i, '.RData') )        # df_comb_i      head(df_comb_i)
  
  # # initialise train/test df
  df_comb.train_i <- subset(df_in, train_sample == T) # head(df_comb.train_i)
  df_comb.test_i  <- subset(df_in, train_sample == F) # head(df_comb.test_i)
  
  # create the pdps using the training testing or all data
  if(s_train_test_all == 'train'){ df_out <-  df_comb.train_i[complete.cases(df_comb.train_i), ] ; print('using train data') }
  if(s_train_test_all == 'test') { df_out <-  df_comb.test_i[complete.cases(df_comb.test_i), ]   ; print('using test data')  }
  if(s_train_test_all == 'all')  { df_out <-  df_in[complete.cases(df_in), ]                     ; print('using all data')   }
  
  # return( list(df_out, df_comb.train_i, df_comb.test_i)) # if we want to return all the dfs for whatever reason
  return( df_out )
}

# remove the below when happy all fine
# # load rf model
# load( paste0(input_dir, 'rf_model_div-',var_name_i, '_targ-', target_name_k, '_seed-102.RData' ) ) # rf.model load the rf model for each div variable
# # load dataframes of variables containing test/train split
# # load df containing all test train data
# load( paste0(input_dir, 'df_all_div-',var_name_i, '_targ-', target_name_k, '.RData') )        # df_comb_i      head(df_comb_i)
# # # initialise train/test df
# df_comb.train_i <- subset(df_comb_i, train_sample == T) # head(df_comb.train_i)
# df_comb.test_i  <- subset(df_comb_i, train_sample == F) # head(df_comb.test_i)
# 
# # create the pdps using the training testing or all data
# if(s_use_pdp_dataset == 'train') df_pdp <-  df_comb.train_i[complete.cases(df_comb.train_i), ]
# if(s_use_pdp_dataset == 'test')  df_pdp <-  df_comb.test_i[complete.cases(df_comb.test_i), ]
# if(s_use_pdp_dataset == 'all')   df_pdp <-  df_comb_i[complete.cases(df_comb_i), ]

# f_plotRF_load_RFDF <- function(var_name_i){ 
#   # load df containing all test train data
#   load( paste0(input_dir, 'df_all_div-',var_name_i, '.RData') )        # df_comb_i      head(df_comb_i)
#   
#   # # initialise train/test df
#   df_comb.train_i <- subset(df_comb_i, train_sample == T) # head(df_comb.train_i)
#   df_comb.test_i  <- subset(df_comb_i, train_sample == F) # head(df_comb.test_i)
#   
#   return( c(df_comb_i, df_comb.train_i, df_comb.test_i))
# }


###################################################
######     PLOTTING FUNCTIONS RF PERFORMANCE  #####
###################################################

# performance plot of modelled vs observed
# takes in a df with columns 1 = observed, and 2= modelled 
# calculates certain statistics (N, R2, MSE, RMSE, MAE, percentage bias) if given as true
# plots these variables and the Obs vs Mod data points
f_obs_vs_mod_density <- function(df_in, s_title = var_name_i, 
                                 b_cor = F, b_mse = F, b_rmse = F,  b_mae = F, b_pbias = F,  b_coefDet = F,
                                 lims_x =c(-0.1, 1), lims_y = c(-0.1, 1) ){
  
  # calculate metrics
  if(b_cor)  v_cor    <- round( cor(df_in[,1] , df_in[,2]), digits = 3)
  if(b_mse)  v_mse    <- round( mse(df_in[,1] , df_in[,2]), digits = 5)
  if(b_rmse) v_rmse   <- round( rmse(df_in[,1], df_in[,2]), digits = 3)
  if(b_mae)  v_mae    <- round( mae(df_in[,1] , df_in[,2]), digits = 3) # MAE vs RMSE comparison https://medium.com/human-in-a-machine-world/mae-and-rmse-which-metric-is-better-e60ac3bde13d
  if(b_pbias)v_pbias  <- round( percent_bias(df_in[,1], df_in[,2]), digits = 3) # mean(actual - predicted)/abs(actual)  - not really a percentage
  if(b_coefDet) {
    # Fit a linear model
    model <- lm(Observed ~ Modelled, data = df_in)
    # Get the summary of the linear model
    model_summary <- summary(model)
    # Extract the R-squared value
    r_squared <- round( model_summary$r.squared, digits = 3)
  }
  
  # create label
  s_label_stats <- paste0( 
    "N = ", dim(df_in)[1], '\n',
    ifelse(b_coefDet, paste0("R2 = ", r_squared, '\n') , ''), # rho = \u03C1
    ifelse(b_cor  , paste0("R = ", v_cor, '\n') , ''), # rho = \u03C1
    ifelse(b_mse  , paste0("MSE = "    , v_mse, '\n') , ''),
    ifelse(b_rmse , paste0("RMSE = " , v_rmse , '\n') , ''),
    ifelse(b_mae  , paste0("MAE = "  , v_mae  , '\n') , ''),
    ifelse(b_pbias, paste0("PBIAS = ", v_pbias, '\n') , '')
  )
  
  n_bins_dens <- n_bins # set to global hist binning value
  
  # create a plot of or density figure
  ggp <- ggplot( df_in , aes(Modelled, Observed)) +    # Draw ggplot2 plot with one axis
    # geom_point(alpha = 0.01, size = 0.0005, color = 'blue') + # to show datapoints
    stat_bin2d( bins= c(n_bins,n_bins) , aes(fill = after_stat(ndensity)), na.rm = TRUE) + # ..ndensity.. ..count..
    # stat_bin2d( bins= c(n_bins,n_bins) , aes(fill = ..ndensity..), na.rm = TRUE) + # ..ndensity.. ..count..                  
    scale_fill_distiller(name = 'Relative \ndensity', direction = 1 ) +
    geom_abline(slope=1, intercept=0, linetype = "dashed" ) +
    xlim( lims_x ) + ylim( lims_y ) + 
    labs(title = s_title) +
    basic_graph_theme
  # add text of the selected stats
  ggp <- ggp +  annotate("text" , 
                         x = -Inf, y = Inf, vjust = 1, # place text relative to top left
                         # x=-0.05, y=0.8, # - relative to exact posiiton
                         size = text_size/2, hjust = -0.05,
                         #parse = TRUE,
                         label = s_label_stats  ) # geom_text(data = df_cor, size=txt_size*1.4, aes(x = lim_upper[1]*text_pos_x, y = lim_upper[2]*0.96), parse = TRUE)  # add text of data
  return(ggp)
}

###################################################
######     PLOTTING FUNCTIONS RF IMPORTANCE   #####
###################################################

# for a df df_in, creates line graph showing the ranking of variables var_in in, ordered  by their values value_in 
# (_full denotes x,y labesl). Option to include a categorisation by color
f_importance_ranking <- function(df_in, var_in, var_in_full, value_in, value_in_full, s_title = 'he', 
                                 point_color = 'orange', lims_c = NA,
                                 b_unc = F
                                 ){
  
  # set a default color that will be overwritten if vector of colors applied
  ifelse(length(point_color)==1 , point_color_1 <- point_color , point_color_1 <- 'orange' )
  
  g_importance_i <-
    ggplot(df_in %>% arrange( !!sym(value_in) ) %>% mutate( Variable = factor(!!sym(var_in), levels=!!sym(var_in)) ),
           # aes_string(x="Variable", y=value_in )) + # , color = "Category"
           aes(x= Variable, y= !!sym(value_in) )) + # , color = "Category"
    # geom_segment( aes_string(xend=var_in, yend=0),  linewidth = 1) + # , color = type (size or line size)
    geom_point( size=4  , color = point_color_1  ) + # single color point
    coord_flip() +  #scale_y_log10() + 
    ylim( lims_c ) +
    # labs(title = s_title)
    ylab(value_in_full) + xlab(var_in_full) +
    # theme(text = element_text(size=20) ) + #axis.text.x = element_text(size = 20) # angle=90, hjust=1
    basic_graph_theme +  # theme_bw() 
    theme(text = element_text(size=30) ) #axis.text.x = element_text(size = 20) # angle=90, hjust=1
    
    
  
  # overwrite color scale with category of colors 
  if(length(point_color) > 1){
    g_importance_i <- g_importance_i +
      # geom_point( size=4  , aes_string( color = "Category")   ) + #, color="orange") + #  , aes_string( color = "Category")
      geom_point( size=4  , aes( color = Category)   ) + #, color="orange") + #  , aes_string( color = "Category")
      guides(color=guide_legend(override.aes=list(fill=NA))) +   # no grey box around legend
      scale_color_manual(name = "Category", values=point_color ) 
    # scale_color_manual(name = "category", values = c(T2M = "red", TP = 'blue', SSR = 'brown', VPD = 'orange', Other = "dark green") )  +
    # scale_color_manual(name = "type",     values = c(mean = "black", TAC = 'grey50', CV = 'grey10') )  +
  }
  
  # add unc band if requested - assumes default label yhat_upper and yhat_lower
  if(b_unc == 'CI'){
    g_importance_i <- g_importance_i +
     geom_errorbar(aes(x = Variable, ymin = yhat_lower, ymax = yhat_upper, 
                       color = Category), width = 0.2)
  } else if(b_unc == 'sd'){
    g_importance_i <- g_importance_i +
      geom_errorbar(aes(x = Variable, ymin = mean - sd, ymax = mean + sd, 
                        color = Category), width = 0.2)
  }
  
  return(g_importance_i)  
}

###################################################
######     PLOTTING FUNCTIONS RF PARTIAL DEP  #####
###################################################

# Define a function to create fixed-width y-axis labels
# This will set aside some space on the y-axis for the labels
# I'm not sure it's 100% necessary, but it seems to work
fixed_width_labels <- function(width = 12) {
  function(labels) {
    str_pad(labels, width, "right")
  }
}

# this histogram is designed to sit below the partial plots. 
# the difficulty (yet to fully get right) is to match the axes of the x-axes of histogram and partial. 
# The only way I've managed to match them so far is by having fixed text_size at 8 for the histogram of frequency. It's all quite messy
f_plot_partial_hist <- function(df_pdp, var_name_x, var_label_x = NA, s_oob = 'squish', # s_oob  = 'censor'
                                n_bins_x = 100, lims_x, coord_flip = F ){
  
  # rename the axes if required
  if(is.na(var_label_x)) var_label_x <- var_name_x
  
  # Plot the histogram for the x-axis
  # g_hist <- ggplot(df_pdp, aes_string(x = var_name_x) ) +
  g_hist <- ggplot(df_pdp, aes( x = !!sym(var_name_x) ) ) +
    geom_histogram(bins = n_bins_x) +
    scale_x_continuous(limits = lims_x, oob= get(s_oob, envir = asNamespace('scales')))  +  # access squish etc limits
    xlab(var_label_x) +
    theme(axis.text.y = element_text(size = 8)) +
    ylab("Frequency") +
    # basic_graph_theme +
    scale_y_continuous(labels = fixed_width_labels(12))
  # theme(plot.margin = unit(c(0.5, 0.5, 0.5, y_axis_space), "cm"))    # geom_point(data = pdp_list_df, aes(Petal.Width, yhat, color = iteration ), size = 3 )
  
  if(coord_flip)  g_hist <-g_hist + coord_flip() 
  return(g_hist)
}


# Base function for common plotting setup for 1D pdps
f_base_plot_partial <- function(df_pdp_base, 
                                var_name_x_base      , var_name_y_base, 
                                var_label_x_base = NA, var_label_y_base = NA,
                                lims_x_base          , lims_y_base,  
                                line_width_base = 1, add_hist_under_base = FALSE) {
  
  # Rename the axes if required
  if(is.na(var_label_x_base)) var_label_x_base <- var_name_x_base
  if(is.na(var_label_y_base)) var_label_y_base <- var_name_y_base
  
  g_pdp <- ggplot(df_pdp_base, aes(x = !!sym(var_name_x_base), y = !!sym(var_name_y_base))) +
    geom_line(linewidth = line_width_base) +  # Default line width can be adjusted here if needed
    ylab(var_label_y_base) +
    scale_y_continuous(limits = lims_y_base, labels = fixed_width_labels(12)) #+
    # theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))  # Adjust margins as needed
  
  # Set x-axis scale or leave to default
  if(length(lims_x_base) == 2) {    g_pdp <- g_pdp + scale_x_continuous(limits = lims_x_base)   }
  
  # Adjustments if adding a histogram underneath
  if(add_hist_under_base) {
    g_pdp <- g_pdp + xlab("") + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
  } else {
    g_pdp <- g_pdp  + basic_graph_theme + xlab(var_label_x_base)  }
  
  
  return(g_pdp)
}

# Function for plotting partial dependence plots
f_plot_partial <- function(df_pdp, var_name_x, var_name_y, 
                           var_label_x = NA, var_label_y = NA, 
                           lims_x = lims, lims_y = y_lims_pdp, 
                           line_width = 1, add_hist_under = FALSE, 
                           add_error_band_even = FALSE, add_error_band_uneven = FALSE, var_name_se = NA) {

    g_pdp <- f_base_plot_partial(df_pdp_base = df_pdp, 
                               var_name_x_base  = var_name_x , var_name_y_base  = var_name_y , 
                               var_label_x_base = var_label_x, var_label_y_base = var_label_y,
                               lims_x_base = lims_x, lims_y_base = lims_y,  
                               line_width_base = line_width, add_hist_under_base = add_hist_under)
  
  # Add an even band of error values (e.g., from standard error)
  if(add_error_band_even && !is.na(var_name_se)) {
    g_pdp <- g_pdp + geom_ribbon(aes(ymin = !!sym(var_name_y) - !!sym(var_name_se), 
                                     ymax = !!sym(var_name_y) + !!sym(var_name_se)), alpha=0.4,
                                     linetype=0) # remove edge line
  }
  
  # Add an uneven band of uncertainty (e.g., confidence interval)
  if(add_error_band_uneven && !is.na(var_name_se)) {
    g_pdp <- g_pdp + geom_ribbon(aes(ymin = !!sym(paste0(var_name_se, "lower")), 
                                     ymax = !!sym(paste0(var_name_se, "upper"))), alpha=0.4,
                                     linetype=0) # remove edge line
  }
  
  return(g_pdp)
}

# Function for overlaying multiple pdps
f_plot_partial_overlay <- function(df_pdp, var_name_x, var_name_y, 
                                   overlay_column = 'index', color_column = NA,  
                                   var_label_z = NA, l_colors = c(), 
                                   lims_x = lims, lims_y = y_lims_pdp, var_label_x = NA, var_label_y = NA, 
                                   line_width = 1, legend_pos = 'none',
                                   add_hist_under = FALSE,
                                   add_error_band_even = FALSE, add_error_band_uneven = FALSE, var_name_se = NA) {
  
  # g_pdp <- f_base_plot_partial(df_pdp_base = df_pdp, var_name_x_base  = var_name_x , var_name_y_base  = var_name_y , 
  #                              var_label_x_base = var_label_x, var_label_y_base = var_label_y, lims_x_base = lims_in, lims_y_base = lims_y, 
  #                              line_width_base = line_width, add_hist_under_base = add_hist_under)

  # first produce the partial plot
  g_pdp <- f_plot_partial(df_pdp = df_pdp, 
                               var_name_x  = var_name_x , var_name_y  = var_name_y , 
                               var_label_x = var_label_x, var_label_y = var_label_y,
                               lims_x = lims_x, lims_y = lims_y, 
                               line_width = line_width,
                               add_hist_under = add_hist_under,
                               add_error_band_even = add_error_band_even, add_error_band_uneven = add_error_band_uneven, var_name_se = var_name_se)
  
  
  # Overlay logic
  if(!is.na(color_column)) {
    print(paste0('using color column : ', color_column ))
    g_pdp <- g_pdp + aes(fill = !!sym(color_column), color = !!sym(color_column), group = !!sym(overlay_column)) +
      # geom_line(linewidth = line_width) +
      labs(color = color_column)
    if(!is.na(var_label_z)) g_pdp <- g_pdp + labs(color = var_label_z, fill = var_label_z) 
    
    if(  length(l_colors) > 0 ) g_pdp <- g_pdp + scale_color_manual( values = l_colors) + scale_fill_manual( values = l_colors)
  } else {
    g_pdp <- g_pdp + aes(group = !!sym(overlay_column)) #+
      # geom_line(linewidth = line_width)
  }
  
  # g_pdp <- f_base_plot_partial(df_pdp, var_name_x, var_name_y, lims_x, lims_y, var_label_x, var_label_y, add_hist_under)
  # if(!is.na(legend_pos)) 
  g_pdp <- g_pdp + theme(legend.position = legend_pos ) # "none")
  
  
  return(g_pdp)
}

# # this function tries to create a common plotting format for partial dependence plots which can be matched with a 
# # histogram underneath. 
# # input a df containing pdp vals of format x-col
# f_plot_partial <- function(df_pdp, var_name_x, var_name_y, 
#                            lims_x = lims_in, lims_y = y_lims_pdp, 
#                            var_label_x = NA, var_label_y = NA,
#                            add_hist_under = F, add_error_band_even = F, add_error_band_uneven = F, # add error bands or histogram
#                            var_name_se = 'NA'){
#   
#   # rename the axes if required
#   if(is.na(var_label_x)) var_label_x <- var_name_x
#   if(is.na(var_label_y)) var_label_y <- var_name_y
#   
#   g_pdp <- ggplot( df_pdp , aes( !!sym(var_name_x), !!sym(var_name_y) ) ) + # replace aes_string with !!
#   # g_pdp <- ggplot( df_pdp , aes_string(var_name_x, var_name_y) ) + #, color = variable)) + group_color = 'black',   # Draw ggplot2 plot with one axis
#     geom_line( linewidth = 1) + # add line of mean
#     ylab(var_label_y) + # "TAC"
#     # basic_graph_theme +
#     scale_y_continuous(limits = lims_y , labels = fixed_width_labels(12)) 
#   # theme(plot.margin = unit(c(0.5, 0.5, 0.5, y_axis_space), "cm")) # y_axis_space <- unit(2, "cm")
#   
#   
#   
#   # set x-axis scale or leave to default
#   if(length(lims_x) ==2){ g_pdp <- g_pdp +   scale_x_continuous(limits = lims_x  ) }
#   
#   # if we add a histogram underneath, we want to remove the x_axis label and ticks
#   if(add_hist_under) {
#     g_pdp <- g_pdp + xlab("") +  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) 
#   } else{
#     g_pdp <- g_pdp + basic_graph_theme +     
#       xlab(var_label_x)}
#   # add an even band of error vals(e.g. from standard error)
#   if(add_error_band_even) {
#     g_pdp <- g_pdp + geom_ribbon( aes(ymin = ( !!(sym(var_name_y)) - !!(sym(var_name_se) ) ), 
#                                       ymax = ( !!(sym(var_name_y)) + !!(sym(var_name_se) ) ) ), alpha=0.2)  # add uncertainty band
#   }
#   # add a uneven band of uncertainty (e.g. confidence interval)
#   if(add_error_band_uneven) {
#     g_pdp <- g_pdp + geom_ribbon( aes(ymin = (  !!(sym( paste0( var_name_se, 'lower' )  ) ) ), 
#                                       ymax = (  !!(sym( paste0( var_name_se, 'upper' )  ) ) ) ), alpha=0.2)  # add uncertainty band
#   }
#   return(g_pdp)
# }
# 
# 
# 
# 
# 
# # this function overlays a number of different pdps onto the same figure
# # input a df containing pdp vals of format x-col
# f_plot_partial_overlay <- function(df_pdp, var_name_x, var_name_y, 
#                                    overlay_column = 'index', color_column = NA, 
#                            lims_x = lims_in, lims_y = y_lims_pdp, 
#                            var_label_x = NA, var_label_y = NA,
#                            line_width = 0.05, 
#                            add_hist_under = F){
#   
#   # rename the axes if required
#   if(is.na(var_label_x)) var_label_x <- var_name_x
#   if(is.na(var_label_y)) var_label_y <- var_name_y
#   
#   g_pdp <- ggplot( df_pdp , aes( x =!!sym(var_name_x), y = !!sym(var_name_y), group = !!sym(overlay_column)  ) ) + # replace aes_string with !!
#   # g_pdp <- ggplot( df_pdp , aes( x =!!sym(var_name_x), y = !!sym(var_name_y), group = !!sym(overlay_column),  color = !!sym(color_column)  ) ) + # replace aes_string with !!
#     # g_pdp <- ggplot( df_pdp , aes_string(var_name_x, var_name_y) ) + #, color = variable)) + group_color = 'black',   # Draw ggplot2 plot with one axis
#     geom_line( linewidth = line_width) + # add line of mean
#     # geom_line() 
#     ylab(var_label_y) + # "TAC"
#     # basic_graph_theme +
#     scale_y_continuous(limits = lims_y , labels = fixed_width_labels(12))
#   # theme(plot.margin = unit(c(0.5, 0.5, 0.5, y_axis_space), "cm")) # y_axis_space <- unit(2, "cm")
#   
#   if( ! is.na( color_column ) ){
#     g_pdp <- g_pdp + aes( color = factor(!!sym( color_column  ) ) ) +
#       geom_line( linewidth = line_width ) +  labs( color = color_column )
#   }
#   
#   # set x-axis scale or leave to default
#   if(length(lims_x) ==2){ g_pdp <- g_pdp +   scale_x_continuous(limits = lims_x  ) }
#   
#   # if we add a histogram underneath, we want to remove the x_axis label and ticks
#   if(add_hist_under) {
#     g_pdp <- g_pdp + xlab("") +  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) 
#   } else{
#     g_pdp <- g_pdp + basic_graph_theme +     
#       xlab(var_label_x)
#   }
#   
#   return(g_pdp)
# }


###################################################
######     2D PDP                             #####
###################################################

# Function to take random forest (for given diversity + res metric) 
# and compute 2d PDP for one diversity metric: var_name_i
# NOTE: to add v_target - needs testing and maybe adding to function input!
pdp_2d_parallelDiv_function <- function( var_name_i, target_name_k) {
  
  ###################################################
  ####### LOAD RF AND DF                   ##########
  ###################################################
  
  # load rf model
  # s_name_rf_model <-  paste0(input_dir, 'rf_model_div-',var_name_i, '_targ-', target_name_k, '_seed-102.RData' ) 
  s_name_rf_model <-  paste0(input_dir, 'list_rf_model_results_parallelDiv_div-',var_name_i, '_targ-', target_name_k, '.RData')
  load(s_name_rf_model) # rf.model - load the rf model for each div variable
  
  if(!b_do_common_testTrainSplit){ 
    load( paste0(input_dir,
                 'df_all_div-', var_name_i, '_targ-', target_name_k , '.RData' ) ) # head(df_comb_i)
  } else{
    load( paste0(input_dir, 'df_all.RData' ) ) # head(df_comb)
    df_comb_i <- df_comb ; rm(df_comb)
  }
  
  # # load dataframes of variables containing test/train split
  # s_name_df_comb <-  paste0(input_dir, 'df_all_div-',var_name_i, '_targ-', target_name_k, '.RData')
  # load( s_name_df_comb )        # df_comb_i      head(df_comb_i)
  
  # # initialise train/test df and df to use in analysis - select only those needed
  df_pdp <- f_plotRF_load_RFDF(df_comb_i, s_use_pdp_dataset) #; head(df_pdp)
  # # df_pdp <- result_temp[[1]] ; # df_comb.train_i <- result_temp[[2]] ; df_comb.test_i <- result_temp[[3]] ; rm(result_temp)
  
  ###################################################
  ######     PARTIAL DEPENDENCE DIVERSITY 2D    #####
  ###################################################
  
  # loop over selected variabels for 2d pdp plots
  for (j in 1:length(pdp_2d_extra_vars)){ # j <- 1
    pdp_2d_var_j <- pdp_2d_extra_vars[j] # select independent var for 2d pdp
    print(paste0('2d pdp plot between ', var_name_i, ' and ', pdp_2d_var_j) )
    
    # loop over the different metrics for the 2d pdp
    # df_comb.train_i_complete <- df_comb.train_i[complete.cases(df_comb.train_i), ]
    print('create pdp_2d')
    pdp_2d <- partial(rf.model, train = df_pdp, pred.var = c(pdp_2d_var_j, var_name_i) )
    print('save pdp_2d')
    save(pdp_2d, file=paste0(output_path, 'df_pdp_2d-',var_name_i, '-', pdp_2d_var_j, '_targ-', target_name_k, '.RData' )    ) # load(/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-03-21_plot/df_ice_test-fhd_avg_diversity.RData)
    
    # create basic partial plot
    print('create pdp_2d plot')
    g_pdp1 <- plotPartial(pdp_2d) # plot(g_pdp1)
    save(g_pdp1, file=paste0(output_path, 'g_pdp_2d-',var_name_i, '-', pdp_2d_var_j, '_targ-', target_name_k, '.png' )    ) # load(/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-03-21_plot/df_ice_test-fhd_avg_diversity.RData)
    # ggsave(filename = paste0(output_path, 'g_pdp_2d-',var_name_i, '-', pdp_2d_var_j, '_targ-', target_name_k, '.png' ), plot = g_pdp1, path = output_path, width = fig_width_wide, height = fig_height_wide)
    
    f_time_update(t_start_time)
    
    # probably want to build a separate function for plloting by ggplot2 - can add features        
    # # plot the histogram below
    # # Plot instead the normalized histogram for kurtosis
    # x_hist <- make_hist(df_comb_i,  pdp_2d_var_j, pdp_2d_var_j, lims_in)
    #   ggplot(df_comb_i, aes_string(x = Kurtosis) ) + geom_histogram(binwidth = 0.1) +
    #   xlab("Kurtosis") + ylab("Frequency")
    
  } # end loop over 2d pdp vars
  
  return('saved')
  
} # end function

###################################################
######     PLOTTING FUNCTIONS RF ICE          #####
###################################################



# take a dataframe with a column containing selected keywords and put these keywords as a category
f_add_category <- function(df_in, col_name, div_metrics_list = v_optional_predictors){
  df_in <- df_in %>% mutate(Category = ifelse( !!sym( col_name ) %in% div_metrics_list, 'Diversity',
                                              ifelse( grepl( 't2m', !!sym(col_name), fixed = TRUE), 'T2M',
                                               ifelse( grepl( 'VPD', !!sym(col_name), fixed = TRUE), 'VPD',
                                                       ifelse( grepl( 'tp', !!sym(col_name), fixed = TRUE), 'TP',
                                                               ifelse( grepl( 'ssr', !!sym(col_name), fixed = TRUE), 'SSR',
                                                                       # ifelse( grepl( 'kndvi', !!sym(col_name), fixed = TRUE), 'kNDVI',
                                                                               'Other' )
                                                               )      )       )       )       ) 
  return(df_in)
}





# function taking dataframe as input, an rf.model to run over and a variable of interest (and plotting limits if required)
# first splits the dataframe into manageable chunks
# honestly, this takes so long it is better to run over each variable individually and not loop over the diversity variables
# unless you are absolutely sure it is working and will plot the correct limits
# also may need to rename input values in functions as function input names are same as acutal names that will be used
# function is untested (but code was separately tested)
f_run_ICE <- function(df_comb_i, rf.model,  predictor_metric = var_name_i, predicted_metric = target_name_k, n_splits = 10){ # , lims_h_i = F, lims_h_i = F
  
  # First split the dataframe into manageable chunks over which to run using parameters:
  n_rows <- dim(df_comb_i)[1] ; 
  print( paste0('n rows: ', n_rows))
  chunk_size <- ceiling( n_rows/n_splits ) 
  print( paste0('chunk size: ', chunk_size))
  print( paste0('splits: ', n_splits))
  
  # old method - not sure about
  # df_comb_ij <- split(df_comb_i,  (as.numeric(rownames(df_comb_i))-1) %/% chunk_size) # split dataframe
  # new method to obtain the chunks - assign all rows a number in a column showing which chunk they are in
  df_comb_i$Chunk <- cut(seq_len(nrow(df_comb_i)), breaks = seq(1, nrow(df_comb_i) + chunk_size, chunk_size),
                  labels = FALSE, include.lowest = TRUE)
  df_comb_ij <- split(df_comb_i, df_comb_i$Chunk)
  
  # dim(df_comb_ij$`0`) ; dim(df_comb_ij$`9`) ; 
  # test the sum of the split rows adds up
  print('Does sum of rows in splits add up to total : ')
  print( dim(df_comb_ij$`2`)[1]*(n_splits - 2)  + dim(df_comb_ij$`1`)[1] + dim(df_comb_ij[[n_splits ]])[1] == n_rows)
  
  # create a 'vector of response values for the object was trained on' - i.e. the values of diversity metric to compute ICE curve for at each pixel
  # probably 500 is sufficient - this might be the reason things were slow before - we wanted to use the full range of values at each point (like 10,000 if dim(df_comb_i)[1]/10
  # set a range containing 500 values between the 2% and 98% percentiles:
  # c_min_max <- find_cutpts_from_percentile(df_comb_i, predictor_metric , 0.02)
  # v_vect_div_values <- seq(c_min_max[[1]], c_min_max[[2]], length.out = 500)
  # That didn't work - instead, just set -> # num_grid_pts = 500
  # Previously was defined by : 
  # y = df_comb_i[[predictor_metric]] = dictates that the response values comes from the values of the original df and not the chunk
  # this should ensure that all the same values are trained on - it doesn't - they area always different - same as y = df_comb_ij_j[[predictor_metric]]
  # if nothing is inserted, then just the actual values of the other points are used for each point - seems like a computationally expensive way to run
  # can instead create a range from the original data and feed that in? Use enough points to get a meaningful curve (but not )
  # or can just set:
  
  
  # create empty df of ICE
  df_ice_i <- data.frame()
  
  # create loop over deciles of the df to build the full model
  for (j in 1:n_splits){ # j <- 5
  # for (j in 9:10){ # j <- 1
    print(j)
    #select only the jth subdivision
    df_comb_ij_j <- df_comb_ij[[j]]
    print('dim of chunk: ') ; print(dim(df_comb_ij_j))
    
    print(paste0('create ice-object block:', j))
    # create an ice object from the rf model the split df
    rf.ice_j = ice(object = rf.model, X = df_comb_ij_j, y = df_comb_ij_j[[target_name_k]], predictor = predictor_metric, # y = df_comb_i$tac_resid_kndvi
                   # y = v_vect_div_values  
                   # num_grid_pts = 100, # 500, # setting this selects only certain grid points in diversity metric for building
                   indices_to_build = )
    # print(rf.ice_j$gridpts)
    # }
    #frac_to_build = 1)
    
    # as a temporary precaution save these blocks of ice objects
    save(rf.ice_j, file=paste0(output_path, 'df_dice-', predictor_metric, '_targ-', target_name_k, '_iceObj_split-', j, '.RData' )    ) # load(/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-03-21_plot/df_ice_test-fhd_avg_diversity.RData)
    print(paste0('saved ice-object block:', j))
    
    # create a derivative ice object (dice)
    # rf.dice_j <- dice(rf.ice_j, ICEbox::DerivEstimator ( rf.ice_j$gridpts, rf.ice_j$actual_prediction) )
    rf.dice_j <- dice(rf.ice_j )
    # test plot ;    plot.dice
    
    # ice sorts the order of the df so need to recombine with the input dataframe 
    # (match the input diversity values with the 'xj' valuse of predictor as a check)
    print('check column match:') ; print(summary(rf.dice_j$Xice[[predictor_metric]] == rf.dice_j$xj))
    
    # now bind the dice output of the derivative at the point (local derivative)
    df_rf.dice_j_t <- as.data.frame(rf.dice_j$actual_deriv) #, rf.dice_j$dpdp)
    names(df_rf.dice_j_t) <- 'actual_deriv'
    if(dim(rf.dice_j$Xice)[1] == dim(df_rf.dice_j_t)[1]) {
      df_rf.dice_j <- cbind(rf.dice_j$Xice, df_rf.dice_j_t) }
    # # should I add dpdp (overall derivative? It has a different length so can't bind to original df
    # # need to confirm what exactly is this from literature but was difficult
    # df_rf.dice_j_d <- as.data.frame(rf.dice_j$dpdp) #, rf.dice_j$dpdp) }
    # names(df_rf.dice_j_d) <- 'dpdp'
    # if( dim(df_rf.dice_j)[1] == dim(df_rf.dice_j_d)[1] ) { print('add dpdp')
    #   df_rf.dice_j <- cbind(df_rf.dice_j, df_rf.dice_j_d) }
    
    # df_rf.dice_j <- cbind(rf.dice_j$xj,rf.dice_j$actual_deriv) # list of changed points
    # df_rf.dice_j <- as.data.frame(df_rf.dice_j)
    # names(df_rf.dice_j)[2] <- 'ice_deriv'
    # df_rf.dice_j <- cbind(rf.dice_j$Xice, df_rf.dice_j$ice_deriv)
    
    print('save chunk')
    save(df_rf.dice_j, file=paste0(output_path, 'df_dice-',predictor_metric, '_targ-', target_name_k, '_diceObj_split-', j, '.RData' )    ) # load(/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-03-21_plot/df_ice_test-fhd_avg_diversity.RData)
    
    print('join chunk')
    # join the split dice with the other splits
    if (j == 1){df_ice_i <- df_rf.dice_j
    } else{df_ice_i <- rbind(df_ice_i, df_rf.dice_j)}
    
    print('dice split timing complete for split : ', i)
    f_time_update(t_start_time)
    
  } # end loop over deciles
  save(df_ice_i, file=paste0(output_path, 'df_dice-',predictor_metric, '_targ-', target_name_k, '_diceObj_split-all.RData' )    ) # load(/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-03-21_plot/df_ice_test-fhd_avg_diversity.RData)
  
  return(df_ice_i)
} # close function


# simply loop over different dice objects and rbind them together
# should not be a real function - this is a quick fix
# input_dice <- "/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_2_postEGU/2023-06-27_allVars_waitingFinalEdit/plotRF_2023-07-10_shannonIndex/"
# input_dice <- "/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_2_postEGU/2023-06-27_allVars_waitingFinalEdit/plotRF_2023-07-10_rh98/"
# input_dice <- "/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_2_postEGU/2023-06-27_allVars_waitingFinalEdit/plotRF_2023-07-10_kurt/"
input_dice <- "/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_3_Aug23/2023-11-08_alignment/plotRF_newDiv_2023-12-18_createICE/"
f_run_ICE_loop<- function(input_dice  ) { #df_comb_i, rf.model,  var_name_i){ # , lims_h_i = F, lims_h_i = F
  
  # create empty df of ICE
  df_dice <- data.frame()

  # loop over the previous outputs, load and add to df of ICE
  for (m in 1:100){ # m <- 9
    
    # load( paste0(input_dice, "df_dice-shannon_entropy_diceObj_split-", m, ".RData" ) )
    # load( paste0(input_dice, "df_dice-shannon_entropy_diceObj_split-", m, ".RData" ) )
    load( paste0(input_dice, "df_dice-", var_name_i , "_targ-", target_name_k, "_diceObj_split-", m, ".RData" ) )
    # load( paste0(input_dice, "df_dice-kurt_mean_diceObj_split-", m, ".RData" ) )
    print(dim(df_rf.dice_j)[1])
    
    # test if we have a dpdp col in some but not others 
    if( dim(df_rf.dice_j)[2] < dim(df_dice)[2] & dim(df_dice)[2] != 0 ) { df_rf.dice_j$dpdp <- NA   ; print('m missing dpdp col'  ) }
    if( dim(df_rf.dice_j)[2] > dim(df_dice)[2] & dim(df_dice)[2] != 0 ) { df_rf.dice_j$dpdp <- NULL ; print('m has extra dpdp col') }
    
    # join the split dice with the other splits
    if (m == 1){df_dice <- df_rf.dice_j
    } else{df_dice <- rbind(df_dice, df_rf.dice_j)}
  
  } # end loop  
  
  print(dim(df_dice)) # head(df_dice)
  
  return(df_dice)
} # close function


# 
# # simply loop over different ice objects - create dice objects with them... and rbind them together
# # should not be a real function - this is a quick fix
# # input_dice <- "/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_2_postEGU/2023-06-27_allVars_waitingFinalEdit/plotRF_2023-07-10_shannonIndex/"
# # input_dice <- "/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_2_postEGU/2023-06-27_allVars_waitingFinalEdit/plotRF_2023-07-10_rh98/"
# input_dice <- "/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/version_2_postEGU/2023-06-27_allVars_waitingFinalEdit/plotRF_2023-07-10_fhd_mean/"
# f_run_ICE_loop<- function(input_dice  ) { #df_comb_i, rf.model,  var_name_i){ # , lims_h_i = F, lims_h_i = F
#   
#   # create empty df of ICE
#   df_ice_i <- data.frame()
#   
#   # loop over the previous outputs, load and add to df of ICE
#   for (m in 1:10){ # m <- 1 
#     
#     # load( paste0(input_dice, "df_dice-shannon_entropy_diceObj_split-", m, ".RData" ) )
#     load( paste0(input_dice, "df_dice-fhd_mean_iceObj_split-", m, ".RData" ) )
#     # print(dim(rf.ice_j)[1])
#     
#     # create a derivative ice object (dice)
#     rf.dice_j <- dice(rf.ice_j)
#     # test plot ;    plot.dice
#     
#     # ice sorts the order of the df so need to recombine with the input dataframe 
#     # (match the input diversity values with the 'xj' valuse of predictor as a check)
#     print('check column match:') ; print(summary(rf.dice_j$Xice[[var_name_i]] == rf.dice_j$xj))
#     
#     # now bind the dice output of the derivative at the point (local derivative)
#     df_rf.dice_j_t <- as.data.frame(rf.dice_j$actual_deriv) #, rf.dice_j$dpdp)
#     names(df_rf.dice_j_t) <- 'actual_deriv'
#     if(dim(rf.dice_j$Xice)[1] == dim(df_rf.dice_j_t)[1]) {
#       df_rf.dice_j <- cbind(rf.dice_j$Xice, df_rf.dice_j_t) }
#     # should I add dpdp (overall derivative? It has a different length so can't bind to original df
#     # need to confirm what exactly is this from literature but was difficult
#     df_rf.dice_j_d <- as.data.frame(rf.dice_j$dpdp) #, rf.dice_j$dpdp) }
#     names(df_rf.dice_j_d) <- 'dpdp'
#     if( dim(df_rf.dice_j)[1] == dim(df_rf.dice_j_d)[1] ) { print('add dpdp')
#       df_rf.dice_j <- cbind(df_rf.dice_j, df_rf.dice_j_d) }
#     
#     # df_rf.dice_j <- cbind(rf.dice_j$xj,rf.dice_j$actual_deriv) # list of changed points
#     # df_rf.dice_j <- as.data.frame(df_rf.dice_j)
#     # names(df_rf.dice_j)[2] <- 'ice_deriv'
#     # df_rf.dice_j <- cbind(rf.dice_j$Xice, df_rf.dice_j$ice_deriv)
#     
#     
#     save(df_rf.dice_j, file=paste0(output_path, 'df_dice-',var_name_i, '_diceObj_split-', m, '.RData' )    ) # load(/eos/jeodpp/data/projects/FOREST-RESILIENCE/figures/rf_test_diversity/rf_test_diversity_2023-03-21_plot/df_ice_test-fhd_avg_diversity.RData)
#     
#     
#     print('dice split timing complete for split : ', m)
#     rf_end_time <- Sys.time() ; print(rf_end_time)      # initialise time
#     rf_duration <- rf_end_time - rf_start_time ; print(rf_duration)
#     
#     # join the split dice with the other splits
#     if (m == 1){df_ice_i <- df_rf.dice_j
#     } else{df_ice_i <- rbind(df_ice_i, df_rf.dice_j)}
#     
#   } # end loop  
#   
#   print(dim(df_ice_i)[1]) # head(df_ice_i)
#   # df_ice_i_ <- df_ice_i
#   
#   return(df_ice_i)
# } # close function