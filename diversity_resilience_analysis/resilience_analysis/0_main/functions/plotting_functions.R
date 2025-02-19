# ########################################################
# Title         : plotting_functions.R
# Description   : This script holds the plotting functions of the analysis
#                 
#                 
#                 Themes
# Date          : 19/02/25
# Authors       : Mark Pickering & Agata Elia
# Notes         : 
# ########################################################



###################################################
######     PLOTTING FUNCTIONS BASIC MAP/HIST  #####
###################################################

# create a histogram of stat_in
make_hist <- function(df_in, stat_in, stat_in_full, lims_in){
  # h_dist <- ggplot(df_in, aes_string(x = stat_in )) +
  h_dist <- ggplot(df_in, aes(x = !!sym(stat_in) )) +
    geom_histogram( bins = n_bins, colour = "black", fill='black') + #  # ..density.. breaks = seq(-1, 1, by = 10), aes_string(y =rescaled_hist_density, colour = cat_a)
    labs( y= paste0( 'Frequency'), 
          x= paste0(  stat_in_full    ) ) + 
    theme_classic() +
    scale_x_continuous(limits = lims_in, oob=squish  ) + 
    basic_hist_theme          +   
    theme(
      axis.title.y = element_text( vjust = 2)
    #   # axis.title.y = element_text(margin = margin(r = 10, unit = "pt"))  # Increase right margin of y-axis title (more space title-number)
    #   # plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Top, right, bottom, left margins of the plot
    )
  return(h_dist)
}


# create a map of stat_in with a selected shapefile outline
make_map <- function(df_in, stat_in, stat_in_full, title_full , lims_in, col_palette = hcl.colors(12, "Blue-Red 3", rev = FALSE)){
    g_input <-  ggplot() +
    # geom_tile(data = df_in, aes_string(x = 'x', y = 'y', fill = stat_in)) + # add the raster data
    geom_tile(data = df_in, aes(x = x, y = y, fill = !!sym(stat_in) )) + # add the raster data
    geom_sf(data = land_shapefile) + # , color = 'black', size =2) + # add the shapefile (place the base layer first)
    labs( x='', y='', # x= 'latitude', y = 'longitude', 
          fill = paste0( stat_in_full ) ) + #,  title = paste0( title_full ) ) + #, '_', gsub('_',' ', stat_j) ) ) + # label axes and title
    coord_sf() + # align the coordinates to those of shapefile?
    # coord_equal() + # different crs
    # discrete_fill_viridis_c() + # alternative colour system
    # scale_fill_distiller(limits= lims_in , palette =  'Spectral', na.value = "white",
    #                      breaks = pretty_breaks(n = 4),  # Set the number of breaks
    #                      oob=scales::squish, direction = s_direction) + # set the colour scheme and palette # direction = 1,
    # Use a custom color set from hcl.colors
    scale_fill_gradientn(colors = col_palette , 
                         limits = lims_in, 
                         breaks = pretty_breaks(n = 4), 
                         na.value = "white", 
                         oob = scales::squish) +
    # scale_fill_distiller(limits=c(-1, 1), palette =  'YlGn', direction = 1,    
    #                      trans = "log",  breaks = scales::trans_breaks('log10', function(x) round(10 ^ x, digits=1), n=3)    ) + # , guide = guide_colorbar(frame.colour = "black")
    # theme_void() +
    basic_fig_theme  +
    theme(
      legend.title = element_text(hjust = 0.5)  # Center the legend title
    ) + 
    guides(fill = guide_colorbar(title.position = "top"))
  
  return(g_input)
}

f_combine_map_hist <- function (g_in, h_in, b_cut_title = 'T'){
  # adjust the positioning to fit histogram below figures
  
  # create vector of x, y, width, height, hjust)
  # draw_plot(g_input, x = 0, y = 0.4, width = 0.6, height = 0.6) 
  # if we want to cut the title
  if(b_cut_title == 'T') { v_pos_g <- c(0.21, 0.23, 0.83, 0.83, 0.3) 
                           v_pos_h <- c(0, 0, 0.77,  0.24, 0) } # 1.2x  legend (cuts top of title)
  # don't cut title
  if(b_cut_title == 'F') {v_pos_g <- c(0.23, 0.23, 0.77, 0.77, 0.3)
                          v_pos_h <- c(0, 0, 0.77,  0.24, 0)        } # 1x  legend
  # cut title and reduced GEDI Europe size - small size
  if(b_cut_title == 'T_short_europe_small') {v_pos_g <- c(0.23, 0.11, 0.77, 0.77, 0.3)
                                             v_pos_h <- c(0   , 0   , 0.77, 0.24, 0)}
  # cut title and reduced GEDI Europe size - fullsize
  if(b_cut_title == 'T_short_europe') {v_pos_g <- c(0.025, 0.17, 0.95, 0.95, 0)
                                       v_pos_h <- c(0.025, 0.05   , 0.95, 0.30, 0)}
  # cutdon't  title and reduced GEDI Europe size - fullsize
  if(b_cut_title == 'F_short_europe') {v_pos_g <- c(0.025, 0.192, 0.95, 0.95, 0)
                                       v_pos_h <- c(0.025, 0.05   , 0.95, 0.30, 0)}
  
  g_draw <- ggdraw( clip = 'off') +
    draw_plot(g_input, x = v_pos_g[1], y = v_pos_g[2], width = v_pos_g[3], height = v_pos_g[4], hjust = v_pos_g[5]) + # 1.2x  legend (cuts top of title)
    draw_plot(h_dist , x = v_pos_h[1], y = v_pos_h[2], width = v_pos_h[3], height = v_pos_h[4], hjust = v_pos_h[5]) 
  
  return(g_draw)
}

make_norm_hist <- function(df_in, stat_in, stat_in_full, lims_in){
   h_dist <- ggplot(df_in, aes_string(x = stat_in)) +
      geom_histogram(aes(y = ..count.. / sum(..count..)), bins = 100, colour = "black", fill='lightblue') +
      labs( y = 'Normalized frequency', 
      x = stat_in_full) + 
      theme_classic() +
      scale_x_continuous(limits = lims_in)        
   return(h_dist)
}



###################################################
######     PLOTTING FUNCTION CLIMATE SPACE    #####
###################################################

# function to plot a variable in climate space (specifying all three variables)
make_plot_clim_space <- function(df_in, var_x, var_y, var_z, x_label, y_label, z_label, lim){
  columns_to_select <- c(var_x, var_y, var_z)
  df_in_sel <- df_in[, columns_to_select]
  g_clim_space <- ggplot(data = df_in_sel, aes(x = !!as.symbol(var_x), y = !!as.symbol(var_y)-273.15, z = !!as.symbol(var_z))) +
    stat_summary_2d(bins = 100, fun="mean", na.rm = TRUE) +
    labs(x = x_label, y = y_label, fill = z_label) +
    scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 10)) + 
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
    scale_fill_gradientn(colors = rev(brewer.pal(9, "RdYlGn")), limits = lim, na.value = "transparent", aesthetics = "fill", oob=scales::squish, breaks = scales::pretty_breaks(n=4)) + # "RdYlBu", breaks = scales::pretty_breaks(n=5)) +
    basic_graph_theme +
    theme(
      legend.title = element_text(margin = ggplot2::margin(0, 30, 30, 0)),  # increase bottom margin of legend title
      legend.position = "bottom",
      legend.key.width = unit(3, "cm"))
  return(g_clim_space) #RdYlBu #YlGnBu
}

###################################################
######     PLOTTING VARIABLE VS VARIABLE      #####
###################################################

# function to plot a variable (kndvi TAC) against all other variables and show the r2 of a lm between the two and the corr
make_plot_variable_vs_variable <- function(df_in, col_name, long_name){
  
  # create a lm between the two variables
  formula_string <- paste("kndvi_TAC ~", col_name)
  ml <- lm(as.formula(formula_string), data = df_in)
  
  # get the r2 of the lm
  r2 <- round(summary(ml)$r.squared, digits = 3)
  
  # calculate the corr coefficient between the two variables
  v_cor <- round(cor(df_in[,1] , df_in[,2]), digits = 3)
  
  # create title
  s_title <- paste0("\nR\u00B2 = ", r2, " ", "Cor = ", v_cor)
  
  # set number of bins
  n_bins_dens <- n_bins 
  
  # plot
  g_variable_vs_variable <- ggplot(data = df_in, aes(x = kndvi_TAC, y = !!as.symbol(col_name))) +
    stat_bin2d(bins= c(n_bins,n_bins), aes(fill = ..ndensity..), na.rm = TRUE) +
    geom_smooth(data = df_in, method = "lm", linetype = "dashed", linewidth = 1.5, color="black", show.legend = TRUE, formula = as.formula(paste('y', paste('x'), sep = " ~ "))) + #, mapping = aes_string("kndvi_TAC", y = col_name)) +
    labs(x = "Long-term kNDVI TAC", y = long_name, title = s_title) +
    scale_fill_gradientn(colors = rev(brewer.pal(9, "GnBu")), limits = c(0.0, 1.0), na.value = "transparent", aesthetics = "fill", name = 'Relative \ndensity') +
    basic_graph_theme
  return(g_variable_vs_variable)
}

# function to plot a variable (ICE) against its respective diversity variable
make_plot_ice_vs_variable <- function(df_in, col_name, long_name, ice, ice_long_name, y_lim){
  
  # create a lm between the two variables
  formula_string <- paste(paste0(ice, " ~"), col_name)
  ml <- lm(as.formula(formula_string), data = df_in)
  
  # get the p-value of the lm
  p_value <- summary(ml)$coefficients[2, "Pr(>|t|)"]
  
  # create title
  s_title <- paste0("ICE partial derivative vs ", long_name, "\nlm = ", formula_string, " ", "\np-value = ", p_value)
  
  # set number of bins
  n_bins_dens <- n_bins 
  
  # create a ggplot for the histogram of the x variable
  hist_x <-ggplot(df_ice_div, aes_string(x = col_name)) +
    geom_histogram(bins = 100, colour = "black", fill='black')  + 
    labs(x = long_name, y = "Frequency") +
    theme_minimal() +
    theme(axis.text = element_text(size = 30), axis.title = element_text(size = 30))

  # create a ggplot for the histogram of the y variable
  hist_y <-ggplot(df_ice_div, aes_string(x = ice)) +
    geom_histogram(bins = 100, colour = "black", fill='black')  + 
    labs(x = "ICE", y = "Frequency") + 
    scale_x_continuous(limits = ice_lim, oob=squish) +
    theme_minimal() +
    theme(axis.text = element_text(size = 30), axis.title = element_text(size = 30))
  
  # plot scatter plot
  g_ice_vs_variable <- ggplot(data = df_in, aes(x = !!as.symbol(col_name), y = !!as.symbol(ice))) +
    stat_bin2d(bins= c(n_bins,n_bins), aes(fill = ..ndensity..), na.rm = TRUE) +
    geom_smooth(data = df_in, method = "lm", linetype = "dashed", linewidth = 1.5, color="black", show.legend = TRUE, formula = as.formula(paste('y', paste('x'), sep = " ~ "))) + #, mapping = aes_string(col_name, y = ice)) +
    labs(x = long_name, y = ice_long_name, title = s_title) +
    scale_fill_gradientn(colors = rev(brewer.pal(9, "GnBu")), limits = c(0.0, 1.0), na.value = "transparent", aesthetics = "fill", name = 'Relative \ndensity') +
    ylim(ice_lim) +
    basic_graph_theme
  
  library(gridExtra)
  g_ice_vs_variable_comb <- grid.arrange(g_ice_vs_variable, hist_x, hist_y, ncol = 1, heights = c(3, 1, 1))
  
  return(g_ice_vs_variable_comb)
}



###################################################
######     OTHER FUNCTIONS                    #####
###################################################

#for plot making - rounds up to predetermined numbers shown
#roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
NiceNumbers=c(0,1,1.5,2,2.5,3,4,5,6,7,7.5,8,9,10)
roundUpNice <- function(x, nice = NiceNumbers) { #https://rdrr.io/cran/subscreen/src/inst/shiny-app/subscreen/app.R
  if (length(x) != 1) stop("'x' must be of length 1")
  if (x >= 0) 10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) *nice)[[1]]]]
  else -1 * (roundDownNice(-x, nice = NiceNumbers))
}

roundDownNice <- function(x, nice = NiceNumbers) {
  if (length(x) != 1) stop("'x' must be of length 1")
  if (x >= 0) 10^floor(log10(x)) * nice[[max(which(x >= 10^floor(log10(x)) * nice))]]
  else -1 * (roundUpNice(-x, nice = NiceNumbers))
}


find_cutpts_from_percentile <- function(df_plotting, col, perc){
  # get the symmetrical cutpoints, (min_scale, max_scale) for a column 'col, within a dataframe, df,
  # using percentiles 'perc' of the values 
  # e.g. perc == 0.01 means max/min scale are at 1% and 99%
  max_val <- quantile(df_plotting[[col]], 1-perc, na.rm = TRUE) ;   min_val <- quantile(df_plotting[[col]], perc,na.rm = TRUE)
  min_scale <-roundDownNice( abs(min_val) ) ;  if( min_val < 0) {min_scale <- 0 - min_scale} ; max_scale <-roundUpNice( max_val )
  if(max_scale*min_scale < 0 ) {
    # if one (only) of max_scale or min_scale is negative, then we want to take the highest absolute value 
    # and set the max or min as that so that the axis is symmetrical 
    if (max_scale >= abs(min_scale)){min_scale <- -1*abs(max_scale)}
    else{max_scale <- abs(min_scale)}
  }
  result <- c(min_scale, max_scale)
  return(result)
}

# set limits that are unsymmetric about zero to symmetric about zero
# take a vector of two elements and set the absolute smallest value of one
# to the opposite sign of the absolte largest value. 
f_symmetric_limits <- function(numbers ) {
  
  # check that the numbers fit the pattern of limits where the second is the largest
  if (length(numbers) != 2 || numbers[2] < numbers[1]) {
    stop("The input must be a vector of two numbers, with the second the larger number.")
  }
  # check if the numbers are already symmetric about zero
  if (numbers[1] == - numbers[2]) {
    return(numbers)
  }
  
  if (numbers[1] != numbers[2]) {
    # Identify the larger number by absolute value
    larger <- max(abs(numbers)) ;   
    
    # Set the smaller number to the absolute value of the larger, with opposite sign
    if (abs(numbers[1]) < abs(numbers[2])) {
      numbers[1] <- -larger
    } else {
      numbers[2] <- larger
    }
  }
  
  return(numbers)
}
