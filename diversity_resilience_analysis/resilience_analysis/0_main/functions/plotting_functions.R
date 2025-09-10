# ########################################################
# Title         : plotting_functions.R
# Description   : This script holds the plotting functions of the analysis
#                 
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
    geom_histogram( bins = n_bins, colour = "black", fill='black') + # 
    labs( y= paste0( 'Frequency'), 
          x= paste0(  stat_in_full    ) ) + 
    theme_classic() +
    scale_x_continuous(limits = lims_in, oob=squish  ) + 
    basic_hist_theme          +   
    theme(
      axis.title.y = element_text( vjust = 2)
    )
  return(h_dist)
}


# create a map of stat_in with a selected shapefile outline
make_map <- function(df_in, stat_in, stat_in_full, title_full , lims_in, col_palette = hcl.colors(12, "Blue-Red 3", rev = FALSE)){
    g_input <-  ggplot() +
    geom_tile(data = df_in, aes(x = x, y = y, fill = !!sym(stat_in) )) + # add the raster data
    geom_sf(data = land_shapefile) + # add the shapefile (place the base layer first)
    labs( x='', y='', # x= 'latitude', y = 'longitude', 
          fill = paste0( stat_in_full ) ) + # label axes and title
    coord_sf() + # align the coordinates to those of shapefile?
    # coord_sf(  expand = FALSE,              # Keep tight bounds, no extra margin
    #              crs = st_crs(4326),           # Set coordinate system to lat/lon WGS84
    #              label_graticule = "both" ) +     # Add lat/lon labels on both axes) 
    # Use a custom color set from hcl.colors
    scale_fill_gradientn(colors = col_palette , 
                         limits = lims_in, 
                         breaks = pretty_breaks(n = 4), 
                         na.value = "white", 
                         oob = scales::squish) +
    basic_fig_theme  +
    theme(
      legend.title = element_text(hjust = 0.5)  # Center the legend title
    ) + 
    guides(fill = guide_colorbar(title.position = "top"))
  
  return(g_input)
}

# combined map & histogram (from funcitons above)
f_combine_map_hist <- function (g_in, h_in, b_cut_title = 'T'){
  # adjust the positioning to fit histogram below figures
  
  # create vector of x, y, width, height, hjust)
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

# function to plot a variable var_z in climate space of var_x var_y (specifying all three variables)
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
