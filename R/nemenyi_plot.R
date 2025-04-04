
# Environment for global variables
.env = new.env(parent = emptyenv())

## main ----

#' Generates a plot for a Nemenyi test results.
#' 
#' @param means Vector of means to plot, the order is not necessary.
#' @param labels Vector of labels corresponding to each means.
#' @param critical_diff Critical difference.
#' @param filepath (Optional) File path to save the plot.
#' @param width (Optional) If filepath is provided, width of the plot to save.
#' @param height (Optional) If filepath is provided, height of the plot to save. 
#' @param extra_xlim (Optional) Left and right extra space in the x-axis.
#' @param extra_ylim (Optional) Bottom and top extra space in the y-axis.
#' @param vertical (Optional) If TRUE the rule is vertical
#' @param color_ties (Optional) If TRUE the tie lines are colored.
#' @param reversed_ruler (Optional) If TRUE the ruler is reversed.
#' @param show_means (Optional) If TRUE shows the means in the labels.
#' @param dots_size (Optional) Dots size, if zero dots are not shown.
#' @param debug_ (Optional) If TRUE plots debug information.
#' 
#' @examples
#' library(NemenyiPlot)
#' 
#' cd = 1
#' 
#' means = c(1, 1.01, 1.2, 3, 3.2, 3.5, 3.5, 3.5, 3.5, 6, 7)
#' 
#' labels = c(
#'   "Method E",
#'   "Method K",
#'   "Method D",
#'   "Method B",
#'   "Method C",
#'   "Method J",
#'   "Method A",
#'   "Method G",
#'   "Method I",
#'   "Method F",
#'   "Method H"
#' )
#' 
#' NemenyiPlot::plot_nemenyi(means, labels, cd)
#' NemenyiPlot::plot_nemenyi(means, labels, cd,
#'   reversed_ruler = TRUE, dots_size = 0)
#' NemenyiPlot::plot_nemenyi(means, labels, cd,
#'   vertical = FALSE, show_means = TRUE)
#' NemenyiPlot::plot_nemenyi(means, labels, cd,
#'  vertical = FALSE, reversed_ruler = TRUE, color_ties = FALSE)
#' 
#' @export
#' 
plot_nemenyi = function(
    means,
    labels,
    critical_diff,
    
    filepath = NA,
    width = NA,
    height = NA,
    
    extra_xlim = c(0,0),
    extra_ylim = c(0,0),
    
    vertical = F,
    color_ties = T,
    reversed_ruler = F,
    show_means = F,
    dots_size = 1,
    debug_ = F) {
  
  
  check_input(means, labels, critical_diff, filepath, width, height, extra_xlim,
              extra_ylim, vertical, color_ties, reversed_ruler,
              show_means, dots_size, debug_)
  
  # Data calculations
  d = data.frame(means, labels) |> dplyr::arrange(means)
  groups_data = get_groups_data(d$means, critical_diff)
  
  initialize_global_varaibles(d, critical_diff, groups_data, extra_xlim, extra_ylim,
                    vertical, color_ties, reversed_ruler, show_means,
                    dots_size, debug_)

  draw_full_plot(filepath, width, height)
  clean_env()
}


initialize_global_varaibles = function(
    d,
    critical_diff,
    groups_data,
    extra_xlim = c(0,0),
    extra_ylim = c(0,0),
    vertical = F,
    color_ties = T,
    reversed_ruler = F,
    show_means = F, 
    dots_size = 1,
    debug_ = F
    ) {

  # general 
  .env$d = d
  .env$groups_data = groups_data
  .env$critical_diff = critical_diff
  .env$vertical = vertical
  
  # size
  .env$xlim = NA
  .env$ylim = NA
  
  if(vertical){
    .env$extra_xlim = extra_xlim + c(0.01,0.1)
    .env$extra_ylim = extra_ylim + c(
      (max(ceiling(d$means)) - min(floor(d$mean))) * 0.05,
      (max(ceiling(d$means)) - min(floor(d$mean))) * 0.05
    )
    
  } else {
    .env$extra_xlim = extra_xlim + c(1,1)
    .env$extra_ylim = extra_ylim + c(0,0)
  }
  
  # Ruler
  .env$reversed_ruler = reversed_ruler
  .env$integer_line_length = 0.01
  .env$non_integer_line_length = .env$integer_line_length/2
  .env$text_height = 0.02
  .env$ruler_min = min(floor(d$mean))
  .env$ruler_len = max(ceiling(d$means))
  
  # Ties
  .env$ties_separation = 0.02
  .env$ties_overflow = 0
  .env$color_ties = color_ties
  .env$dots_size = dots_size
  
  # Label
  .env$label_separation_hPlot = .env$ties_separation # horizontal plot
  .env$arrow_horizontal_len = 0.2 # horizontal plot
  .env$label_line_white_distance = 0.005
  .env$show_means = show_means
  
  # Critical difference
  .env$critical_diff_y = 0.05
  .env$critical_diff_vertical_lines = 0.005
  .env$critical_diff_text_y_separation = 
    .env$critical_diff_vertical_lines # horizontal plot
  .env$critical_diff_text_x_separation = 
    .env$critical_diff_vertical_lines + (0.0075) # vertical plot

  # Debug
  .env$debug_ = debug_
}


draw_full_plot = function(filepath, width, height) {
  
  if (is.na(filepath)) {
    create_plot_window_and_draw_components()
    
  } else {
    ext = tools::file_ext(filepath)
    
    if (ext == "pdf") {
      pdf(filepath, width = width, height = height)
      
    } else if (ext == "jpeg" || ext == "jpg") {
      jpeg(filepath, width = width * 100, height = height * 100)
      
    } else if (ext == "png") {
      png(filepath, width = width * 100, height = height * 100)
      
    } else {
      stop("Unsupported format. Use pdf, jpeg, or png.")
    }

    create_plot_window_and_draw_components()
    dev.off()
  }
}


create_plot_window_and_draw_components = function() {
  create_plot_window()
  draw_components()
}


clean_env = function(){
  rm(list = ls(envir = .env), envir = .env)
}



## window -----


create_plot_window = function() {

  par(mar = c(0,0,0,0))
  plot.new()
  calculate_window_limits()
  set_window_dim()
  
  if(.env$debug_){
    draw_plot_limits()
  }
  
}


#' Calculates the limits of the plot window.
#' If the reversed_ruler is TRUE the limits are reversed. (e.g. (10, -10)
#' instead of (-10, 10)) so the plot is reversed. This way most of the elements
#' don't need to be relocated.
#' 
calculate_window_limits = function(){
  
  if (.env$vertical){
    .env$xlim = get_x_limits_vertical_plot()
    
    if(.env$reversed_ruler){
      .env$ylim = get_y_limits_reversed_vertical_plot()
    } else {
      .env$ylim = get_y_limits_vertical_plot()
    }
  
  # vertical_plot
  } else {
    .env$ylim = get_y_limits_horizontal_plot()
    
    if(.env$reversed_ruler){
      .env$xlim = get_x_limits_reversed_horizontal_plot()
    
    } else {
      .env$xlim = get_x_limits_horizontal_plot()
    }
  }
  
}


get_x_limits_horizontal_plot = function() {
  
  left_label_x = .env$ruler_min - .env$arrow_horizontal_len
  right_label_x = .env$ruler_len + .env$arrow_horizontal_len
  
  c(
    left_label_x - .env$extra_xlim[1],
    right_label_x + .env$extra_xlim[2]
  )
}


get_y_limits_horizontal_plot = function() {
  
  n_groups = nrow(.env$groups_data)
  n_label_layers = ceiling(nrow(.env$d)/2)
  
  c(
    # min
    -.env$ties_separation * n_groups -
    .env$label_separation_hPlot * (n_label_layers+1) - # +1 extra space
    .env$extra_ylim[1],
    
    # max
    (.env$critical_diff_y + .env$critical_diff_text_y_separation) * 1.25 +
    .env$extra_ylim[2]
  )
}

get_x_limits_vertical_plot = function() {
  
  cd_text_x_position = - .env$critical_diff_y - .env$critical_diff_text_x_separation
  label_x_position = .env$ties_separation * (nrow(.env$groups_data) + 2)
  
  c(
    cd_text_x_position - .env$extra_xlim[1],
    label_x_position + .env$extra_xlim[2]
  )
  
}


get_y_limits_vertical_plot = function() {
  c(
    .env$ruler_min - .env$extra_ylim[1],
    .env$ruler_len + .env$extra_ylim[2]
  )
}

get_x_limits_reversed_horizontal_plot = function() {
  get_x_limits_horizontal_plot()[2:1]
}

get_y_limits_reversed_vertical_plot = function() {
  get_y_limits_vertical_plot()[2:1]
}


set_window_dim = function(){
  
  plot.window(
    xlim = .env$xlim,
    ylim = .env$ylim
  )
  
}


#' Draws the plot limits on debugging.
#' 
draw_plot_limits = function(){
  
  rect(
    xleft = .env$xlim[1],
    ybottom = .env$ylim[1],
    xright = .env$xlim[2],
    ytop = .env$ylim[2],
    border = "red"
  )
}

## draw components ----

draw_components = function() {
  
  if (.env$vertical == T){
    draw_ruler_vertical_plot()
    draw_critical_diff_vertical_plot()
    draw_tie_lines_vertical_plot()
    draw_labels_vertical_plot()
    draw_tie_dots_vertical_plot()
  } else{
    draw_ruler_horizontal_plot()
    draw_critical_diff_horizontal_plot()
    draw_tie_lines_horizontal_plot()
    draw_labels_horizontal_plot()
    draw_tie_dots_horizontal_plot()
  }
}


#' Draws the horizontal ruler, it includes the main line, integer numbers and small lines for
#' the ineger and the x.5 numbers.
#' 
draw_ruler_horizontal_plot = function() {
  
  # main line
  lines(c(.env$ruler_min, .env$ruler_len), c(0, 0), lwd = 2)
  
  # numbers
  text(x = .env$ruler_min:.env$ruler_len,
       y = rep(.env$text_height, .env$ruler_len),
       labels = .env$ruler_min:.env$ruler_len
       )
  
  # integer vertical lines
  seq(.env$ruler_min, .env$ruler_len, 1) |> sapply(function(x) {
      line_length = .env$integer_line_length
      line_width = 2
      lines(c(x, x), c(0, line_length), lwd = line_width)
  })
  
  # .5 vertical lines
  seq(.env$ruler_min+.5, .env$ruler_len-0.5, 1) |> sapply(function(x) {
      line_length = .env$non_integer_line_length
      line_width = 1
      lines(c(x, x), c(0, line_length), lwd = line_width)
  })
  
}

#' Draws the vertical ruler, it includes the main line, integer numbers and small lines for
#' the ineger and the x.5 numbers.
#' 
draw_ruler_vertical_plot = function() {
  
  # main line
  lines(c(0, 0), c(.env$ruler_min, .env$ruler_len), lwd = 2)
  
  # numbers
  text(
    x = -rep(.env$text_height, .env$ruler_len-.env$ruler_min+1),
    y = .env$ruler_min:.env$ruler_len,
    labels = .env$ruler_len:.env$ruler_min
  )
  
  # integer vertical lines
  seq(.env$ruler_min, .env$ruler_len, 1) |> sapply(function(y) {
      line_length = .env$integer_line_length
      line_width = 2
      lines(-c(0, line_length), c(y, y), lwd = line_width)
  })
  
  # .5 vertical lines
  seq(.env$ruler_min+.5, .env$ruler_len-.5, 1) |> sapply(function(y) {
      line_length = .env$non_integer_line_length
      line_width = 1
      lines(-c(0, line_length), c(y, y), lwd = line_width)
  })
}


#' Draws the critical difference indicator for the horizontal plot, includes a
#' line, it limits and a text.
#' 
draw_critical_diff_horizontal_plot = function(){
  
  x_start =  .env$ruler_min
  x_end =  x_start + .env$critical_diff
  y = .env$critical_diff_y 
  
  y_start_vertical = y  -.env$critical_diff_vertical_lines
  y_end_vertical = y + .env$critical_diff_vertical_lines
  
  # horizontal line
  lines(
    c(x_start, x_end),
    c(y, y),
    lwd = 2
  )
  
  # left vertical line
  lines(
    c(x_start, x_start),
    c(y_start_vertical, y_end_vertical),
    lwd = 2
  )
  
  # right vertical line
  lines(
    c(x_end, x_end),
    c(y_start_vertical, y_end_vertical),
    lwd = 2
  )
  
  # text
  text(
    x_start + .env$critical_diff/2,
    y + .env$critical_diff_text_y_separation,
    paste("CD =", round(.env$critical_diff, 2)),
    pos=3
  )
}


#' Draws the critical difference indicator for the vertical plot, includes a
#' line, it limits and a text.
#' 
draw_critical_diff_vertical_plot = function(){
  
  y_start =  .env$ruler_len
  y_end =  y_start - .env$critical_diff
  x = - .env$critical_diff_y
  
  x_start_vertical = x-.env$critical_diff_vertical_lines
  x_end_vertical = x+.env$critical_diff_vertical_lines
  
  if(!.env$reversed_ruler){
    pos_text = 3
  } else{
    pos_text = 1
  }
  
  # vertical line
  lines(
    c(x, x),
    c(y_start, y_end),
    lwd = 2
  )
  
  # left vertical line
  lines(
    c(x_start_vertical, x_end_vertical),
    c(y_start, y_start),
    lwd = 2
  )
  
  # right vertical line
  lines(
    c(x_start_vertical, x_end_vertical),
    c(y_end, y_end),
    lwd = 2
  )
  
  # text
  text(
    x - .env$critical_diff_text_x_separation,
    # y_end ,
    y_end + (y_start-y_end)/2,
    
    paste("CD =", round(.env$critical_diff, 2)),
    pos=pos_text,
    srt = 90
  )
}


#' Draws the line ties between groups for the horizontal plot.
#' 
draw_tie_lines_horizontal_plot = function() {
  
  means = .env$d$means
  starts = .env$groups_data$start
  ends = .env$groups_data$end
  colors = get_colors(nrow(.env$groups_data))
  
  for ( i in seq(nrow(.env$groups_data)) ) {
    
    start = means[starts[i]] - .env$ties_overflow
    end = means[ends[i]] + .env$ties_overflow
    
    separation = .env$groups_data$draw_layer_col[i] * .env$ties_separation
    
    lines(
      c(start, end),
      c(-separation, -separation),
      lwd = 3,
      col = colors[i]
    )
  }
  
}

#' Draws the line ties between groups for the vertical plot.
#' 
draw_tie_lines_vertical_plot = function() {
  
  means = get_corresponding_y_coordinate_vertical_plot(.env$d$means)
  starts = .env$groups_data$start
  ends = .env$groups_data$end
  colors = get_colors(nrow(.env$groups_data))
  
  for ( i in seq(nrow(.env$groups_data)) ) {
    
    start = means[starts[i]] + .env$ties_overflow
    end = means[ends[i]] - .env$ties_overflow
    separation = .env$groups_data$draw_layer_col[i] * .env$ties_separation
    
    lines(
      c(separation, separation),
      c(start, end),
      lwd = 3,
      col = colors[i]
    )
  }
}

#' Draws the points where the tie lines cross the arrows for the horizontal plot.
#' 
draw_tie_dots_horizontal_plot = function() {
  
  points_coords = get_points_coords()
  colors = get_colors(nrow(.env$groups_data))
  
  
  separation = points_coords$group_layer * .env$ties_separation
  colors_plot = colors[points_coords$group]
  
  points(points_coords$mean, -separation, col=colors[points_coords$group], pch = 19, cex = .env$dots_size)
}

#' Draws the points where the tie lines cross the arrows for the vertical plot.
#' 
draw_tie_dots_vertical_plot = function() {
  
  points_coords = get_points_coords()
  means = get_corresponding_y_coordinate_vertical_plot(points_coords$mean)
  
  colors = get_colors(nrow(.env$groups_data))
  
  separation = points_coords$group_layer * .env$ties_separation
  colors_plot = colors[points_coords$group]
  
  points(separation, means, col=colors[points_coords$group], pch = 19, cex = .env$dots_size)
}


#' Returns the positions where the tie lines cross the arrows.
#' 
get_points_coords = function(){
  
  out = data.frame(
    mean=numeric(),
    group=numeric()
  )
  
  for ( i in seq(nrow(.env$d))){

    # If the mean belongs to a group
    if(any(.env$groups_data$start<=i & i<=.env$groups_data$end)){
      
      out = rbind(out,
        data.frame(
          mean = .env$d$means[i],
          group = which(.env$groups_data$start<=i & i<=.env$groups_data$end)
        )
      )
      
    }
    
  }
  
  out$group_layer = .env$groups_data$draw_layer_col[out$group]
  out
}


#' Draw labels and the arrow which connects them to the ruler for the horizontal
#'  plot.
#' 
draw_labels_horizontal_plot = function() {
  
  n_tie_layers = ifelse(
    length(.env$groups_data$draw_layer_col) == 0, 0,
    max(.env$groups_data$draw_layer_col)
  )
  
  vertical_label_distance = .env$ties_separation * (n_tie_layers + 1)
  means_str = numbers_2_str_same_rounding(.env$d$means)

  for (i in seq_len(nrow(.env$d))) {
    
    mean = .env$d$means[i]
    mean_str = means_str[i]
    label = .env$d$labels[i]

    # Left side
    if (i <= ceiling(nrow(.env$d) / 2)) {
      label_separation_index = i - 1
      line_end_x = .env$ruler_min - .env$arrow_horizontal_len
      pos = 2
      
      if(.env$show_means){
        label = paste(label, " (", mean_str, ")", sep="")
      }
    
    # Right side  
    } else {
      label_separation_index = nrow(.env$d) - i
      line_end_x = .env$ruler_len + .env$arrow_horizontal_len
      pos = 4
      
      if(.env$show_means){
        label = paste("(", mean_str, ") ", label, sep="")
      }
      
    }
    
    label_y_pos =
      -vertical_label_distance -(label_separation_index * .env$label_separation_hPlot)
    
    # Reverse text pos
    if(.env$reversed_ruler){
      pos = ifelse(pos == 2, 4, ifelse(pos == 4, 2, pos))
    }

    # 3 Lines
    lines(c(mean, mean, line_end_x),
          c(0, label_y_pos, label_y_pos),
          lwd = 1)
    
    # Text
    text(line_end_x, label_y_pos, paste(label), pos = pos)
  }
}


#' Draw labels and the arrow which connects them to the ruler for the vertical
#'  plot.
#'  
draw_labels_vertical_plot = function() {
  
  n_tie_layers = ifelse(
    length(.env$groups_data$draw_layer_col) == 0, 0,
    max(.env$groups_data$draw_layer_col)
  )
  
  horizontal_position = .env$ties_separation * (n_tie_layers + 1)
  line_end_x = horizontal_position + .env$ties_separation
  
  .env$d$y_label = estimate_y_label_position_vertical_plot(.env$d$means, ylim, .env$ruler_len, .env$ruler_min)
  
  means_str = numbers_2_str_same_rounding(.env$d$means)

  for (i in seq_len(nrow(.env$d))) {
    
    # Calculate
    mean_value_pos = get_corresponding_y_coordinate_vertical_plot(.env$d$means[i])
    y_label = get_corresponding_y_coordinate_vertical_plot(.env$d$y_label[i])
    
    label = .env$d$labels[i]
    if(.env$show_means){
        label = paste("(", means_str[i], ") ", label, sep="")
    }
    
    # 2 Lines
    lines(
      c(0, horizontal_position, line_end_x),
      c(mean_value_pos, mean_value_pos, y_label),
      lwd = 1
    )
    
    # Text
    text(line_end_x + .env$label_line_white_distance, y_label, label,
         adj = c(0, 0.5))
    
  }
}

#' Casts a vector of number to string keeping the maximum number of decimals for
#' all the numbers.
#' 
#' @param numbers Numeric vector
#'
numbers_2_str_same_rounding = function(numbers){
  
  max_decimals = sapply(strsplit(as.character(numbers), "\\."), function(l) {
    if (length(l) == 2) nchar(l[2]) else 0
  }) |> max()
  
  formatC(numbers, format = "f", digits = max_decimals)
}


#' Given a mean value, returns the corresponding y-coordinate in the vertical
#' plot. Without this adjustment, the coordinates would be inverted.
#' 
#' @param y Numeric, the original y-coordinate
#' 
get_corresponding_y_coordinate_vertical_plot = function(y){
  y *-1 + .env$ruler_len + .env$ruler_min
}


## groups_data ----

#' Returns a dataframe giving the groups information of the test, where each
#' row is a group, the columns are:
#' start: the index of the means belonging to the start of the group
#' end: the index of the the means index belongin to the start of the group
#' draw_layer_col: the high layer it tie line should be draw
#' 
#' @param means Vector of means to plot, the order is not necessary.
#' @param critical_diff Critical difference.
#' 
#' @return data.frame
#' 
get_groups_data = function(means, critical_diff) {
  
  digits = 2
  
  end_indices = sapply(means, function(mean){
    max(which(round(means, digits) <= round(mean+critical_diff, digits)))
  })
  
  groups_data = data.frame(start = seq_along(means), end = end_indices)
  groups_data = remove_non_first_group_item(groups_data)
  
  # remove_empty_groups
  groups_data = groups_data[groups_data$start != groups_data$end,]
  
  groups_data$draw_layer_col = get_ties_layer(groups_data)
  
  groups_data
}


#' Given a dataframe calculated in "get_groups_data" containing the groups
#' information of the test, where each row is a group, the columns are:
#' start: the index of the means belonging to the start of the group
#' end: the index of the the means index belongin to the start of the group
#' 
#' It adds the column:
#' draw_layer_col: the high layer it tie line should be draw
#' 
#' @param groups_data data.frame
#'  
#' @return data.frame
#' 
get_ties_layer = function(groups_data){
  
  layers = array(dim=nrow(groups_data))
  layer = 0
  
  for (i in seq_len(nrow(groups_data)) ) {
    
    # Restart stacks
    if(1<i && groups_data$end[i-1] < groups_data$start[i]){
      layer = 0
    }
    
    layer = layer + 1
    layers[i] = layer
  }
  
  layers
}



#' Removes the non first item in groups, from a data.frame where each row is a
#' group and the columns are:
#' start: the index of the means belonging to the start of the group
#' end: the index of the the means index belongin to the start of the group
#' 
#' @param groups_data data.frame
#' 
remove_non_first_group_item = function(groups_data) {
  duplicated_indices = which(duplicated(groups_data$end))
  if (length(duplicated_indices) > 0) {
    groups_data = groups_data[-duplicated_indices, ]
  }
  return(groups_data)
}

## colors ----

#' Returns n colors to use
#' 
#' @param n number of colors
#' 
get_colors = function(n){
  
    # Rainbow vs black
    if(.env$color_ties) {
        colors = rainbow(n)
    } else {
        colors = rep("black", n)
    }
}

# Nemenyi test ----


#' Uses the funcion tsutils::nemenyi to calculate the nemenyi test
#' 
#' @param x Numeric matrix with data.
#' @param conf.level Confidence level.
#' 
#' @export
#' 
nemenyi_test = function(x, conf.level = 0.95) {
  r = tsutils::nemenyi(x, conf.level=conf.level, plottype="none")
  
  list(
    p_value = r$fpval,
    means = r$means,
    cd = r$cd
  )
}


#' Calculates the Nemenyi test and plots the resuts.
#' 
#' @param x Numeric matrix where each row represents a measure, and each colum
#' a method. The labels are substracted from the column names
#' @param conf.level Confidence level.
#' @param labels Vector of labels corresponding to each means.
#' @param ... Rest of the params from plot_nemenyi, except "means",
#' "critical_diff" and "labels".
#' 
#' @examples
#' 
#' set.seed(1)
#' 
#' x = matrix( rnorm(50*6, mean = 0, sd = 1), 50, 6)
#' 
#' x[,1] = x[,1]+1
#' x[,2] = x[,2]+2
#' x[,3] = x[,3]+3
#' x[,4] = x[,4]+4
#' x[,5] = x[,5]+100
#' x[,6] = x[,5]+100
#' 
#' colnames(x) = c("Method A", "Method B", "Method C",
#' "Method D", "Method E", "Method F")
#' 
#' nemenyi_test_and_plot(x, conf.level=0.95)
#' 
#' @export
#' 
nemenyi_test_and_plot = function(x, conf.level=0.95, labels=NA, ...) {
  
  labels = if(!is.na(labels)) labels else colnames(x)
  
  r = nemenyi_test(x, conf.level)
  plot_nemenyi(r$means, r$cd, labels=labels, ...)
  r
}

#' Calculates the Nemenyi test and plots the resuts. In contrast to
#' "nemenyi_test_and_plot" every data is stored in a row.
#' 
#' @param d Data frame containing the data.
#' @param score Character name of the column representing the score values.
#' @param method Character name of the column representing the method.
#' @param group Character name of the column representing the grouping variable
#' @param conf.level Confidence level.
#' @param ... Rest of the params from plot_nemenyi, except "means",
#' "critical_diff" and "labels".
#' 
#' @examples
#' 
#' set.seed(1)
#' 
#' methods = c("Method A","Method B","Method C",
#' "Method D", "Method E", "Method F")
#' 
#' df = expand.grid(
#'   fold = 1:10,
#'   method = methods
#' )
#' 
#' df$acc = c(
#'   rnorm(10,mean=0,sd=1),
#'   rnorm(10,mean=1,sd=1),
#'   rnorm(10,mean=2,sd=1),
#'   rnorm(10,mean=3,sd=1),
#'   rnorm(10,mean=100,sd=1),
#'   rnorm(10,mean=200,sd=1)
#' )
#' 
#' nemenyi_test_and_plot_as_rows(df, "acc", "method", "fold")
#' 
#' @export
#' 
nemenyi_test_and_plot_as_rows = function(
    d, score, method, group, conf.level=0.95, ...){

  d = d[c(score, method, group)] |>
    tidyr::pivot_wider(
      names_from = all_of(method),
      values_from = all_of(score)
    )
  
  d = d[-1]
  
  nemenyi_test_and_plot(d, conf.level, ...)
}
