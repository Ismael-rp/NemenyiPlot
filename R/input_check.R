
#' Checks the validity of input parameters for a statistical analysis function
#' 
#' @param means Numeric vector
#' @param labels Character vector
#' @param critical_diff Numeric value
#' @param filepath Character string (optional)
#' @param width Numeric value
#' @param height Numeric value
#' @param extra_xlim Numeric vector of length 2
#' @param extra_ylim Numeric vectorof length 2
#' @param vertical Logical
#' @param color_ties Logical
#' @param reversed_ruler Logical
#' @param show_means Logical
#' @param dots_size Numeric value
#' @param debug_ Logical
#' 
#' @return TRUE if all checks pass
#' 
check_input = function(
    means,
    labels,
    critical_diff,
    filepath,
    width,
    height,
    extra_xlim,
    extra_ylim, 
    vertical,
    color_ties,
    reversed_ruler,
    show_means,
    dots_size,
    debug_
	) {
  
  check_means_and_labels(means, labels)
  check_critical_diff(critical_diff)
  check_saving_params(filepath, width, height)
  check_lim(extra_xlim, extra_ylim)
  check_binaries(vertical, color_ties, reversed_ruler, show_means, debug_)
  check_dots_size(dots_size)
  return(TRUE)
}

#' Validates the means and labels vectors
#' 
#' @param means Numeric vector
#' @param labels Character vector
#' 
check_means_and_labels = function(means, labels) {
  
  labels = as.vector(labels)
  check_numeric(means, "means")
  
  if (!all(1 <= means & means <= length(labels))) {
    stop("Each value in means must be between 1 and the length of labels")
  }
  
  if (length(means) < 2) {
    stop("means must have at least 2 elements")
  }
  
  if (length(labels) != length(means)) {
    stop("labels must have the same length as means")
  }
  
}

#' Checks if the critical difference is a valid positive numeric value
#' 
#' @param critical_diff Numeric value
#' 
check_critical_diff = function(critical_diff) {
  check_numeric(critical_diff, "critical_diff")
  check_positive(critical_diff, "critical_diff")
}

#' Validates parameters related to file saving
#' 
#' @param filepath Character string (optional)
#' @param width Numeric value
#' @param height Numeric value
check_saving_params = function(filepath, width, height){
  
  if (!is.na(filepath)) {
    
    if (!is.character(filepath)) {
      stop("filepath must be a character string")
    }
    
    check_nonnegative_numeric(width, "width")
    check_nonnegative_numeric(height, "height")
  }
  
}

check_binaries = function(vertical, color_ties, reversed_ruler, show_means, debug_){
  check_binary(vertical, "vertical")
  check_binary(color_ties, "color_ties")
  check_binary(reversed_ruler, "reversed_ruler")
  check_binary(show_means, "show_means")
  check_binary(debug_, "debug_")
}

#' Checks if the dot size is a valid non-negative numeric value
#' 
#' @param dots_size Numeric value
#' 
check_dots_size = function(dots_size){

    check_numeric(dots_size, "dots_size")
    
    if (dots_size < 0) {
      stop("dots_size must be greater than or equal to 0")
    }
}

#' Checks if a value is a boolean
#' 
#' @param x The value to check
#' @param name Name of the parameter
#' 
check_binary = function(x, name) {
  if (!is.logical(x)) {
    stop(paste(name, "must be logical (TRUE or FALSE)"))
  }
}


#' Checks if axis limits are valid numeric vectors of length 2
#' 
#' @param extra_xlim Numeric vector of length 2 for x-axis limits
#' @param extra_ylim Numeric vector of length 2 for y-axis limits
#' 
check_lim = function(extra_xlim, extra_ylim) {
  
  check_numeric(extra_xlim, "extra_xlim")
  check_length(extra_xlim, 2, "extra_xlim")
  
  check_numeric(extra_ylim, "extra_ylim")
  check_length(extra_ylim, 2, "extra_ylim")
  
}

#' Checks if a value is a numeric vector without infinite values
#' 
#' @param x The value to check
#' @param name Name of the parameter
#' 
check_numeric = function(x, name) {
  if (!is.numeric(x)) {
    stop(paste(name, "must be numeric"))
  }
  
  if (any(is.infinite(x))) {
    stop(paste(name, "must not contain infinite values"))
  }
}

#' Checks if a vector has the expected length
#' 
#' @param x The vector to check
#' @param expected Expected length
#' @param name Name of the parameter
#' 
check_length = function(x, expected, name){
  if (length(x) != expected){
    stop(paste(name, "must have length", expected))
  }
}

#' Checks if a numeric value is strictly positive
#' 
#' @param x The value to check
#' @param name Name of the parameter
#' 
check_positive = function(x, name){
  if (any(x <= 0)){
    stop(paste(name, "must be greater than 0"))
  }
}


#' Checks if a numeric value is non-negative and finite
#' 
#' @param x The value to check
#' @param name Name of the parameter
#' 
check_nonnegative_numeric = function(x, name){
  
  if (!is.numeric(x)){
    stop(paste(name, "must be numeric"))
  }
  
  if (any(x < 0) || any(is.infinite(x))) {
    stop(paste(name, "must be non-negative and finite"))
  }
}
