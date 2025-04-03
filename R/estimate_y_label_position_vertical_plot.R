
WEIGHT_ORDER = 1e6
WEIGHT_SEP = 1e6
WEIGHT_MOVEMENT = 1e3
SEP_P = 0.05

#' Estimates the optimal y-label positions for a vertical plot
#' 
#' @param p Numeric vector of initial label positions
#' @param ylim Numeric vector of plot y-axis limits
#' @param ruler_max Numeric, upper limit for label positions
#' @param ruler_min Numeric, lower limit for label positions
#' 
estimate_y_label_position_vertical_plot = function(p, ylim, ruler_max, ruler_min) {

  n = length(p)
  sep = abs(ruler_max - ruler_min) * SEP_P

  ruler_min = as.numeric(ruler_min)
  ruler_max = as.numeric(ruler_max)

  x0 = initialize_x(p, sep, ruler_min, ruler_max)

  opts = list(
    algorithm = "NLOPT_LN_COBYLA",
    xtol_rel = 1.0e-3,
    maxeval = 10000
  )

  res = nloptr::nloptr(
    x0 = x0,
    eval_f = eval_f,
    lb = rep(ruler_min, n),
    ub = rep(ruler_max, n),
    opts = opts,
    p = p,
    ruler_min = ruler_min,
    ruler_max = ruler_max,
    sep = sep
  )
  
  return(round(res$solution, 3))
}


#' Ensures the minimum separation distance is met on the starting point and
#' prevents the last element from exceeding the limit.
#' 
#' @param p Numeric vector of initial label positions
#' @param sep Numeric, minimum required separation between labels
#' @param ruler_min Numeric, lower limit for label positions
#' @param ruler_max Numeric, upper limit for label positions
#' 
initialize_x = function(p, sep, ruler_min, ruler_max){
  
  for (i in 2:length(p)) {
    if (p[i] - p[i - 1] < sep) {
      p[i] = p[i - 1] + sep
    }
  }

  pmax(ruler_min, pmin(p, ruler_max))
}

#' Objective function for optimization, combines all penalty terms
#' 
#' @param x Numeric vector of adjusted label positions
#' @param p Numeric vector of initial label positions
#' @param ruler_min Numeric, lower limit for label positions
#' @param ruler_max Numeric, upper limit for label positions
#' @param sep Numeric, minimum required separation between labels
#' 
eval_f = function(x, p, ruler_min, ruler_max, sep) {
  WEIGHT_MOVEMENT * penalization_change(x, p) +
  WEIGHT_SEP * penalization_separation(x, sep)+
  WEIGHT_ORDER * eval_g_ineq(x)
}

#' Penalizes changes from the original positions
#' 
#' @param x Numeric vector of adjusted label positions
#' @param p Numeric vector of initial label positions
#' 
penalization_change = function(x, p) {
  sum((x - p)^2)
}

#' Penalizes label positions that do not maintain the minimum separation
#' 
#' @param x Numeric vector of adjusted label positions
#' @param sep Numeric, minimum required separation between labels
#' 
penalization_separation = function(x, sep) {
  
  if (length(x) < 2) return(0)
  
  pairs = combn(x, 2)
  diferences = abs(pairs[2, ] - pairs[1, ])
  sum((pmax(0, sep - diferences))^2)
}

#' Penalizes ordering violations (labels appearing in the wrong order)
#' 
#' @param x Numeric vector of adjusted label positions
#' 
eval_g_ineq = function(x) {
  sum(diff(x) < 0)
}
