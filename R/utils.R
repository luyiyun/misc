#' Select the group variable and other target variables as a vector and data.frame from dat
#'
#' @param dat Data
#' @param group unquoted variable name for group
#' @param ... One or more unquoted expressions separated by commas
#'
#' @importFrom rlang enquo enquos syms as_name
#' @importFrom dplyr pull select
#'
#' @return list of vector and data.frame
#'
#' @examples
#' select_params(iris, Species)
select_params <- function(dat, group, ...) {
  group <- enquo(group)
  vars <- enquos(...)
  if (length(vars) == 0) {
    vars <- base::setdiff(names(dat), as_name(group))
    vars <- syms(vars)
  } else {
    vars <- c(vars, quo(-!!group))
  }
  group <- pull(dat, !!group)
  vars <- select(dat, !!!vars)
  return(list(group, vars))
}


round2char <- function(x, digit = 2, zero_pre = "< ", nozero_pre = "= ") {
  zero_str <- sprintf(paste0(zero_pre, "%.", digit, "f"), x)
  nozero_str <- sprintf(paste0(nozero_pre, "%.", digit, "f"), x)
  ifelse(abs(x) > 5^(-digit-1), nozero_str, zero_str)
}
