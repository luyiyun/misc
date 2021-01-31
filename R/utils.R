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
.select_params <- function(dat, group, ...) {
  group <- enquo(group)
  vars <- enquos(...)
  if (length(vars) == 0) {
    vars <- base::setdiff(names(dat), as_name(group))
    vars <- syms(vars)
  }
  group <- pull(dat, !!group)
  vars <- select(dat, !!!vars)
  return(list(group, vars))
}
