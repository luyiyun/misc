#' Correlation Analysis
#'
#' @param dat DataFrame.
#' @param ... if NULL, use all variables, otherwise the variables here will be calculated.
#' @param .method Default "spearman", "pearson".
#'
#' @return An stats_Correlation object.
#' @export
#'
#' @examples
stats_Correlation <- function(dat, ..., .method = "spearman")
{
  vars <- enquos(...)
  if (length(vars) == 0) {
    use_dat <- dat
  } else {
    use_dat <- select(dat, !!!vars)
  }

  .stats_Correlation(use_dat, .method)
}

.stats_Correlation <- function(use_dat, .method) {
  use_dat <- as.data.frame(purrr::map(use_dat, as.numeric))
  nc <- ncol(use_dat)
  res <- structure(
    matrix(NA_real_, nrow = nc, ncol = nc,
           dimnames = list(names(use_dat), names(use_dat))),
    class = c("stats_Correlation", "matrix")
  )
  for (i in seq_len(nc)) {
    if (i == nc) break
    for (j in seq((i+1), nc)) {
      fit <- cor.test(use_dat[[i]], use_dat[[j]], method = .method)
      res[i, j] <- fit$estimate
      res[j, i] <- fit$p.value
    }
  }
  return(res)
}
