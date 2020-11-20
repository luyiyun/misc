#' Cross Tabulation with Margin and Probability
#'
#' An upgraded version of the \code{table} that can calculate marginal counts
#' and probabilites.
#'
#' @param factor1 Factor vector.
#' @param factor2 Factor vector, the length is equal to that of \code{factor1}.
#' @param margin The strings for computing the sum of table, default "none",
#' or "row", "col" and "all".
#' @param prob The strings for computing the probabilities, default "none", or
#' "row", "col", "all".
#' @param digit Positive integer indicating the number of decimal places to be
#' used.
#'
#' @return A data.frame. If \code{prob = "none"}, all entries is characters.
#' @export
#' @seealso
#' \code{\link{desc_numeric}}
#' \code{\link{stats_description}}
#'
#' @examples
#' fct1 <- factor(c(rep("x1", 10), rep("x2", 10)))
#' fct2 <- factor(c(rep("y1", 5), rep("y2", 5), rep("y3", 5), rep("y4", 5)))
#' desc_table(fct1, fct2)
desc_table <- function(factor1, factor2, margin = "none",
                       prob="none", digit = 2)
{
  stopifnot(is.factor(factor1))
  stopifnot(is.factor(factor2))
  margin <- match.arg(margin, c("none", "row", "col", "all"))
  prob <- match.arg(prob, c("none", "row", "col", "all"))

  tab <- unclass(table(factor1, factor2))
  if (margin == "none" & prob == "none") {
    return(as.data.frame(tab, stringsAsFactors = FALSE))
  }
  if (margin == "row" | margin == "all") {
    tab <- cbind(tab, apply(tab, 1, sum))
    colnames(tab)[ncol(tab)] <- "all"
  }
  if (margin == "col" | margin == "all") {
    tab <- rbind(tab, apply(tab, 2, sum))
    rownames(tab)[nrow(tab)] <- "all"
  }

  if (prob == "row") {
    if ("all" %in% colnames(tab)) {
      denominator <- tab[, "all"]
    } else {
      denominator <- apply(tab, 1, sum)
    }
    tab_prob <- tab / rep_len(denominator, length.out = length(tab))
  } else if (prob == "col") {
    if ("all" %in% rownames(tab)) {
      denominator <- tab["all", ]
    } else {
      denominator <- apply(tab, 2, sum)
    }
    tab_prob <- t(t(tab) / rep_len(denominator, length.out = length(tab)))
  } else if (prob == "all") {
    tab_prob <- tab / length(factor1)
  }

  if (prob != "none") {
    attrs <- attributes(tab)
    tab <- sprintf(paste0("%d(%.", digit, "f%%)"), tab, tab_prob * 100)
    attributes(tab) <- attrs
  }
  return(as.data.frame(tab, stringsAsFactors = FALSE))
}
