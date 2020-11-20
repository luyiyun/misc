#' Statistical description of continuous variables
#'
#' This function can only deal with one continuous variable. The output is
#' tibble whose entries are characters.
#'
#' @param var Double vector, continuous variable.
#' @param group Factor vector, group variable.
#' @param type String, "auto", "mean" or "median".
#' @param digit Integer, significant number of results retained.
#'
#' @return Tibble, the entries are characters.
#' @export
#' @importFrom dplyr group_by summarise mutate select %>% tibble
#' @importFrom tibble column_to_rownames as_tibble
#'
#' @details
#' If you want to handle the categorical variable, use \code{desc_table}.
#' This function can only handle the case of one continuous variable. If there
#' are multiple variables should be handled, use \code{stats_description}.
#'
#' @seealso
#' \code{\link{desc_table}}
#' \code{\link{stats_description}}
#'
#' @examples
#' desc_numeric(iris$Sepal.Length, iris$Species, "mean", 2)
desc_numeric <- function(var, group, type = "auto", digit = 2) {
  stopifnot(is.double(var))
  stopifnot(is.factor(group))
  stopifnot(length(var) == length(group))
  type = match.arg(type, c("auto", "mean", "median"))

  if (type == "auto") {
    if (decide_isvarhomo(var, group, 0.1)) {
      type <- "mean"
    } else{
      type <- "median"
    }
  }

  df <- data.frame(group = group, var = var, stringsAsFactors = FALSE)
  df_group <- group_by(df, group)
  if (type == "mean") {
    summ_res <- summarise(df_group,
                          location = mean(var, na.rm = TRUE),
                          scale = sd(var, na.rm = TRUE)
                          ) %>%
      mutate(group = as.character(group)) %>%
      bind_rows(tibble(group = "all",
                       location = mean(var, na.rm = TRUE),
                       scale = sd(var, na.rm = TRUE))) %>%
      mutate(use = sprintf(paste0("%.", digit, "f+-%.", digit, "f"), location, scale))
  } else if (type == "median") {
    summ_res <- summarise(df_group,
                          location = median(var, na.rm = TRUE),
                          scale1 = quantile(var, 0.25, na.rm = TRUE),
                          scale2 = quantile(var, 0.75, na.rm = TRUE)
                          ) %>%
      mutate(group = as.character(group)) %>%
      bind_rows(tibble(group = "all",
                       location = median(var, na.rm = TRUE),
                       scale1 = quantile(var, 0.25, na.rm = TRUE),
                       scale2 = quantile(var, 0.75, na.rm = TRUE))) %>%

      mutate(use = sprintf(paste0("%.", digit, "f(%.", digit, "f, %.", digit, "f)"),
                           location, scale1, scale2))
  }
  summ_res %>% select(group, use) %>%
    column_to_rownames("group") %>%
    as.matrix %>% t %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    as_tibble
}
