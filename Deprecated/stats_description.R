#' Statistical descripation for data.frame
#'
#' @param dat data.frame
#' @param group The group variable unquoted name
#' @param ... One or more unquoted expressions separated by commas
#' @param .num_desc_type Character, "auto" , "median" and "mean"
#' @param .digit Integer, significant number of results retained.
#'
#' @return Tibble, the entries are characters.
#' @export
#'
#' @examples
#' stats_description(iris, Species)
#' stats_description(iris, Species, .num_desc_type = "median")
#' stats_description(iris, Species, .num_desc_type = "mean", .digit = 4)
stats_description <- function(dat, group, ...,
                              .num_desc_type = "auto", .digit = 2) {
  # 整理得到想要的vars
  group <- enquo(group)
  vars <- enquos(...)
  if (length(vars) == 0) {
    vars <- setdiff(names(dat), as_name(group))
    vars <- syms(vars)
  } else {
    vars <- c(vars, quo(-!!group))
  }
  group <- pull(dat, !!group)
  vars <- select(dat, !!!vars)

  .stats_description(vars, group, .num_desc_type, .digit)
}

.stats_description <- function(vars, group, .num_desc_type, .digit) {
  # 确定每个变量使用的描述方式
  l_isfct <- map_lgl(vars, is.factor)
  l_isdbl <- map_lgl(vars, is.double)
  .num_desc_type <- match.arg(.num_desc_type, c("auto", "mean", "median"))
  # 进行统计描述
  vars_names <- names(vars)
  res <- list()
  for (i in seq_len(ncol(vars))) {
    var <- vars[[i]]
    if (is.factor(var)) {
      tab <- desc_table(
        var, group, margin = "row", prob = "col", digit = .digit)
      tab <- tab %>% add_column(Categories = rownames(.), .before = 1) %>%
        as_tibble %>%
        add_column(Var = c(vars_names[i], rep(NA_character_, nrow(tab) - 1)),
                   .before = 1)
    } else if (is.double(var)) {
      tab <- desc_numeric(var, group, .num_desc_type, .digit) %>%
        add_column(Var = vars_names[i], .before = 1)
    } else {
      stop("The type of var must be factor or double.")
    }
    res[[i]] <- tab
  }
  res <- bind_rows(res)
  # 把Categorical移到前面去
  if ("Categories" %in% names(res)) {
    select(res, Var, Categories, everything())
  } else {
    res
  }
}
