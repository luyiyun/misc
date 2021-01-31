#' Statistical descripation for baseline variables
#'
#' This function can be used to generate the statistical describtion in publishing level. It create a
#' desc_baseline object, then \code{format} can be used to convert this object to a string dataframe publish.
#'
#'
#' @param dat data.frame
#' @param group The group variable unquoted name
#' @param ... One or more unquoted expressions separated by commas
#' @param .num_desc_type Character, "auto" , "median" or "mean". It's used to control the forms of numeric variable.
#' "mean" means "mean+-std". "median" means "median (25% quantile, 75% quantile)". "auto" means that forms are
#' controlled by the \code{check_NE} (Just look the results of normality test).
#' @param .num_margin Logical, whether caculate the numeric variables' statistics for all samples. Default TRUE.
#' @param .fct_margin Character, "none", "row", "col", "all". Whether margin counts will be calculated. If it's
#' "row", the counts of each row will be calculated. If "col", the counts of each col will be calculated. if
#' "all", above two will be both done.
#' @param .fct_prob Character, "none", "row", "col", "all". Whether the frenquency will be calculated. "row" means
#' the frequency will be calculated relative to row margin counts. As for "col", It's col margin counts. If "all",
#' the total counts will be used.
#' @param .check_NE_args If \code{.num_desc_type="auto"}, this is the list of parameters of \code{check_NE} expect \code{dat},
#' \code{group} and \code{...}. The values are fed as named list. Default is \code{list(.N = TRUE, .E = FALSE, .Nthre = 0.1, .Ethre = 0.1, .Ntest_object = "every")}
#'
#' @seealso \code{\link{check_NE}} \code{\link{format.desc_baseline}}
#'
#' @return desc_baseline object. As a matter of fact, it's a list.
#' \describe{
#'     \item{main}{list, each item is the result for one variable. For numeric variable, the item is a dataframe which
#'     columns are "mean", "std" for \code{.num_desc_type="mean"} or "median", "quan25" and "quan75" for \code{.num_desc_type="median"}.
#'     For factor, the item is a dataframe when \code{.fct_prob="none"}. If \code{.fct_prob} is not "none", the item
#'     will be a list that contains  count and prob.
#'     }
#'     \item{desc_methods}{Character vector that indicates the described type.}
#'     \item{vars, group}{stored original data.}
#' }
#'
#'
#' @export
#'
#'
#' @examples
#' desc_baseline(iris, Species) %>% format(digit=2)
desc_baseline <- function(
  dat, group, ...,
  .num_desc_type = "auto",
  .num_margin = TRUE,
  .fct_margin = "row",
  .fct_prob = "none",
  .check_NE_args=list()
)
{
  preprocess <- .select_params(dat, !!enquo(group), ...)
  group <- preprocess[[1]]
  vars <- preprocess[[2]]
  .desc_baseline(
    group, vars,
    .num_desc_type = .num_desc_type,
    .num_margin = .num_margin,
    .fct_margin = .fct_margin,
    .fct_prob = .fct_prob,
    .check_NE_args = .check_NE_args
  )
}


.desc_baseline <- function(
  group, vars,
  .num_desc_type,
  .num_margin,
  .fct_margin,
  .fct_prob,
  .check_NE_args
)
{
  .num_desc_type <- match.arg(.num_desc_type, c("auto", "mean", "median"))
  .default_check_NE_args <- list(  # 默认的check_NE的参数设置
    group = group,
    vars = vars,
    .N = TRUE,
    .E = FALSE,  # 在决定使用哪种统计描述方式的时候，不需要考虑方差齐性，只需要考虑正态性即可
    .Nthre = 0.1,
    .Ethre = 0.1,
    .Ntest_object = "every"
  )
  for (n in names(.check_NE_args)) {
    if (n %in% c(".E", ".Ethre")){
      warnings("决定统计描述方式时，不需要进行方差齐性检验！")
      next
    }
    .default_check_NE_args[[n]] <- .check_NE_args[[n]]
  }
  # 确定每个变量的描述方式（这里对于factor的变量，只有一种方式，所以就不需要进行check了）
  if (.num_desc_type == "auto") {
    # 这里check的时候，只进行了正态性的检验，
    NE_check_res <- do.call(.check_NE, .default_check_NE_args)$logic_N
    use_methods <- rep(NA, ncol(vars))
    for (i in seq_along(vars)) {
      if (is.na(NE_check_res[i])) {
        # 这是一个factor向量
        use_methods[i] <- "table"
      } else {
        # 这是一个numeric向量
        if (NE_check_res[i]) {
          # 满足正态，使用mean
          use_methods[i] <- "mean"
        } else {
          # 不满足正态，使用median
          use_methods[i] <- "median"
        }
      }
    }
  } else if (.num_desc_type == "mean") {
    use_methods <- sapply(vars, function(x) {if (is.factor(x)) {"table"} else {"mean"}})
  } else {
    use_methods <- sapply(vars, function(x) {if (is.factor(x)) {"table"} else {"median"}})
  }
  # 进行统计描述
  main_res <- mapply(
    function(v, m) {
      switch (
        m,
        table = .desc_table(v, group, margin = .fct_margin, prob = .fct_prob),
        mean = .desc_me(v, group, margin = .num_margin, method = "mean"),
        median = .desc_me(v, group, margin = .num_margin, method = "median")
      )
    },
    vars, use_methods,
    SIMPLIFY = FALSE, USE.NAMES = TRUE
  )

  structure(
    list(
      main = main_res,
      desc_methods = structure(use_methods, names = names(vars)),
      vars = vars,
      group = group
    ),
    class = "desc_baseline"
  )
}


.desc_table <- function(factor1, factor2, margin = "none", prob="none")
{
  stopifnot(is.factor(factor1))
  stopifnot(is.factor(factor2))
  margin <- match.arg(margin, c("none", "row", "col", "all"))
  prob <- match.arg(prob, c("none", "row", "col", "all"))

  tab <- unclass(table(factor1, factor2))
  if (margin == "none" & prob == "none") {
    return(list(
      count=as.data.frame(tab, stringAsFactors = FALSE),
      prob=NULL
    ))
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
  } else {
    tab_prob <- NULL
  }

  if (prob == "none") {
    return(list(
      count=as.data.frame(tab, stringAsFactors = FALSE),
      prob=NULL
    ))
  } else {
    return(list(
      count=as.data.frame(tab, stringAsFactors = FALSE),
      prob=as.data.frame(tab_prob, stringAsFactors = FALSE)
    ))
  }

}

.desc_me <- function(v, group, margin = FALSE, method = "mean") {
  stopifnot(is.factor(group))
  stopifnot(is.numeric(v))
  gl <- levels(group)

  if (margin) {
    if ("all" %in% gl) {
      stop("all不能是group中的分类名，请将其修改！")
    }
    gl <- c(gl, "all")
  }

  if (method == "mean") {
    .func <- .f_mean_sd
    res_colnames <- c("mean", "std")
  } else{
    .func <- .f_median_quan
    res_colnames <- c("median", "quan25", "quan75")
  }

  res_mat <- t(mapply(.func, gl, MoreArgs = list(group = group, values = v)))
  res_df <- data.frame(res_mat)
  names(res_df) <- res_colnames
  res_df["Categories"] <- gl
  res_df <- res_df[c("Categories", res_colnames)]
  return(res_df)
}

.f_mean_sd <- function(cat, group, values) {
  if (cat == "all") {
    values_ <- values
  } else {
    values_ <- values[group == cat]
  }
  return(c(mean(values_, na.rm = TRUE), sd(values_, na.rm = TRUE)))
}

.f_median_quan <- function(cat, group, values) {
  if (cat == "all") {
    values_ <- values
  } else {
    values_ <- values[group == cat]
  }
  quans <- quantile(values_, probs = c(0.5, 0.25, 0.75), na.rm = TRUE, names = FALSE)
  return(quans)
}
