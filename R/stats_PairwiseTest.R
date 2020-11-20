#' Pairwise Testing
#'
#' This function is used to compare all pairs of categories of \code{group} variable.
#' This pairwise test methods used contain bonferroni adjust of t, wilcox and chisq, and tukey method
#' for continues variables.
#'
#' @param x \code{stats_SingleTest} object.
#' @param .just_for_sign Logical value. If \code{TRUE}, just test for variables that be tested as significant in \code{stats_SingleTest}.
#' @param .sign_thre Float threshold used to decide which variable is significant. Default 0.05.
#' @param dat data.frame.
#' @param group The group variable unquoted name
#' @param ... One or more unquoted expressions separated by commas
#' @param .num_method Character. "auto", "tukey", "t", "wilcox".
#' @param .fct_method Character. "auto", "chisq", "fisher".
#' @param .adjust_method Character. "holm", "bonferroni", "hochberg", "hommel".
#' @param .check_NE_args Named list. If \code{.num_method} is "auto", these are the parameters of \code{.check_NE}.
#' The default parameters are \code{list(.N = TRUE, .E = TRUE, .Nthre = 0.1, .Ethre = 0.1, .Ntest_object="every")}.
#'
#' @return \code{stats_PairwiseTest} object.
#' \describe{
#'   \item{main}{This is a list whose items are 2-length lists which contains the pairwise test results.
#'     The two items are \code{p_value}(named matrix) and \code{method}.}
#'   \item{SingleTest}{If the input is \code{stats_SingleTest} object, this is it.}
#'   \item{group, vars}{Stored original data.}
#' }
#'
#' @export
#'
#' @examples
#' stats_PairwiseTest(iris, Species)
stats_PairwiseTest <- function(x, ...) {
  UseMethod("stats_PairwiseTest")
}

#' @describeIn stats_PairwiseTest Pairwise Test for data.frame.
#' @export
stats_PairwiseTest.data.frame <- function(
  dat, group, ..., .num_method = "auto", .fct_method = "auto",
  .adjust_method = "holm", .check_NE_args = list()
)
{
  preprocess <- select_params(dat, !!enquo(group), ...)
  group <- preprocess[[1]]
  vars <- preprocess[[2]]
  .stats_PairwiseTest(vars, group, .num_method, .fct_method, .adjust_method, .check_NE_args)
}

#' @describeIn stats_PairwiseTest Pairwise Test for \code{stats_SingleTest} object. The test method will be choose
#' by the \code{x$test_methods}. \code{"anova"} uses \code{"tukey"}, \code{"kwh"} uses \code{"wilcox"},
#' \code{"chisq"} uses \code{"chisq"}, and \code{"fisher"} uses \code{"fisher"}. The \code{"wilcox"}, \code{"chisq"} and \code{"fisher"}
#' will be adjusted by \code{".adjust_method"}.
#'
#' @export
stats_PairwiseTest.stats_SingleTest <- function(x, .adjust_method = "holm", .just_for_sign = FALSE, .sign_thre = 0.05)
{
  var_names <- names(x$vars)
  if (.just_for_sign) {
    use_names <- c()
    use_methods <- c()
    for (n in var_names) {
      if (x$main[[n]]$p_value < .sign_thre) {
        use_names <- c(use_names, n)
        use_methods <- c(
          use_methods,
          switch (x$main[[n]]$method,
            anova = "tukey",
            kruskal = "wilcox",
            chisq = "chisq",
            fisher = "fisher"
          )
        )
      }
    }
    use_vars <- x$vars[, use_names]
  } else {
    use_methods <- c()
    for (n in var_names) {
      use_methods <- c(
        use_methods,
        switch (x$main[[n]]$method,
                anova = "tukey",
                kruskal = "wilcox",
                chisq = "chisq",
                fisher = "fisher"
        )
      )
    }
    use_vars <- x$vars
  }
  res <- .stats_PairwiseTest_(use_vars, x$group, use_methods, .adjust_method)
  res[["SingleTest"]] <- x
  res
}


.stats_PairwiseTest <- function(vars, group, .num_method, .fct_method, .adjust_method="holm", .check_NE_args = list()) {

  if (nlevels(group) <= 2) {
    stop("The nlevels of group variable must be greater than 2.")
  }

  all_adjust_methods <- c("holm", "bonferroni", "hochberg", "hommel")
  all_num_methods <- c("auto", "tukey", "t", "wilcox")
  all_fct_methods <- c("auto", "chisq", "fisher")

  .adjust_method <- match.arg(.adjust_method, all_adjust_methods)
  .num_method <- match.arg(.num_method, all_num_methods)
  .fct_method <- match.arg(.fct_method, all_num_methods)
  .default_check_NE_args <- list(
    group = group, vars = vars, .N = TRUE, .E = TRUE, .Nthre = 0.1, .Ethre = 0.1,
    .Ntest_object = "every"
  )
  for (n in names(.check_NE_args)) {
    .default_check_NE_args[[n]] <- .check_NE_args[[n]]
  }

  use_methods <- rep(NA_character_, ncol(vars))
  # 确定每个变量要使用的方法
  if (.num_method == "auto") {
    conti_methods <- do.call(.check_NE, .default_check_NE_args)$methods
    use_methods <- case_when(
      conti_methods == "anova" ~ "tukey",
      conti_methods == "kruskal" ~ "wilcox",
      TRUE ~ use_methods
    )
  } else {
    for (i in seq_along(use_methods)) {
      if (is.numeric(vars[[i]])) {
        use_methods[i] <- .num_method
      }
    }
  }
  if (.fct_method == "auto") {
    fct_methods <- .check_chisq(group, vars)$methods
    use_methods <- case_when(
      fct_methods == "chisq" ~ "chisq",
      fct_methods == "fisher" ~ "fisher",
      TRUE ~ use_methods
    )
  } else {
    for (i in seq_along(use_methods)) {
      if (is.factor(vars[[i]])) {
        use_methods[i] <- .fct_method
      }
    }
  }

  # 进行检验
  .stats_PairwiseTest_(vars, group, use_methods, .adjust_method)
}

.stats_PairwiseTest_ <- function(vars, group, use_methods, adjust_method) {

  var_names <- names(vars)
  # 每个变量使用其最适合的方法进行统计检验
  res <- list()
  for (i in seq_len(ncol(vars))) {
    v <- vars[[i]]
    res[[var_names[i]]] <- switch(use_methods[i],
                  tukey = pairwise_tukey(v, group),
                  t = pairwise_t(v, group, adjust_method),
                  wilcox = pairwise_wilcox(v, group, adjust_method),
                  chisq = pairwise_chisq(v, group, adjust_method),
                  fisher = pairwise_fisher(v, group, adjust_method))
  }

  structure(
    list(
      main = res,
      group = group,
      vars = vars
    ),
    class = "stats_PairwiseTest"
  )
}


# single test function
pairwise_tukey <- function(v, group) {
  aov.fit <- aov(v~group, data = data.frame(v=v, group=group))
  tukey.fit <- TukeyHSD(aov.fit)

  tukey_res <- tukey.fit$group
  index <- rownames(tukey_res)
  index <- strsplit(index, "-")

  gl <- levels(group)
  res_list <- list(method = "tukey")
  for (i in seq_len(ncol(tukey_res))) {
    mat <- matrix(nrow = nlevels(group)-1, ncol = nlevels(group)-1, dimnames = list(gl[-1], gl[-nlevels(group)]))
    for (j in seq_len(nrow(tukey_res))) {
      mat[index[[j]][1], index[[j]][2]] <- tukey_res[j, i]
    }
    if (colnames(tukey_res)[i] == "p adj") {
      res_list[["p_value"]] <- mat
    } else {
      res_list[[colnames(tukey_res)[i]]] <- mat
    }
  }

  res_list
}


pairwise_t <- function(v, group, adjust_method) {
  list(p_value = pairwise.t.test(v, group, p.adjust.method = adjust_method)$p.value,
       method = paste0(adjust_method, "_t"))
}

pairwise_wilcox <- function(v, group, adjust_method) {
  list(p_value = pairwise.wilcox.test(v, group, p.adjust.method = adjust_method)$p.value,
       method = paste0(adjust_method, "_wilcox"))
}

pairwise_chisq <- function(v, group, adjust_method) {
  chisq_xy <- function(x, y) {
    lgl_index <- as.integer(group) %in% c(x, y)
    fct1_ <- droplevels(v[lgl_index])
    fct2_ <- droplevels(group[lgl_index])
    tab <- table(fct1_, fct2_)
    chisq_fit <- chisq.test(tab)
    chisq_fit$p.value
  }
  list(p_value = pairwise.table(chisq_xy, levels(group), adjust_method),
       method = paste0(adjust_method, "_chisq"))
}


pairwise_fisher <- function(v, group, adjust_method) {
  fisher_xy <- function(x, y) {
    lgl_index <- as.integer(group) %in% c(x, y)
    fct1_ <- droplevels(v[lgl_index])
    fct2_ <- droplevels(group[lgl_index])
    tab <- table(fct1_, fct2_)
    fisher_fit <- fisher.test(tab)
    fisher_fit$p.value
  }
  list(p_value = pairwise.table(fisher_xy, levels(group), adjust_method),
       method = paste0(adjust_method, "_fisher"))
}
