#' Single Null Hypothesis Significant Test (NHST) for selected variables of data.frame
#'
#' @param dat data.frame
#' @param group The group variable unquoted name.
#' @param ... One or more unquoted expressions separated by commas. If none, use every variables else.
#' @param .desc Logical, whether calculate \code{desc_baseline} object.
#' @param .num_method Character, "auto", "t", "anova", "wilcox", "kruskal". If \code{nlevels(group)} > 2 and "t" or
#' "wilcox" are chosen, a warning will be throwed and the parameter will be replaced by "anova" or "kruskal". Default
#' is "auto", the test method of numeric variables will be decided by \code{.check_NE}. The "t" means t test with Welch
#' (or Satterthwaite) approximation of the degrees of freedom.
#' @param .fct_method Character, "auto", "chisq", "fisher". Default is "auto", decided by \code{.check_chisq}.
#' @param .check_NE_args Named list. If \code{.num_method} is "auto", these are the parameters of \code{.check_NE}.
#' The default parameters are \code{list(.N = TRUE, .E = TRUE, .Nthre = 0.1, .Ethre = 0.1, .Ntest_object="every")}.
#' @param .desc_baseline_args Named list. If \code{.desc=TRUE}, these are the parameters of \code{.desc_baseline}.
#' The default parameters are \code{.num_desc_type = "auto", .num_margin = TRUE, .fct_margin = "all", .fct_prob = "col", .check_NE_args = list()}.
#' Because the parameters will pass into \code{desc_baseline}, \code{.check_NE_args} will use the default values of
#' \code{desc_baseline}.
#'
#' @return stats_SingleTest object.
#' \describe{
#'     \item{main}{This is a list whose items are 3-length lists which contains the NHST results. The three items
#'     are \code{statistic}, \code{p_value} and \code{method}.}
#'     \item{test_methods}{Named vector. Its items are the \code{method} of every variable used for NHST.}
#'     \item{desc}{The \code{desc_baseline} object. See \code{\link{desc_baseline}}.}
#'     \item{vars, group}{Stored original data.}
#' }
#'
#' @export
#'
#' @examples
#' stats_SingleTest(iris, Species)
#' stats_SingleTest(iris, Species, .desc = FALSE)
stats_SingleTest <- function(
  dat, group, ...,
  .desc = TRUE,
  .num_method = "auto",
  .fct_method = "auto",
  .check_NE_args = list(),
  .desc_baseline_args = list()
)
{
  preprocess <- .select_params(dat, !!enquo(group), ...)
  group <- preprocess[[1]]
  vars <- preprocess[[2]]
  .stats_SingleTest(vars, group, .desc, .num_method, .fct_method, .check_NE_args, .desc_baseline_args)
}

.stats_SingleTest <- function(
  vars, group,
  .desc,
  .num_method,
  .fct_method,
  .check_NE_args,
  .desc_baseline_args
)
{
  .num_method <- match.arg(.num_method, c("auto", "t", "welch", "anova", "wilcox", "kruskal"))
  .fct_method <- match.arg(.fct_method, c("auto", "chisq", "fisher"))
  nl <- nlevels(group)
  if (nl > 2 & .num_method %in% c("t", "welch", "wilcox")) {
    old_meth <- .num_method
    new_meth <- ifelse(.num_method %in% c("t", "welch"), "anova", "kwh")
    warning(sprintf("The number of categories of group more than 2, you should use %s, not %s", new_meth, old_meth))
    .num_method <- new_meth
  }

  .default_check_NE_args <- list(
    group = group,
    vars = vars,
    .N = TRUE,
    .E = TRUE,
    .Nthre = 0.1,
    .Ethre = 0.1,
    .Ntest_object="every"
  )
  for (n in names(.check_NE_args)) {
    .default_check_NE_args[[n]] <- .check_NE_args[[n]]
  }

  # 1. 先去判断每个变量应该使用的统计方法
  use_methods <- structure(
    rep(NA_character_, ncol(vars)),
    names = names(vars)
  )
  if (.num_method == "auto") {
    conti_methods <- do.call(.check_NE, .default_check_NE_args)$methods
    use_methods <- ifelse(is.na(conti_methods), use_methods, conti_methods)
  } else {
    use_methods <- ifelse(sapply(vars, is.numeric), .num_method, use_methods)
  }
  if (.fct_method == "auto") {
    fct_methods <- .check_chisq(group, vars)$methods
    use_methods <- ifelse(is.na(fct_methods), use_methods, fct_methods)
  } else {
    use_methods <- ifelse(sapply(vars, is.numeric), .fct_method, use_methods)
  }

  # 2. 对照需要使用的方法，进行分别进行统计学检验
  main <- mapply(
    function(v, m) {
      switch(
        m,
        chisq = chisq_test(v, group),
        fisher = fisher_test(v, group),
        t = t_test(v, group),
        welch = t_test(v, group, TRUE),
        anova = anova_test(v, group),
        wilcox = wilcox_test(v, group),
        kruskal = kwh_test(v, group)
      )
    },
    vars, use_methods,
    SIMPLIFY = FALSE, USE.NAMES = TRUE
  )

  # 3. 将其他的一些内容加入其中，将结果转换成stats_SingleTest对象并返回
  if (.desc) {
    .default_desc_baseline_args <- list(
      group = group,
      vars = vars,
      .num_desc_type = "auto",
      .num_margin = TRUE,
      .fct_margin = "all",
      .fct_prob = "col",
      .check_NE_args = list()
    )
    for (n in names(.desc_baseline_args)) {
      .default_desc_baseline_args[[n]] <- .desc_baseline_args[[n]]
    }
    desc <- do.call(.desc_baseline, .default_desc_baseline_args)
  } else {
    desc <- NULL
  }

  structure(
    list(
      main = main,
      test_methods = use_methods,
      desc = desc,
      group = group,
      vars = vars
    ),
    class = "stats_SingleTest"
  )
}



# statistical test
chisq_test <- function(var, group) {
  n_cat <- length(levels(var))
  chisq.fit <- chisq.test(table(var, group))
  list(
    method = "chisq",
    statistic = chisq.fit$statistic,
    p_value = chisq.fit$p.value
  )
}

fisher_test <- function(var, group) {
  n_cat <- length(levels(var))
  fisher.fit <- fisher.test(table(var, group), simulate.p.value = TRUE)
  list(
    method = "fisher",
    statistic = NA_real_,
    p_value = fisher.fit$p.value
  )
}

t_test <- function(var, group, welch=TRUE) {
  t.fit <- t.test(var~group, data = data.frame(var=var, group=group), var.equal = !welch)
  list(
    method = if (welch) "welch" else "t",
    statistic = t.fit$statistic,
    p_value = t.fit$p.value
  )
}

anova_test <- function(var, group) {
  aov.fit <- summary(aov(var~group,
                         data = data.frame(var=var, group=group)))[[1]]
  list(
    method = "anova",
    statistic = aov.fit$`F value`[1],
    p_value = aov.fit$`Pr(>F)`[1]
  )
}

wilcox_test <- function(var, group) {
  wilcox.fit <- wilcox.test(var~group, data = data.frame(var=var, group=group))
  list(
    statistic = wilcox.fit$statistic,
    p_value = wilcox.fit$p.value,
    method = "wilcox")
}

kwh_test <- function(var, group) {
  kwh.fit <- kruskal.test(var~group, data = data.frame(var=var, group=group))
  list(
    statistic = kwh.fit$statistic,
    p_value = kwh.fit$p.value,
    method = "kruskal"
  )
}

