#' Check whether Normality and homogeneity of variance are satisfied
#'
#' This function check whether the data satisfies both normality and homogeneity of variance. The \code{shapiro.test}
#' and \code{car::leveneTest} are used. It's worth noting that we default to check normality for every category and whole variable. Parameter
#' \code{.Ntest_object} is used to control this. In order to reduce the type II error, \code{.Nthre=0.1} and \code{.Ethre=0.1}
#' are default.
#'
#' @param dat data.frame
#' @param group The group variable with unquoted name
#' @param ... One or more unquoted expressions separated by commas. If none, use every variables else.
#' @param .N Whether need check normality, default TRUE.
#' @param .E Whether need check homogeneity of variance, default TRUE.
#' @param .Nthre Significance level for Normalization test, default 0.1.
#' @param .Ethre Significance level for variance equalization test, default 0.1.
#' @param .Ntest_object character, \code{"all"}, \code{"every"} or \code{"total"}. \code{"total"} means that
#' normality is checked for a whole variable. \code{"every"} means that normality is checked for every category of
#' variable. \code{"all"} means above two are both checked. Default "every".
#'
#' @return A list whose class is \code{check_NE}.
#' \describe{
#'   \item{methods}{
#'   Named character vector to represent the methods should used. If the the normality and homogeneity test can both pass,
#'   the variable uses \code{t.test} or \code{anova}. If the normality test can pass but homogeneity can't, the variable uses
#'   \code{t.test} (Welch t). If the normality test can't pass, \code{wilcox.test} or \code{kruskal.test} should be used.
#'   \code{NA} means that the variable isn't a numeric vector. Only the \code{.N=TRUE} and \code{.E=TRUE}, this item will be
#'   provided.
#'   }
#'   \item{logic_N}{Named logical vector to indicate which variable is normality.}
#'   \item{logic_E}{Named logical vector to indicate which variable is homogeneity of variable.}
#'   \item{pvalue_N}{Float vector of p value of normality test.}
#'   \item{pvalue_E}{Float vector of p value of homogeneity test of variable.}
#'   \item{test_method_N}{Until to now, it's just \code{"shapiro.test"}.}
#'   \item{test_method_E}{Until to now, it's just \code{"leveneTest"}.}
#'   \item{test_object_N}{It's the parameter \code{.Ntest_object}.}
#' }
#'
#' @seealso \code{\link{check_chisq}}
#' @export
#' @importFrom car leveneTest
#' @importFrom dplyr case_when
#'
#' @examples
#' check_NE(iris, Species)
#'
#'# settle data used to test
#' library(tidyverse)
#' dat <- mtcars %>% mutate(cyl=factor(cyl), vs=factor(vs), am=factor(am), gear=factor(gear), carb=factor(carb))
#' # test
#' dat %>% check_NE(cyl)
check_NE <- function(dat, group, ..., .N=TRUE, .E=TRUE, .Nthre=0.1, .Ethre=0.1, .Ntest_object="every"){
  preprocess <- .select_params(dat, !!enquo(group), ...)
  group <- preprocess[[1]]
  vars <- preprocess[[2]]
  .check_NE(group, vars, .N, .E, .Nthre, .Ethre, .Ntest_object)
}

.check_NE <- function(group, vars, .N, .E, .Nthre, .Ethre, .Ntest_object) {
  glevels <- levels(group)
  g_nlevel <- length(glevels)
  match.arg(.Ntest_object, c("all", "total", "every"))
  if ((!.N) & (!.E)) {
    stop("One of .N or .E must be TRUE.")
  }
  if (g_nlevel < 2) {
    stop("The nlevels of group variables must greater than or equal to 2.")
  }
  if (.Ntest_object == "total") {
    N_ncol <- 1
    N_colname <- "total"
  } else if (.Ntest_object == "every") {
    N_ncol <- g_nlevel
    N_colname <- glevels
  } else {
    N_ncol <- g_nlevel + 1
    N_colname <- c(glevels, "total")
  }
  res <- structure(
    list(
      # logic = structure(rep(NA, ncol(vars)), names = names(vars)),
      # pvalue_N = matrix(nrow = ncol(vars), ncol = N_ncol, dimnames = list(names(vars), N_colname)),
      # logic_N = matrix(nrow = ncol(vars), ncol = nlevels(group), dimnames = list(names(vars), glevels)),
      # pvalue_E = structure(rep(NA, ncol(vars)), names = names(vars)),
      # logic_E = structure(rep(NA, ncol(vars)), names = names(vars))
      test_method_N = "shapiro.test",
      test_method_E = "leveneTest",
      test_object_N = .Ntest_object
    ),
    class = "check_NE"
  )

  if (.N) {
    res$pvalue_N <- matrix(NA, nrow = ncol(vars), ncol = N_ncol, dimnames = list(names(vars), N_colname))
    for (i in seq_along(vars)) {
      v <- vars[[i]]
      if (!is.numeric(v)) next
      if (.Ntest_object != "every") {
        res$pvalue_N[i, N_ncol] <- shapiro.test(v)$p.value
      }
      if (.Ntest_object != "total") {
        for (j in seq_along(glevels)) {
          vl <- v[group == glevels[j]]
          res$pvalue_N[i, j] <- shapiro.test(vl)$p.value
        }
      }
    }
    res$logic_N <- res$pvalue_N > .Nthre
  }

  if (.E) {
    res$pvalue_E <- structure(rep(NA, ncol(vars)), names = names(vars))
    for (i in seq_along(vars)) {
      v <- vars[[i]]
      if (!is.numeric(v)) next
      levene.fit <- car::leveneTest(v, group)
      res$pvalue_E[i] <- levene.fit$`Pr(>F)`[1]
    }
    res$logic_E <- res$pvalue_E > .Ethre
  }

  if (.N & .E) {
    logic_N <- apply(res$logic_N, 1, all)
    res$methods <- case_when(
      is.na(logic_N) | is.na(res$logic_E) ~ NA_character_,
      logic_N & res$logic_E & (g_nlevel == 2) ~ "t",
      logic_N & res$logic_E & (g_nlevel > 2) ~ "anova",
      logic_N & (!res$logic_E) & (g_nlevel == 2) ~ "welch",
      logic_N & (!res$logic_E) & (g_nlevel > 2) ~ "anova",  # 暂时没有找到更加合适的方法
      !logic_N & (g_nlevel == 2) ~ "wilcox",
      TRUE ~ "kruskal"  # !logic_N，只剩下不是正态的情况了
    )
  } else if (.N) {
    logic_N <- apply(res$logic_N, 1, all)
    res$methods <- case_when(
      is.na(logic_N) ~ NA_character_,
      logic_N & (g_nlevel == 2) ~ "welch",  # 这里为了保险，对于所有的正态情况都使用welch t检验
      logic_N & (g_nlevel > 2) ~ "anova",
      !logic_N & (g_nlevel == 2) ~ "wilcox",
      TRUE ~ "kruskal"  # !logic_N，只剩下不是正态的情况了
    )
  } else {
    if (g_nlevel == 2) {
      res$methods <- ifelse(is.na(res$logic_E), NA_character_, "welch")  # 这里为了保险，对于所有的方差齐情况都使用welch t检验
    } else {
      res$methods <- case_when(
        is.na(res$logic_E) ~ NA_character_,
        res$logic_E ~ "anova",
        TRUE ~ "kruskal"
      )
    }
  }
  res
}
