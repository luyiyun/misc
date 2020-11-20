#' Multiple elements Analysis for data.frame
#'
#' @param formula formula, like \code{glm}
#' @param dat data.frame
#' @param .digit Integer, significant number of results retained.
#'
#' @return tibble, the element is character
#' @export
#'
#' @importFrom tibble rownames_to_column add_column
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace str_starts
#' @importFrom rlang f_lhs
#' @importFrom MASS polr dropterm
#'
#' @examples
#' stats_MultiTest(mpg~., mtcars)
stats_MultiTest <- function(formula, dat, .digit = 2) {
  .stats_MultiTest(formula, dat, .digit = .digit)
}


.stats_MultiTest <- function(formula, dat, .digit) {
  response <- dat[[as.character(f_lhs(formula))]]
  # 确定使用的方法
  if (is.double(response)) {
    .types <- "linear"
  } else if (is.factor(response) & nlevels(response) == 2) {
    .types <- "logistic"
  } else if (is.factor(response) & nlevels(response) > 2) {
    .types <- "order"
  } else {
    stop("Not Implement Error.")
  }

  # 进行数据分析
  res <- switch (
    .types,
    linear = .linear_MultiTest(formula, dat, .digit),
    logistic = .logistic_MultiTest(formula, dat, .digit),
    order = .order_MultiTest(formula, dat, .digit)
  )
  # 规范有效数字，并将全部的元素变成chr
  map_dfc(res, function(x) {
    if (is.character(x)) {
      x
    } else if (is.double(x)) {
      round(x, digits = .digit) %>% as.character
    } else {
      as.character(x)
    }
  })
}


.linear_MultiTest <- function(formula, dat, .digit) {
  lm_fit <- lm(formula = formula, data = dat)
  lm_summ <- summary(lm_fit)
  Coefs <- round(lm_summ$coefficients, .digit) %>% as.data.frame %>%
    rownames_to_column("predictors")

  CIs <- round(confint(lm_fit), .digit)
  CIs <- sprintf("%s - %s", as.character(CIs[, 1]), as.character(CIs[, 2]))

  Coefs <- Coefs %>% add_column(`CI (95%)` = CIs, .before = "t value")
  return(.clear_varname(Coefs, dat))
}

.logistic_MultiTest <- function(formula, dat, .digit) {
  logit_fit <- glm(formula = formula, data = dat, family = binomial())
  logit_summ <- summary(logit_fit)
  Coefs <- round(logit_summ$coefficients, .digit) %>% as.data.frame %>%
    rownames_to_column("predictors")

  ssconf <-  suppressMessages(confint(logit_fit))
  CIs <- round(ssconf, .digit)
  CIs_or <- round(exp(ssconf), .digit)
  CIs_coef <- sprintf("%s - %s", as.character(CIs[, 1]), as.character(CIs[, 2]))
  CIs_or <- sprintf("%s - %s", as.character(CIs_or[, 1]), as.character(CIs_or[, 2]))

  Coefs <- Coefs %>% add_column(`CI (Coef 95%)` = CIs_coef, .after = "Std. Error") %>%
    add_column(OR = exp(Coefs$Estimate), .after = "CI (Coef 95%)") %>%
    add_column(`CI (OR 95%)` = CIs_or, .after = "OR") %>%
    add_column(Wald = Coefs$`z value` ^ 2, .after = "z value")

  return(.clear_varname(Coefs, dat))
}

.order_MultiTest <- function(formu, dat, .digit) {
  fit <- MASS::polr(formu, dat, Hess = TRUE)
  summ <- summary(fit)
  Coefs <- round(summ$coefficients, .digit) %>% as.data.frame %>%
    rownames_to_column("predictors")
  Coefs <- Coefs %>% .[1:(nrow(Coefs) - length(fit$lev) + 1),] # 去掉截距项

  ssconf <-  suppressMessages(confint(fit))
  CIs <- round(ssconf, .digit)
  CIs_or <- round(exp(ssconf), .digit)
  CIs_coef <- sprintf("%s - %s", as.character(CIs[, 1]), as.character(CIs[, 2]))
  CIs_or <- sprintf("%s - %s", as.character(CIs_or[, 1]), as.character(CIs_or[, 2]))

  # 需要另外得到p值
  test_res <- dropterm(fit, test = "Chi")

  Coefs <- Coefs %>% add_column(`CI (Coef 95%)` = CIs_coef, .after = "Std. Error") %>%
    add_column(OR = exp(Coefs$Value), .after = "CI (Coef 95%)") %>%
    add_column(`CI (OR 95%)` = CIs_or, .after = "OR") %>%
    # add_column(Wald = Coefs$`z value` ^ 2, .after = "z value")
    add_column(`P Value` = test_res$`Pr(Chi)`[-1])

  return(.clear_varname(Coefs, dat))
}

.clear_varname <- function(Coefs, df) {
  # 将Coefs中涉及到的变量名按其中的顺序整理一遍
  fit_names <- Coefs$predictors
  for (v in names(df)) {
    # index <- grep(v, fit_names)  # 用str_detect会出错，比如其不能识别地区南方中的地区
    # 使用grep对于LDL，VLDL这样的会出错
    # index <- str_starts(fit_names, v)  # string_r包中对于中文支持不够
    v_len <- nchar(v)
    index <- c()
    for (i in seq_along(fit_names)) {
      fit_name <- fit_names[i]
      index <- c(index, substr(fit_name, start = 1, stop = v_len) == v)
    }
    fit_names[index] <- v
  }
  fit_names <- fit_names[!(fit_names %>% duplicated)]
  # 为每个分类变量将其分类补全
  dfs <- list()
  for (v in fit_names) {
    if (is.factor(df[[v]]) | is.character(df[[v]])) {
      n_level <- nlevels(df[[v]])
      res_dfi <- matrix(nrow = n_level, ncol = ncol(Coefs) + 1) %>% as.data.frame
      names(res_dfi) <- c("Preds", "Cats", names(Coefs)[-1])

      index <- grep(v, Coefs$predictors)

      res_dfi[1, 1] <- v
      res_dfi[, 2] <- levels(df[[v]])
      res_dfi[1, 2] <- paste0(res_dfi[1, 2], "(Control)")
      for (i in 3:ncol(res_dfi)) {
        res_dfi[2:n_level, i] <- Coefs[index, i-1]
      }
    } else if (is.double(df[[v]]) | v == "(Intercept)") {
      res_dfi <- matrix(nrow = 1, ncol= ncol(Coefs) + 1) %>% as.data.frame()
      names(res_dfi) <- c("Preds", "Cats", names(Coefs)[-1])
      res_dfi[1, 1] <- v

      res_dfi[1, 3:ncol(res_dfi)] <- Coefs[Coefs$predictors == v, -1]
    } else {
      stop(sprintf("The type of %s is not matched.", v))
    }
    dfs[[v]] <- res_dfi
  }
  bind_rows(dfs)
}
