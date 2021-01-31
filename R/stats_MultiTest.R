#' Multiple elements Analysis for data.frame
#'
#' @param formula formula, like \code{glm}
#' @param obj \code{lm}, \code{glm} or \code{polr} object.
#' @param dat data.frame
#' @param ... other parameters for \code{lm}, \code{glm} or \code{MASS::polr}.
#'
#' @return a \code{stat_MultiTest} object.
#' @export
#'
#' @importFrom MASS polr
#'
#' @examples
#' stats_MultiTest(mpg~., mtcars)
stats_MultiTest <- function(x, ...) {
  UseMethod("stats_MultiTest")
}

#' @rdname stats_MultiTest
#' @export
stats_MultiTest.formula <- function(formu, dat, ...) {
  response <- dat[[all.vars(formu)[1]]]
  # 确定使用的方法
  if (is.double(response)) {
    typ <- "linear"
  } else if (is.factor(response) & nlevels(response) == 2) {
    typ <- "logistic"
  } else if (is.factor(response) & nlevels(response) > 2 & class(response)[1] == "ordered") {
    typ <- "order"
  } else {
    stop("Not Implement Error.")
  }
  # 进行数据分析
  switch (
    typ,
    linear = .MultiTest_linear(formu, dat, ...),
    logistic = .MultiTest_logistic(formu, dat, ...),
    order = .MultiTest_polr(formu, dat, ...)
  )
}

.MultiTest_linear <- function(formu, dat, ...) {
  lm_fit <- lm(formula = formu, data = dat, ...)
  stats_MultiTest.lm(lm_fit)

}

#' @rdname stats_MultiTest
#' @export
stats_MultiTest.lm <- function(obj) {
  summ_obj <- summary(obj)
  indep_mat <- cbind(coef(summ_obj), confint(obj))
  colnames(indep_mat) <- c(
    "Coefficients",
    "Std Error",
    "Statistic",
    "p value",
    "CI2.5%",
    "CI97.5%"
  )
  indep_df <- as.data.frame(indep_mat)
  indep_df["Independent Variable"] <- rownames(indep_df)
  rownames(indep_df) <- NULL
  indep_df <- indep_df[c(
    "Independent Variable",
    "Coefficients",
    "CI2.5%",
    "CI97.5%",
    "Std Error",
    "Statistic",
    "p value"
  )]

  structure(
    list(
      type = "OLS",
      statistic = "t value",
      ori_obj = obj,
      independent = indep_df,
      dependent = all.vars(obj$terms)[1],
      goodness = c(
        R2 = summ_obj$r.squared,
        `adjusted R2` = summ_obj$r.squared
      )
    ),
    class = "stats_MultiTest"
  )
}

.MultiTest_logistic <- function(formu, dat, ...) {
  logit_fit <- glm(formula = formu, data = dat, family = binomial(), ...)
  stats_MultiTest.glm(logit_fit)
}

#' @rdname stats_MultiTest
#' @export
stats_MultiTest.glm <- function(obj) {
  if (obj$family$link == "logit" & obj$family$family == "binomial") {
    summ_obj <- summary(obj)
    # 计算CIs
    indep_ci <- try(confint(obj), silent = TRUE)
    if (class(indep_ci) == "try-error") {
      warning("使用confint估计CIs失败，使用confint.default代替")
      indep_ci <- confint.default(obj)
    }
    indep_mat <- cbind(coef(summ_obj), indep_ci)
    # 改列名
    colnames(indep_mat) <- c(
      "Coefficients",
      "Std Error",
      "Statistic",
      "p value",
      "CI2.5%",
      "CI97.5%"
    )
    indep_df <- as.data.frame(indep_mat)
    indep_df["Independent Variable"] <- rownames(indep_df)
    rownames(indep_df) <- NULL
    # 加入OR
    indep_df["ORs"] <- exp(indep_df["Coefficients"])
    or_cis <- exp(indep_df[c("CI2.5%", "CI97.5%")])
    names(or_cis) <- c("OR CI2.5%", "OR CI97.5%")
    indep_df <- cbind(indep_df, or_cis)
    # 对列进行排序
    indep_df <- indep_df[c(
      "Independent Variable",
      "ORs",
      "OR CI2.5%",
      "OR CI97.5%",
      "Coefficients",
      "CI2.5%",
      "CI97.5%",
      "Std Error",
      "Statistic",
      "p value"
    )]
    structure(
      list(
        type = "Logistic",
        statistic = "z value",
        ori_obj = obj,
        independent = indep_df,
        dependent = all.vars(obj$terms)[1],
        goodness = c(
          `pseduo R2` = with(summ_obj, 1 - deviance / null.deviance),
          AIC = summ_obj$aic
        )
      ),
      class = "stats_MultiTest"
    )
  } else {
    stop("Not Implement.")
  }
}

.MultiTest_polr <- function(formu, dat, ...) {
  yname <- all.vars(formu)[1]
  yvalues <- dat[[yname]]
  stopifnot(is.ordered(yvalues))
  if (nlevels(yvalues) > 5) {
    warning("当类别数量大于5时，推荐转换成int使用lm！")
  }
  # 一定要加上Hess，不然进行summary时会重新拟合模型，这时会eval(call)来拟合，这时候的数据变量名称可能有问题
  fit <- MASS::polr(formu, data = dat, Hess = TRUE, ...)
  stats_MultiTest.polr(fit)
}

#' @rdname stats_MultiTest
#' @export
stats_MultiTest.polr <- function(obj) {
  summ_obj <- summary(obj)
  coef_obj <- coef(summ_obj)
  # 计算CIs，confint无法计算inception，所以这里直接用标准正态分布算的
  # lamb <- qt(0.975, df = obj$df.residual)
  indep_mat <- cbind(
    coef_obj,
    ci1 = coef_obj[, "Value"] - coef_obj[, "Std. Error"] * 1.96,
    ci2 = coef_obj[, "Value"] + coef_obj[, "Std. Error"] * 1.96
  )
  # 计算p值
  p <- pnorm(abs(indep_mat[, "t value"]), lower.tail = FALSE) * 2
  indep_mat <- cbind(indep_mat, `p value` = p)
  # 改列名
  colnames(indep_mat) <- c(
    "Coefficients",
    "Std Error",
    "Statistic",
    "CI2.5%",
    "CI97.5%",
    "p value"
  )
  indep_df <- as.data.frame(indep_mat)
  indep_df["Independent Variable"] <- rownames(indep_df)
  rownames(indep_df) <- NULL
  # 加入OR
  indep_df["ORs"] <- exp(indep_df["Coefficients"])
  or_cis <- exp(indep_df[c("CI2.5%", "CI97.5%")])
  names(or_cis) <- c("OR CI2.5%", "OR CI97.5%")
  indep_df <- cbind(indep_df, or_cis)
  # 对列进行排序
  indep_df <- indep_df[c(
    "Independent Variable",
    "ORs",
    "OR CI2.5%",
    "OR CI97.5%",
    "Coefficients",
    "CI2.5%",
    "CI97.5%",
    "Std Error",
    "Statistic",
    "p value"
  )]
  structure(
    list(
      type = "Ordinal",
      statistic = "t value",
      ori_obj = obj,
      independent = indep_df,
      dependent = all.vars(obj$terms)[1],
      goodness = c(
        `Residual Deviance` = obj$deviance,
        AIC = obj$deviance + 2 * nrow(indep_df)
      )
    ),
    class = "stats_MultiTest"
  )
}

# .clear_varname <- function(Coefs, df) {
#   # 将Coefs中涉及到的变量名按其中的顺序整理一遍
#   fit_names <- Coefs$predictors
#   for (v in names(df)) {
#     # index <- grep(v, fit_names)  # 用str_detect会出错，比如其不能识别地区南方中的地区
#     # 使用grep对于LDL，VLDL这样的会出错
#     # index <- str_starts(fit_names, v)  # string_r包中对于中文支持不够
#     v_len <- nchar(v)
#     index <- c()
#     for (i in seq_along(fit_names)) {
#       fit_name <- fit_names[i]
#       index <- c(index, substr(fit_name, start = 1, stop = v_len) == v)
#     }
#     fit_names[index] <- v
#   }
#   fit_names <- fit_names[!(fit_names %>% duplicated)]
#   # 为每个分类变量将其分类补全
#   dfs <- list()
#   for (v in fit_names) {
#     if (is.factor(df[[v]]) | is.character(df[[v]])) {
#       n_level <- nlevels(df[[v]])
#       res_dfi <- matrix(nrow = n_level, ncol = ncol(Coefs) + 1) %>% as.data.frame
#       names(res_dfi) <- c("Preds", "Cats", names(Coefs)[-1])
#
#       index <- grep(v, Coefs$predictors)
#
#       res_dfi[1, 1] <- v
#       res_dfi[, 2] <- levels(df[[v]])
#       res_dfi[1, 2] <- paste0(res_dfi[1, 2], "(Control)")
#       for (i in 3:ncol(res_dfi)) {
#         res_dfi[2:n_level, i] <- Coefs[index, i-1]
#       }
#     } else if (is.double(df[[v]]) | v == "(Intercept)") {
#       res_dfi <- matrix(nrow = 1, ncol= ncol(Coefs) + 1) %>% as.data.frame()
#       names(res_dfi) <- c("Preds", "Cats", names(Coefs)[-1])
#       res_dfi[1, 1] <- v
#
#       res_dfi[1, 3:ncol(res_dfi)] <- Coefs[Coefs$predictors == v, -1]
#     } else {
#       stop(sprintf("The type of %s is not matched.", v))
#     }
#     dfs[[v]] <- res_dfi
#   }
#   bind_rows(dfs)
# }
