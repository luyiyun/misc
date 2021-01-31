#' Convert some objects from 'misc' package to a dataframe what can be considered as publish tables
#'
#' @param x desc_baseline, stats_SingleTest or stats_PairwiseTest object.
#' @param digit decimal places, default is 2.
#'
#' @return data.frame
#'
#' @export
#'
#' @examples
#'
publish <- function(x, ...) {
  UseMethod("publish")
}

#' @rdname publish
#' @export
publish.list <- function(x, ...) {
  ns_0 <- paste0("模型", seq_along(x))
  ns_1 <- names(x)
  if (is.null(ns_1)) {
    ns <- ns_0
  } else {
    ns <- ifelse(is.null(ns_1), ns_0, ns_1)
  }

  dfs <- lapply(x, publish)
  # 记录所有的列名的
  all_names <- unique(unlist(lapply(dfs, colnames)))

  Reduce(
    rbind,
    Map(function(x) {x[, setdiff(all_names, names(x))] <- NA;return(x)}, dfs)
  )
}


#' @rdname publish
#' @export
publish.desc_baseline <- function(x, digit = 2) {
  var_names <- names(x$vars)
  stopifnot(length(var_names) == length(x$desc_methods))
  res_list <- list()
  for (i in seq_along(var_names)) {
    df_i <- x$main[[var_names[i]]]
    if (x$desc_methods[i] == "mean") {
      values <- paste0(
        sprintf(paste0("%.", digit, "f"), df_i$mean),
        "±",
        sprintf(paste0("%.", digit, "f"), df_i$std)
      )
      # 如果结果中有关于分类变量的结果，我们需要多加上一列来占位，这一列是
      # 给分类变量结果用的。
      if ("table" %in% x$desc_methods) {
        mat <- matrix(
          c(var_names[i], NA_character_, values), nrow = 1,
          dimnames = list(NULL, c("Variable", "Categories", as.character(df_i$Categories)))
        )
      } else {
        mat <- matrix(
          c(var_names[i], values), nrow = 1,
          dimnames = list(NULL, c("Variable", as.character(df_i$Categories)))
        )
      }
      res_list[[i]] <- as.data.frame(mat, stringsAsFactors = FALSE)
    } else if (x$desc_methods[i] == "median") {
      values <- paste0(
        sprintf(paste0("%.", digit, "f"), df_i$median),
        "(",
        sprintf(paste0("%.", digit, "f"), df_i$quan25),
        ", ",
        sprintf(paste0("%.", digit, "f"), df_i$quan75),
        ")"
      )
      if ("table" %in% x$desc_methods) {
        mat <- matrix(
          c(var_names[i], NA_character_, values), nrow = 1,
          dimnames = list(NULL, c("Variable", "Categories", as.character(df_i$Categories)))
        )
      } else {
        mat <- matrix(
          c(var_names[i], values), nrow = 1,
          dimnames = list(NULL, c("Variable", as.character(df_i$Categories)))
        )
      }
      res_list[[i]] <- as.data.frame(mat, stringsAsFactors = FALSE)
    } else if (x$desc_methods[i] == "table") {
      if (is.null(df_i$prob)) {
        res_i <- df_i$count
      } else {
        res_i <- mapply(
          function(count, prob) sprintf(paste0("%d(%.", digit, "f)"), count, prob * 100),
          df_i$count, df_i$prob, SIMPLIFY = FALSE, USE.NAMES = TRUE
        )
      }
      res_list[[i]] <- cbind(
        data.frame(
          Variable = c(var_names[i], rep(NA_character_, length(res_i[[1]])-1)),
          Categories = rownames(df_i$count),
          stringsAsFactors = FALSE
        ),
        as.data.frame(res_i, stringsAsFactors = FALSE, optional = TRUE)
      )
    } else {
      stop("x$desc_methods must be one of 'mean', 'median' and 'table'.")
    }
  }

  Reduce(rbind, Map(function(x) {
    x[, setdiff(unique(unlist(lapply(res_list, colnames))), names(x))] <- NA;
    return(x)
  },
  res_list))
}



#' @rdname publish
#' @export
publish.stats_SingleTest <- function(x, digit = 2, asterisk = TRUE) {
  desc_exist <- !is.null(x$desc)
  var_names <- names(x$main)
  df_list <- list()
  for (i in seq_along(var_names)) {
    df_i <- x$main[[var_names[i]]]
    if (!desc_exist) {
      df_i[["Variable"]] <- var_names[i]
      df_list[[var_names[i]]] <- as.data.frame(df_i, stringsAsFactors = FALSE)[c("Variable", "statistic", "p_value", "method")]
    } else if (x$desc$desc_methods[i] == "table") {
      nrow_desc <- nrow(x$desc$main[[var_names[i]]]$count)
      df_i <- lapply(df_i, function(y) c(y, rep(NA, nrow_desc - 1)))
      df_list[[var_names[i]]] <- as.data.frame(df_i, stringsAsFactors = FALSE)[c("statistic", "p_value", "method")]
    } else {
      df_list[[var_names[i]]] <- as.data.frame(df_i, stringsAsFactors = FALSE)[c("statistic", "p_value", "method")]
    }
  }
  format_res <- do.call(rbind, df_list)
  format_res <- as.data.frame(
    lapply(format_res, function(x) if (is.character(x)) x else sprintf(paste0("%.", digit, "f"), x)),
    stringsAsFactors = FALSE
  )
  format_res$p_value <- ifelse(
    format_res$p_value == paste0(c("0.", rep("0", digit)), collapse = ""),
    paste0(c("<0.", rep("0", digit-1), "1"), collapse = ""),
    format_res$p_value
  )
  if (desc_exist) {
    format_desc <- publish.desc_baseline(x$desc, digit)
    format_res <- cbind(format_desc, format_res)
  }
  format_res
}


#' @rdname publish
#' @export
publish.stats_PairwiseTest <- function(x, digit = 2) {
  var_names <- names(x$main)
  res <- list()
  for (i in seq_along(var_names)) {
    one_obj <- x$main[[var_names[i]]]$p_value
    cate_col <- rownames(one_obj)
    attr_obj <- attributes(one_obj)
    one_obj <- sprintf(paste0("%.", digit, "f"), one_obj)
    one_obj <- ifelse(
      one_obj == paste0(c("0.", rep("0", digit)), collapse = ""),
      paste0(c("<0.", rep("0", digit-1), "1"), collapse = ""),
      one_obj
    )
    attributes(one_obj) <- attr_obj
    one_df <- as.data.frame(one_obj, stringsAsFactors = FALSE, row.names = NULL)
    names(one_df) <- colnames(one_obj)
    one_df[["method"]] <- c(x$main[[var_names[i]]]$method, rep(NA_character_, nrow(one_df) - 1))
    res[[var_names[i]]] <- cbind(data.frame(
      Variable = c(var_names[i], rep(NA_character_, nrow(one_obj)-1)),
      Categories = cate_col, stringsAsFactors = FALSE), one_df
    )
  }
  res <- do.call(rbind, res)
  rownames(res) <- NULL
  res
}

#' @rdname publish
#' @export
publish.stats_Correlation <- function(x, digit = 2) {
  res <- matrix(NA_character_, ncol = ncol(x), nrow = nrow(x), dimnames = dimnames(x))

  # lower
  lower_ind <- lower.tri(res)
  round_str <- sprintf(paste0("%.", digit, "f"), x[lower_ind])
  round_str <- ifelse(
    round_str == paste0(c("0.", rep("0", digit)), collapse = ""),
    paste0(c("p < 0.", rep("0", digit-1), "1"), collapse = ""),
    paste0("p = ", round_str)
  )
  res[lower_ind] <- round_str
  # upper
  upper_ind <- upper.tri(res)
  round_str <- sprintf(paste0("%.", digit, "f"), x[upper_ind])
  round_str <- ifelse(
    round_str == paste0(c("0.", rep("0", digit)), collapse = ""),
    paste0(c("r < 0.", rep("0", digit-1), "1"), collapse = ""),
    paste0("r = ", round_str)
  )
  res[upper_ind] <- round_str

  return(as.data.frame(res, stringsAsFactors = FALSE, optional = TRUE))
}

#' @rdname publish
#' @export
publish.stats_MultiTest <- function(x, digit = 2) {
  as_charac <- function(xx) {
    if (is.double(xx)) {
      sprintf(paste0("%.", digit, "f"), xx)
    } else {
      xx
    }
  }

  nr <- nrow(x$independent)
  gn <- paste(names(x$goodness), sprintf(paste0("%.", digit, "f"), x$goodness), sep = "=", collapse = ",")
  df <- as.data.frame(
    lapply(x$independent, as_charac),
    stringsAsFactors = FALSE, optional = TRUE
  )
  df[["p value"]] <- ifelse(
    df[["p value"]] == paste0(c("0.", rep("0", digit)), collapse = ""),
    paste0(c("<0.", rep("0", digit-1), "1"), collapse = ""),
    df[["p value"]]
  )
  df <- cbind(
    `Dependent Variable` = c(x$dependent, rep(NA_character_, nr - 2), gn),
    df
  )

  df
}


#' Convert stats_PairwiseTest results as a dataframe for pairwise test plot
#'
#' @param x stats_PairwiseTest object.
#' @param digit decimal places, default is 2.
#'
#' @return list of data.frames
#' @export
#'
#' @importFrom dplyr case_when
#'
#' @examples
#' library(tidyverse)
#' compare_res <- iris %>% stats_PairwiseTest(Species, .num_method="bonferroni_wilcox") %>% format_pairwise_plot(4)
#' iris %>% ggplot(aes(Species, Sepal.Length)) + geom_boxplot() +
#'     ggpubr::stat_pvalue_manual(compare_res$Sepal.Length %>% mutate(p.format = str_c(p.format, p.signif)), label = "p.format")

format_pairwise_plot <- function(x, digit = 2) {
  var_names <- names(x$vars)

  df_list <- list()
  for (i in seq_along(var_names)) {
    obj_i <- x[[var_names[i]]]
    group1 <- c()
    group2 <- c()
    p <- c()
    for (cn in colnames(obj_i$p_value)) {
      for (rn in rownames(obj_i$p_value)) {
        value <- obj_i$p_value[rn, cn]
        if (!is.na(value)) {
          group1 <- c(group1, cn)
          group2 <- c(group2, rn)
          p <- c(p, value)
        }
      }
    }
    df_i <- data.frame(group1=group1, group2=group2, p=p, stringsAsFactors = FALSE)
    df_i$p.format <- sprintf(paste0("%.", digit, "f"), p)
    df_i$p.format <- ifelse(
      df_i$p.format == paste0(c("0.", rep("0", digit)), collapse = ""),
      paste0(c("<0.", rep("0", digit-1), "1"), collapse = ""),
      df_i$p.format
    )
    pos <- max(x$vars[[i]], na.rm = TRUE)
    v_v <- sd(x$vars[[i]], na.rm = TRUE)
    df_i$y.position <- seq(pos, pos + (nrow(df_i) - 1) * v_v, length.out = nrow(df_i))
    df_i$p.signif <- case_when(
      df_i$p <= 0.0001 ~ "****",
      df_i$p <= 0.001 ~ "***",
      df_i$p <= 0.01 ~ "**",
      df_i$p <= 0.05 ~ "*",
      df_i$p > 0.05 ~ "",
      TRUE ~ NA_character_
    )

    df_i$method <- obj_i$method

    df_list[[var_names[i]]] <- df_i
  }

  df_list
}


