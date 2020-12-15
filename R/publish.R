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
publish.desc_baseline <- function(x, digit = 2) {
  var_names <- names(x$main)
  stopifnot(length(var_names) == length(x$desc_methods))
  res_list <- list()
  for (i in seq_along(var_names)) {
    df_i <- x$main[[var_names[i]]]
    if (x$desc_methods[i] == "mean") {
      values <- sprintf(paste0("%.", digit, "f±%.", digit, "f"), df_i$mean, df_i$std)
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
      values <- sprintf(paste0("%.", digit, "f (%.", digit, "f, %.", digit, "f)"), df_i$median, df_i$quan25, df_i$quan75)
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
      if (is.data.frame(df_i)) {
        cate_col <- rownames(df_i)
        tib <- as.data.frame(lapply(df_i, as.character), stringsAsFactors = FALSE)
        names(tib) <- names(df_i)
      } else {
        cate_col <- rownames(df_i$count)
        df_list <- mapply(function(x1, x2) sprintf(paste0("%d(%.", digit, "f%%)"), x1, x2*100), df_i$count, df_i$prob)
        tib <- as.data.frame(df_list, stringsAsFactors = FALSE)
        names(tib) <- names(df_i$count)
      }
      var_col <- c(var_names[i], rep(NA_character_, nrow(tib)-1))
      res_list[[i]] <- cbind(data.frame(Variable = var_col, Categories = cate_col, stringsAsFactors = FALSE), tib)
    } else {
      stop("x$desc_methods must be one of 'mean', 'median' and 'table'.")
    }
  }
  bind_rows(res_list)
}



#' @rdname publish
#' @export
publish.stats_SingleTest <- function(x, digit = 2) {
  desc_exist <- !is.null(x$desc)
  var_names <- names(x$main)
  df_list <- list()
  for (i in seq_along(var_names)) {
    df_i <- x$main[[var_names[i]]]
    if (!desc_exist) {
      df_i[["Variable"]] <- var_names[i]
      df_list[[var_names[i]]] <- as.data.frame(df_i, stringsAsFactors = FALSE)[c("Variable", "statistic", "p_value", "method")]
    } else if (x$desc$desc_methods[i] == "table") {
      desc_res_i <- x$desc$main[[var_names[i]]]
      if (is.list(desc_res_i)) {
        desc_res_i <- desc_res_i$count
      }
      df_i <- lapply(df_i, function(y) c(y, rep(NA, nrow(desc_res_i) - 1)))
      df_list[[var_names[i]]] <- as.data.frame(df_i, stringsAsFactors = FALSE)[c("statistic", "p_value", "method")]
    } else {
      df_list[[var_names[i]]] <- as.data.frame(df_i, stringsAsFactors = FALSE)[c("statistic", "p_value", "method")]
    }
  }
  format_res <- bind_rows(df_list)
  format_res <- as.data.frame(
    lapply(
      format_res, function(x) if (is.character(x)) x else sprintf(paste0("%.", digit, "f"), x)
    ),
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
    one_df <- as.data.frame(one_obj, stringsAsFactors = FALSE)
    names(one_df) <- colnames(one_obj)
    one_df[["method"]] <- c(x$main[[var_names[i]]]$method, rep(NA_character_, nrow(one_df) - 1))
    res[[var_names[i]]] <- cbind(data.frame(
      Variable = c(var_names[i], rep(NA_character_, nrow(one_obj)-1)),
      Categories = cate_col, stringsAsFactors = FALSE), one_df
    )
  }
  bind_rows(res)
}

#' @rdname publish
#' @export
publish.stats_Correlation <- function(x, digit = 2) {
  res <- matrix(NA_character_, ncol = ncol(x), nrow = nrow(x), dimnames = dimnames(x))

  # lower
  lower_ind <- lower.tri(res)
  res[lower_ind] <- round2char(x[lower_ind], digit, zero_pre = "p < ", nozero_pre = "p = ")
  # upper
  upper_ind <- upper.tri(res)
  res[upper_ind] <- round2char(x[upper_ind], digit, zero_pre = "r < ", nozero_pre = "r = ")

  return(as.data.frame(res))
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


