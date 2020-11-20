#' Check whether variables can use chisq test.
#'
#' This function use the \code{chisq.test} to calculate the expected frequency, so there often are some warnings
#' about "Chi-squared approximation may be incorrect".
#'
#' @param dat data.frame
#' @param group The group variable with unquoted name
#' @param ... One or more unquoted expressions separated by commas. If none, use all variables except group.
#'
#' @return A list whose class is \code{check_chisq}.
#' \describe{
#'   \item{methods}{
#'   Named character vector to represent the method for every variable. \code{'chisq'} represents the \code{chisq.test},
#'   \code{'fisher'} represents the \code{fisher.test}, \code{NA} means that the variable isn't a factor.
#'   }
#'   \item{expected}{Named list which contains the expected counts of all variables.}
#' }
#'
#' @seealso \code{\link{check_NE}}
#'
#' @export
#'
#' @examples
#' # settle data used to test
#' library(tidyverse)
#' dat <- mtcars %>% mutate(cyl=factor(cyl), vs=factor(vs), am=factor(am), gear=factor(gear), carb=factor(carb))
#' # test
#' dat %>% check_chisq(cyl)
check_chisq <- function(dat, group, ...) {
  preprocess <- select_params(dat, !!enquo(group), ...)
  group <- preprocess[[1]]
  vars <- preprocess[[2]]
  .check_chisq(group, vars)
}

.check_chisq <- function(group, vars) {
  methods <- structure(rep(NA_character_, ncol(vars)), names = names(vars))
  expected <- list()
  for (i in seq_along(vars)) {
    v <- vars[[i]]
    # 这里注意两点：
    #   1. 使用name来进行索引赋值。如果最后一个var是一个numeric，则expected最后一个元素是NULL，R默认将不添加这个
    #     元素，这使得expected的元素数量和ncol(vars)是不一致的。如果不使用name来赋值，则我们就需要在最后将names
    #     添加上，这时候names(expected) <- names(vars)会因为元素数量不一致而出错。
    #   2. 如果全都是NULL（即vars都是numeric），则expected是空列表，此时无法为其赋予names，即便是使用structure也
    #     不行。
    #
    # 但现在使用name来赋值又出现新的问题：所有numeric变量都不会在expected中留下名称，使得其length和logic的length
    #   不一致，这可能会让使用者比较难受。之前的实现是使用index，如果出现间断的index赋值，则中间省略的index会自
    #   动被保存成NULL，所以在结果中会出现NULL的元素值。但现在我们将赋值方式改为name复制后，就没有这个效果了。
    #
    # 所以这里将numeric的变量所代表的expected使用NA（logical）来占位，保证expected的长度和logic相同，并且他们的names
    #   和names(vars)是一致的。
    if (is.factor(v)) {
      res <- .check_chisq_one(group, v)
      methods[names(vars)[i]] <- res[[1]]
      expected[[names(vars)[i]]] <- res[[2]]
    } else {
      expected[[names(vars)[i]]] <- NA
    }
  }
  structure(
    list(methods = methods, expected = expected),
    class = "check_chisq"
  )
}

.check_chisq_one <- function(group, v) {
  tab <- table(v, group)
  chisq.fit <- chisq.test(tab)
  expected <- as.vector(chisq.fit$expected)

  res <- "chisq"
  if (length(expected) > 4) {
    if ((mean(expected < 5 & expected > 1) > 0.2) | (any(expected < 1)) ) {
      res <- "fisher"
    }
  }
  else {
    if (any(expected < 1) | (sum(expected) < 40)) {
      res <- "fisher"
    }
  }
  list(res, chisq.fit$expected)
}
