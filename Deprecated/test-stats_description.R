context("stats_description")

library(dplyr)
mtcar_test <- mtcars %>% select(mpg, cyl, disp, vs, am) %>%
  mutate(cyl = as.factor(cyl), vs = as.factor(vs), am = as.factor(am))
res_iris <- stats_description(iris, Species)
res_mtcar <- stats_description(mtcar_test, am)

test_that("the dimension of result is right", {
  expect_equal(dim(res_iris), c(4, 5))
  expect_equal(dim(res_mtcar), c(7, 5))
})

test_that("selecting the specified variables is available", {
  res <- stats_description(mtcar_test, am, mpg, cyl)
  expect_equal(dim(res), c(4, 5))
})

test_that("inverse selection is available", {
  res <- stats_description(mtcar_test, am, -vs, -disp)
  expect_equal(dim(res), c(4, 5))
})

test_that("the Var and Categories in front", {
  target <- c("Var", "Categories")
  for (i in 1:2) {
    expect_match(names(res_mtcar)[i], target[i])
  }
  expect_match(names(res_iris)[1], "Var")
})

test_that("types parameter is available", {
  res <- stats_description(mtcar_test, am, disp, cyl, vs, .num_desc_type = "auto")
  expect_match(res[["0"]][1], "^\\d+\\.\\d{2}\\+\\-\\d+\\.\\d{2}$")

  res <- stats_description(mtcar_test, am, disp, cyl, vs, .num_desc_type = "mean")
  expect_match(res[["0"]][1], "^\\d+\\.\\d{2}\\+\\-\\d+\\.\\d{2}$")

  res <- stats_description(mtcar_test, am, disp, cyl, vs, .num_desc_type = "median")
  expect_match(res[["0"]][1], "^\\d+\\.\\d{2}\\(\\d+\\.\\d{2}, \\d+\\.\\d{2}\\)$")
})
