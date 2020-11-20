context("stats_PairwiseTest")

library(dplyr)

res_iris <- stats_PairwiseTest(iris, Species, .digit = 4)
use_mtcars <- mtcars %>% mutate(
  cyl = as.factor(cyl), vs = as.factor(vs),
  am = as.factor(am), gear = as.factor(gear),
  carb = as.factor(carb)
)
res_mtcars <- use_mtcars %>% stats_PairwiseTest(cyl, .digit = 4)

true_vars_iris <- c()
for (v in names(iris)[-5]) {
  true_vars_iris <- c(true_vars_iris, v, NA_character_)
}
true_vars_mtcars <- c()
for (v in names(use_mtcars)[-2]) {
  true_vars_mtcars <- c(true_vars_mtcars, v, NA_character_)
}

test_that("the dim of results for iris is right", {
  expect_equal(dim(res_iris), c(8, 5))
})

test_that("the dim of results for mtcars is right, also for success in discrete variables",{
  expect_equal(dim(res_mtcars), c(20, 5))
})

test_that("the Vars of results is right", {
  expect_equal(res_iris[[1]], true_vars_iris)
  expect_equal(res_mtcars[[1]], true_vars_mtcars)
})
