library(dplyr)

dat <- mtcars %>% mutate(cyl=factor(cyl), vs=factor(vs), am=factor(am), gear=factor(gear), carb=factor(carb))
res1 <- dat %>% check_NE(cyl)
res2 <- dat %>% check_NE(cyl, .N = FALSE)
res3 <- dat %>% check_NE(cyl, .E = FALSE)

test_that("the class is 'check_NE'", {
  expect_is(res1, "check_NE")
  expect_is(res2, "check_NE")
  expect_is(res3, "check_NE")
})

test_that(".N and .E can't are FALSE", {
  expect_error(check_NE(dat, cyl, .N=FALSE, .E=FALSE), ".N | .E is not TRUE")
})

test_that("the number of elements of result is right", {
  expect_equal(length(res1), 8)
  expect_equal(length(res2), 6)
  expect_equal(length(res3), 6)
})

test_that("the structure of result is right", {
  exp_names <- c("methods", "test_method_N", "test_method_E", "test_object_N")
  expect_setequal(names(res1), c(exp_names, "logic_N", "logic_E", "pvalue_N", "pvalue_E"))
  expect_setequal(names(res2), c(exp_names, "logic_E", "pvalue_E"))
  expect_setequal(names(res3), c(exp_names, "logic_N", "pvalue_N"))
  expect_is(res1$methods, "character")
  expect_is(res1$logic_N, "matrix")
  expect_is(res1$logic_E, "logical")
  expect_is(res1$pvalue_N, "matrix")
  expect_is(res1$pvalue_E, "numeric")
})

# test_that("the name of logic and expected are right", {
#   expect_equal(names(res$logic), c("mpg", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
#   expect_equal(names(res$expected), c("mpg", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
# })
#
test_that("the elements of results is right", {
  expect_equal(res1$methods[1], "anova")
  expect_equal(res1$methods[2:6], rep("kruskal", 5))
  expect_equal(res1$methods[7:10], rep(NA_character_, 4))
})
