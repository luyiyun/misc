library(dplyr)

dat <- mtcars %>% mutate(cyl=factor(cyl), vs=factor(vs), am=factor(am), gear=factor(gear), carb=factor(carb))
res <- dat %>% check_chisq(cyl)
dat2 <- dat %>% select(mpg, cyl, disp, hp, vs, am, gear, carb, drat, wt, qsec)
res3 <- check_chisq(dat2, cyl)

test_that("the class is 'check_chisq'", {
  expect_is(res, "check_chisq")
})

test_that("the structure of result is right", {
  expect_equal(length(res), 2)
  expect_equal(names(res), c("methods", "expected"))
  expect_is(res$methods, "character")
  expect_is(res$expected, "list")
})

test_that("the name of logic and expected are right", {
  expect_equal(names(res$methods), c("mpg", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
  expect_equal(names(res$expected), c("mpg", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
})

test_that("the elements of results is right", {
  for (i in 1:6) {
    expect_true(is.character(res$methods[i]))
    expect_true(is.na(res$methods[i]))
    expect_true(is.na(res$expected[[i]]))
  }
  for (i in 7:10) {
    expect_true(res$methods[i] == "fisher")
    expect_is(res$expected[[i]], "matrix")
    expect_false(is.null(dimnames(res$expected[[i]])))
  }
})

test_that("It is available for dataframe with all numeric variables", {
  res2 <- check_chisq(iris, Species)
  expect_length(res2$expected, 4)
})

test_that("It is available for dataframe whose last variable is numeric", {
  expect_length(res3$expected, 10)
})
