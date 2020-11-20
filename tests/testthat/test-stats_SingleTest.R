library(dplyr)

dat <- mtcars %>% mutate(cyl=factor(cyl), vs=factor(vs), am=factor(am), gear=factor(gear), carb=factor(carb))
res <- dat %>% stats_SingleTest(cyl)

test_that("the class of result is right", {
  expect_is(res, "stats_SingleTest")
  expect_setequal(names(res), c("main", "test_methods", "desc", "vars", "group"))
  expect_is(res$desc, "desc_baseline")
  expect_null(stats_SingleTest(dat, cyl, .desc = FALSE)$desc)
})

test_that("the main part is right", {
  expect_length(res$main, ncol(dat)-1)
})
