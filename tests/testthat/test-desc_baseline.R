library(dplyr)

dat <- mtcars %>% mutate(cyl=factor(cyl), vs=factor(vs), am=factor(am), gear=factor(gear), carb=factor(carb))
res <- dat %>% desc_baseline(cyl)

test_that("the class of result is right", {
  expect_is(res, "desc_baseline")
})

test_that("the names of elements of result are right", {
  expect_setequal(names(res), c("main", "desc_methods", "vars", "group"))
  expect_setequal(names(res$main), setdiff(names(dat), "cyl"))
})

test_that("the classes of elements of result are right", {
  expect_equal(unname(sapply(res$main, class)), rep("data.frame", length(res$main)))
  for (s in c("all", "row", "col")) {
    res2 <- dat %>% desc_baseline(cyl, .fct_prob = s)
    expect_equal(unname(sapply(res2$main, class)), c(rep("data.frame", 6), rep("list", 4)))
  }
})

