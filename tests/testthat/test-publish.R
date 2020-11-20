library(dplyr)
dat <- mtcars %>% mutate(cyl=factor(cyl), vs=factor(vs), am=factor(am), gear=factor(gear), carb=factor(carb))
res1 <- dat %>% desc_baseline(cyl)
res2 <- dat %>% stats_SingleTest(cyl)
res2_2 <- dat %>% stats_SingleTest(cyl, .desc = FALSE)
res3 <- dat %>% stats_PairwiseTest(cyl)

fres1 <- publish(res1)
fres2 <- publish(res2)
fres2_2 <- publish(res2_2)
fres3 <- publish(res3)

test_that("the results of format are right", {
  expect_is(fres1, "data.frame")
  expect_is(fres2, "data.frame")
  expect_is(fres2_2, "data.frame")
  expect_is(fres3, "data.frame")

  expect_equal(dim(fres1), c(19, 6))
  expect_equal(dim(fres2), c(23, 9))
  expect_equal(dim(fres2_2), c(10, 4))
  expect_equal(dim(fres3), c(20, 5))
})

sim_dat <- data.frame(
  g = sample(5, 100, replace = TRUE) %>% factor,
  a = rnorm(100),
  b = rexp(100),
  c = sample(4, 100, replace = TRUE) %>% factor
)
sim_res <- sim_dat %>% stats_PairwiseTest(g)
pub_res <- publish(sim_res)
test_that("publish.stats_PairwiseTest for group variable whose level is more than 3", {
  expect_is(pub_res, "data.frame")
  expect_equal(dim(pub_res), c(12, 7))
})

