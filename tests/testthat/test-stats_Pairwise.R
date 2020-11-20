library(dplyr)

dat <- mtcars %>% mutate(cyl=factor(cyl), vs=factor(vs), am=factor(am), gear=factor(gear), carb=factor(carb))
st_res <- dat %>% stats_SingleTest(cyl)
res1 <- st_res %>% stats_PairwiseTest()
res2 <- st_res %>% stats_PairwiseTest(.just_for_sign = TRUE)
res3 <- dat %>% stats_PairwiseTest(cyl)
# res4 <- iris %>% stats_SingleTest(Species) %>% stats_PairwiseTest(.just_for_sign = TRUE)
# res5 <- iris %>% stats_SingleTest(Species) %>% stats_PairwiseTest()

test_that("the class of result is right", {
  expect_is(res1, "stats_PairwiseTest")
  expect_is(res2, "stats_PairwiseTest")
  expect_setequal(names(res1), c("main", "SingleTest", "vars", "group"))
  expect_setequal(names(res2), c("main", "SingleTest", "vars", "group"))
  expect_setequal(names(res3), c("main", "vars", "group"))
})

test_that("the length of 'main' is right", {
  expect_equal(length(res1$main), 10)
  expect_equal(length(res2$main), 10)  # 暂时还没有找到三组间没有差异的示例进行测试，但总体上，应该是可行的。
})

sim_dat <- data.frame(
  g = sample(5, 100, replace = TRUE) %>% factor,
  a = rnorm(100),
  b = rexp(100),
  c = sample(4, 100, replace = TRUE) %>% factor
)

sim_res <- sim_dat %>% stats_PairwiseTest(g)
test_that("For group variable whose level is more than 3", {
  expect_is(sim_res, "stats_PairwiseTest")
})
