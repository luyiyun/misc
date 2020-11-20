context("stats_MultiTest")

res <- stats_MultiTest(mpg~., mtcars)
dat1 <- data.frame(
  LDL = rnorm(100),
  VLDL = rnorm(100),
  性别 = rep(c("A", "B"), each = 50) %>% factor,
  y = rnorm(100)
)


test_that("partical repicated varible name", {
  res <- stats_MultiTest(y~VLDL, dat1)
  expect_equal(dim(res), c(2, 7))
})

test_that("support chinese", {
  res <- stats_MultiTest(y~., dat1)
  expect_equal(dim(res), c(5, 7))
})
# res_mtcars3 <- stats_SingleTest(mtcars, cyl)

# test_that("dim of results is right", {
#   expect_equal(dim(res_iris), c(4, 4))
#   expect_equal(dim(res_mtcars2), c(20, 4))
#   expect_equal(dim(res_mtcars3), c(10, 4))
# })
#
#
# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
