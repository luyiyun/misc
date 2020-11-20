context("stats_SingleTest")

res_iris <- stats_SingleTest(iris, Species)
mtcars <- dplyr::mutate(mtcars, cyl = as.factor(cyl), vs = as.factor(vs),
                        am = as.factor(am), gear = as.factor(gear),
                        carb = as.factor(carb))
res_mtcars2 <- stats_SingleTest(mtcars, vs)
res_mtcars3 <- stats_SingleTest(mtcars, vs, .compact = TRUE)
# res_mtcars3 <- stats_SingleTest(mtcars, cyl)

test_that("dim of results is right", {
  expect_equal(dim(res_iris), c(4, 4))
  expect_equal(dim(res_mtcars2), c(20, 4))
  expect_equal(dim(res_mtcars3), c(10, 4))
})
