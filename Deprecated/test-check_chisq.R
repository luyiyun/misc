test_that("results are right", {
  temp_dat <- dplyr::mutate(mtcars,
    cyl = as.factor(cyl), vs = as.factor(vs), am = as.factor(am),
    gear = as.factor(gear), carb = as.factor(carb)
  )
  res <- check_chisq(temp_dat, cyl)
  expect_equal(res[[1]], c(
    mpg = NA, disp = NA, hp = NA, drat = NA, wt = NA, vs = FALSE,
    am = FALSE, gear = FALSE, carb = FALSE)
  )
})
