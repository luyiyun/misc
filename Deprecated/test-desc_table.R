context("desc_table")

factor1 <- factor(c(rep("x1", 10), rep("x2", 10)))
factor2 <- factor(c(rep("y1", 5), rep("y2", 5), rep("y3", 5), rep("y4", 5)))

test_that("the dim of results is right", {
  for (prob in c("none", "row", "col", "all")) {
    res <- desc_table(factor1, factor2, prob = prob)
    expect_equal(dim(res), c(2, 4))
    res <- desc_table(factor1, factor2, margin = "row", prob = prob)
    expect_equal(dim(res), c(2, 5))
    res <- desc_table(factor1, factor2, margin = "col", prob = prob)
    expect_equal(dim(res), c(3, 4))
    res <- desc_table(factor1, factor2, margin = "all", prob = prob)
    expect_equal(dim(res), c(3, 5))
  }
})

test_that("results is true", {
  expect <- data.frame(
    y1 = c(5, 0),
    y2 = c(5, 0),
    y3 = c(0, 5),
    y4 = c(0, 5),
    row.names = c("x1", "x2")
  )
  res <- desc_table(factor1, factor2)
  expect_equal(res, expect)
})

test_that("prob is available", {
  expect <- data.frame(
    y1 = c("5(50.00%)", "0(0.00%)"),
    y2 = c("5(50.00%)", "0(0.00%)"),
    y3 = c("0(0.00%)", "5(50.00%)"),
    y4 = c("0(0.00%)", "5(50.00%)"),
    row.names = c("x1", "x2"),
    stringsAsFactors = FALSE
  )
  res <- desc_table(factor1, factor2, prob = "row")
  expect_equal(res, expect)

  expect <- data.frame(
    y1 = c("5(100.00%)", "0(0.00%)"),
    y2 = c("5(100.00%)", "0(0.00%)"),
    y3 = c("0(0.00%)", "5(100.00%)"),
    y4 = c("0(0.00%)", "5(100.00%)"),
    row.names = c("x1", "x2"),
    stringsAsFactors = FALSE
  )
  res <- desc_table(factor1, factor2, prob = "col")
  expect_equal(res, expect)

  expect <- data.frame(
    y1 = c("5(25.00%)", "0(0.00%)"),
    y2 = c("5(25.00%)", "0(0.00%)"),
    y3 = c("0(0.00%)", "5(25.00%)"),
    y4 = c("0(0.00%)", "5(25.00%)"),
    row.names = c("x1", "x2"),
    stringsAsFactors = FALSE
  )
  res <- desc_table(factor1, factor2, prob = "all")
  expect_equal(res, expect)
})

test_that("digit is right", {
  res <- desc_table(factor1, factor2, prob = "col", digit = 3)
  expect_equal(res[1, 1], "5(100.000%)")

  res <- desc_table(factor1, factor2, prob = "none", digit = 3)
  expect_equal(res[1, 1], 5)

  res2 <- desc_table(factor1, factor2, prob = "none", digit = 4)
  expect_equal(res[1, 1], res2[1, 1])
})

test_that("prob of all is right", {
  res <- desc_table(factor1, factor2, margin = "row", prob = "col")
  expect_equal(res$all, c("10(50.00%)", "10(50.00%)"))

  res <- desc_table(factor1, factor2, margin = "col", prob = "row")
  expect_equal(as.character(res[3, ]), rep("5(25.00%)", 4))
})
