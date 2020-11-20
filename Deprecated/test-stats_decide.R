context("stats_decide")

mtcars2 <- mtcars
for (i in c(2, 8:11)) {
  mtcars2[[i]] <- as.factor(mtcars[[2]])
}
res_iris <- stats_decide(iris, Species)
res_mtcars_cyl <- stats_decide(mtcars2, cyl)
res_mtcars_vs <- stats_decide(mtcars2, vs)

# true_iris <- matrix(
#   c(rep(na_lgl, 4), c(FALSE, TRUE, FALSE, FALSE)),
#   nrow = 4, ncol = 2,
#   dimnames = list(
#     names(iris), c("isChisq", "VarHomo")
#   )
# )

test_that("results is matrix", {
  expect_true(class(res_iris) == "matrix")
})

test_that("dimnames of matrix is available", {
  col_names <- colnames(res_iris)
  row_names <- rownames(res_iris)
  expect_rownames <- setdiff(names(iris), "Species")
  expect_colnames <- c("isChisq", "VarHomo")
  expect_true(setequal(col_names, expect_colnames))
  expect_true(setequal(row_names, expect_rownames))
})

test_that("entries of matrix are logical", {
  for (i in res_iris) {
    expect_type(i, "logical")
  }
})

test_that("one row contains that NA and logical", {
  for (i in seq_len(nrow(res_iris))) {
    one_row <- res_iris[i, ]
    expect_true(rlang::na_lgl %in% one_row)
    expect_equal(sum(is.na(one_row)), 1)
  }
})


test_that(
  "the result is right", {
    expect_false(res_iris[1, 2])
    expect_true(res_iris[2, 2])
  }
)
