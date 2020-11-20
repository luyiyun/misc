context("desc_numeric")

# 先将结果准备好
results <- list()
for (t in c("auto", "mean", "median")) {
  results[[t]] <- desc_numeric(iris$Sepal.Length, iris$Species, type = t)
}

test_that("the dimensions of result are right", {
  for (res in results) {
    expect_equal(dim(res), c(1, 4))
  }
})

test_that("the colnames of result are right", {
  true_names <- c(levels(iris$Species), "all")
  for (res in results) {
    expect_true(setequal(colnames(res), true_names))
  }
})

test_that("the entries of result are character", {
  for (res in results) {
    ds <- dim(res)
    for (i in seq_len(ds[1])) {
      for (j in seq_len(ds[2])) {
        expect_type(res[[i, j]], "character")
      }
    }
  }
})

test_that("the entries of result match right format", {
  for (t in c("mean", "median")) {
    for (digit in c(2, 3)) {
      res <- desc_numeric(iris$Sepal.Length, iris$Species,
                          type = t, digit = digit)
      ds <- dim(res)
      if (t == "mean") {
        target <- c("^\\d*\\.\\d{", "}\\+\\-\\d*\\.\\d{", "}$")
        target <- paste0(target, collapse = as.character(digit))
      } else {
        one_num <- paste0("\\d*\\.\\d{", digit, "}")
        target <- paste0("^", one_num, "\\(", one_num, ", ", one_num, "\\)$")
      }
      for (i in ds[1]) {
        for (j in ds[2]) {
          expect_match(res[[j]][i], target)
        }
      }
    }
  }
})
