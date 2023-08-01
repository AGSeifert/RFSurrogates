test_that("mean.index always returns a numeric vector of length(index.variables)", {
  list.res <- rlist::list.flatten(
    list(
      list(`1` = c(0.0, 0.5, 0.5, 0.5)),
      list(`1` = c(0.0, 0.6, 0.7, 0.8)),
      list(`5` = c(0.4, 0.0, 0.4, 0.4)),
      list(`99` = c(0.1, 0.2, 0.1, 0.2)),
      list(`100` = c(0.9, 0.3, 0.0, 0.9)),
      list(`10000` = c(0.7, 0.8, 0.9, 0.0)),
      list(`5` = c(0.3, 0.0, 0.5, 0.6), `100` = c(0.7, 0.6, 0.0, 0.9))
    )
  )
  index.variables <- c(1, 5, 100, 10000)

  # 1 would not go out of bounds, thus should always pass
  testthat::expect_length(mean.index(1, list.res, index.variables), length(index.variables))
  testthat::expect_length(mean.index(2, list.res, index.variables), length(index.variables))
  testthat::expect_length(mean.index(3, list.res, index.variables), length(index.variables))
  testthat::expect_length(mean.index(4, list.res, index.variables), length(index.variables))
  # 5 is out of bounds of index.variables, returning only NA of correct length
  testthat::expect_length(mean.index(5, list.res, index.variables), length(index.variables))
})

test_that("mean.index returns NA vector when i is out of bounds of index.variables", {
  list.res <- list()
  index.variables <- 1:4
  testthat::expect_equal(mean.index(5, list.res, index.variables), rep(NA, length(index.variables)))
})
