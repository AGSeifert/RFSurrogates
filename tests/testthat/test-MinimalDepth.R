test_that("MD", {
  skip_on_ci()

  data("SMD_example_data")
  res.md <- MinimalDepth(RandomForestSurrogates(
    x = SMD_example_data[, -1],
    y = SMD_example_data[, 1],
    num.trees = 100,
    num.threads = 1,
    seed = 42,
    s = 0
  ))

  res.md$var

  expect_equal(res.md$selected[[1]], "X2")
})
