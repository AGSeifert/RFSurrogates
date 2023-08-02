test_that("SMD", {
  skip_on_ci()

  data("SMD_example_data")

  res.smd <- SurrogateMinimalDepth(RandomForestSurrogates(
    x = SMD_example_data[, -1],
    y = SMD_example_data[, 1],
    num.trees = 100,
    num.threads = 1,
    seed = 42,
    s = 10
  ))

  expect_equal(res.smd$selected[[1]], "X1")
})
