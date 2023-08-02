test_that("MAA", {
  skip_on_ci()

  data("SMD_example_data")
  rel <- MeanAdjustedAgreement(
    RandomForestSurrogates(
      x = SMD_example_data[, -1],
      y = SMD_example_data[, 1],
      num.trees = 100,
      num.threads = 1,
      seed = 42,
      s = 10
    ),
    variables = c("X7", "X1"),
    candidates = colnames(SMD_example_data)[2:101],
    t = 5, num.threads = 1
  )

  expect_equal(rel$var$X1[[1]], "cp1_1")
})
