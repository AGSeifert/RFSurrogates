test_that("RFS", {
  skip_on_ci()

  data("SMD_example_data")

  rfs <- RandomForestSurrogates(
    x = SMD_example_data[, -1],
    y = SMD_example_data[, 1],
    num.trees = 50,
    num.threads = 1,
    seed = 42,
    s = 3
  )

  expect(TRUE, "never")
})
