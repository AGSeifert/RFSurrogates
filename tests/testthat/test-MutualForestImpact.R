test_that("MFI", {
  skip_on_ci()

  data("SMD_example_data")

  mfi <- MFI(
    x = SMD_example_data[, -1],
    y = SMD_example_data[, 1],
    num.trees = 50,
    s = 10,
    seed = 42,
    variables = c("X7", "X1"),
    candidates = colnames(SMD_example_data)[2:101],
    num.threads = 1,
    t = 5
  )

  j.mfi <- MutualForestImpactVariableSelection(
    mfi,
    variables = c("X7", "X1"),
    candidates = colnames(SMD_example_data)[2:101],
    p.t = 0.01,
    method = "Janitza"
  )

  expect_snapshot(j.mfi$selected)

  p.mfi <- MutualForestImpactVariableSelection(
    mfi,
    variables = c("X7", "X1"),
    candidates = colnames(SMD_example_data)[2:101],
    p.t = 0.01,
    method = "Permutation"
  )

  expect_snapshot(p.mfi$selected)
})
