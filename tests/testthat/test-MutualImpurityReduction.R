test_that("MFI-MIR", {
  skip_on_ci()

  data("SMD_example_data")

  rel <- MFI(
    x = SMD_example_data[, -1],
    y = SMD_example_data[, 1],
    num.trees = 50,
    s = 10,
    seed = 42,
    importance = "impurity_corrected",
    variables = colnames(SMD_example_data)[-1],
    candidates = colnames(SMD_example_data)[-1],
    num.threads = 1,
    t = 5
  )

  mir <- MutualImpurityReduction(rel)

  j.mir <- MutualImpurityReductionVariableSelection(
    mir,
    method = "Janitza",
    p.t = 0.01
  )

  expect_snapshot(j.mir$selected)

  p.mir <- MutualImpurityReductionVariableSelection(
    mir,
    method = "Permutation",
    permutation.num = 50,
    p.t = 0.01
  )

  expect_snapshot(p.mir$selected)
})

test_that("MAA-MIR", {
  skip_on_ci()

  data("SMD_example_data")
  set.seed(42)
  rel <- MeanAdjustedAgreement(
    RandomForestSurrogates(
      x = SMD_example_data[, -1],
      y = SMD_example_data[, 1],
      num.trees = 50,
      num.threads = 1,
      importance = "impurity_corrected",
      seed = 42,
      s = 10
    ),
    variables = c("X7", "X1"),
    candidates = colnames(SMD_example_data)[2:101],
    t = 5, num.threads = 1
  )

  mir <- MutualImpurityReduction(rel)

  # j.mir <- Janitza.MutualImpurityReduction(
  #   mir,
  #   p.t = 0.01
  # )
  #
  # expect_equal(j.mfi$selected$X1[[1]], "cp1_1")

  p.mir <- MutualImpurityReductionVariableSelection(
    mir,
    method = "Permutation",
    permutation.MeanAdjustedAgreement = MeanAdjustedAgreement(
      RandomForestSurrogates(
        x = SMD_example_data[, -1],
        y = SMD_example_data[, 1],
        num.trees = 50,
        importance = "impurity_corrected",
        permutate = TRUE,
        num.threads = 1,
        seed = 42,
        s = 10
      ),
      t = 5, num.threads = 1
    ),
    permutation.num = 50,
    p.t = 0.01
  )

  expect_snapshot(p.mir$selected)
})
