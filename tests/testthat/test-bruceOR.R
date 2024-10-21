test_that("test bruceOR works", {
  expect_equal(OR_95CI(0.5, 1, 0.05, 2), "1.65 (1.23, 2.21)")
})
