library(testthat)

test_that("Netty function returns a data frame", {
  result <- Netty()

  # Test that the result is a data frame
  expect_true(is.data.frame(result))

  # Test that the data frame has the expected columns
  expected_columns <- c("preferredName_A", "preferredName_B")
  expect_true(all(expected_columns %in% colnames(result)))

  # Test that the data frame is not empty
  expect_true(nrow(result) > 0)

  # Test some expected values if known
  expect_true("GLI1" %in% result$preferredName_A)
  expect_true("DISP1" %in% result$preferredName_B)
})

