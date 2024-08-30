library(testthat)

# Base case test with TP53
test_that("get_partners returns a data frame with correct columns and data for TP53", {
  result <- get_partners("TP53")
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true(all(c("preferredName_A", "preferredName_B", "score") %in% colnames(result)))
  expect_true(all(result$score >= 0 & result$score <= 1))
})

# Test with an empty protein name
test_that("get_partners raises an error when protein name is empty", {
  expect_error(get_partners(""), "Protein name must not be empty")
})

# Test with a different species ID
test_that("get_partners returns data for a different species ID", {
  result <- get_partners("TP53", species = 10090)  # Using the species ID for mouse
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true(all(c("preferredName_A", "preferredName_B", "score") %in% colnames(result)))
})

# Test with an invalid protein name
test_that("get_partners raises an error when protein name is invalid", {
  expect_error(get_partners("INVALID_PROTEIN_NAME"))
})

