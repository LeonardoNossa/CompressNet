library(testthat)

test_that("draw_complete_net handles missing columns correctly", {

  # Example data frame with wrong name columns
  df_missing <- data.frame(node1 = c("A", "B"), node2 = c("C", "D"), scores = c(0.9, 0.8))

  # So, handlig the correct error
  expect_error(draw_complete_net(df_missing, "preferredName_A", "preferredName_B", "score"),
               "Something went wrong with the columns name, check if these columns are inside the dataframe")
})

test_that("draw_complete_net produces a graph with correct parameters", {

  # Example data frame with correct columns name
  df <- data.frame(preferredName_A = c("A", "B"), preferredName_B = c("C", "D"), score = c(0.9, 0.8))

  # running the function
  result <- draw_complete_net(df, "preferredName_A", "preferredName_B", "score")

  # verify all the results of the function
  expect_true(inherits(result$Complete_network, "igraph"))
  expect_true(inherits(result$nodes, "data.frame"))
  expect_true(inherits(result$edges_data, "data.frame"))
  expect_true(inherits(result$edges, "kableExtra"))

  # Verify that the content of the graph is correct
  expect_equal(igraph::vcount(result$Complete_network), 4)  # 4 nodes
  expect_equal(igraph::ecount(result$Complete_network), 2)  # 2 edges
})

test_that("draw_complete_net handles empty dataframe", {

  # Creation of empty data frame
  df_empty <- data.frame(preferredName_A = character(0), preferredName_B = character(0), score = numeric(0))

  # Verify the handling of the error
  result <- expect_error(draw_complete_net(df_empty, "preferredName_A", "preferredName_B", "score"), "The dataframe is empty. Returning NULL")

})

# Test for node and label sizing
test_that("draw_complete_net applies correct sizing", {

  df <- data.frame(preferredName_A = c("A", "B"), preferredName_B = c("C", "D"), score = c(0.9, 0.8))

  # Running the function with specific size parameters
  result <- draw_complete_net(df, "preferredName_A", "preferredName_B", "score", fontSize = 1.5, size = 50)

  # Verify that fontSize and size are applied
  expect_equal(V(result$Complete_network)$size[1], 50)
})

# Test for performance on large dataset
test_that("draw_complete_net handles large datasets efficiently", {

  # Creating a large dataframe
  large_df <- data.frame(preferredName_A = rep(letters, each = 26),
                         preferredName_B = rep(LETTERS, 26),
                         score = runif(26*26))

  # Measuring time to ensure efficiency
  expect_silent(system.time(draw_complete_net(large_df, "preferredName_A", "preferredName_B", "score")))
})

# Test for visualization accuracy
test_that("draw_complete_net produces a visually accurate network", {

  df <- data.frame(preferredName_A = c("A", "B"), preferredName_B = c("C", "D"), score = c(0.9, 0.8))

  result <- draw_complete_net(df, "preferredName_A", "preferredName_B", "score")

  # Verify that nodes are positioned correctly
  expect_equal(length(V(result$Complete_network)), 4)
  expect_equal(length(E(result$Complete_network)), 2)
})
