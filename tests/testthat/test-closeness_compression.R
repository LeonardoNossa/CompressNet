library(testthat)
library(igraph)

test_that("GraphCompression handles missing or invalid method", {

  # Creation of example graph
  nodes <- data.frame(name = c("A", "B", "C", "D"))
  links <- data.frame(nodeA = c("A", "A", "B", "C"), nodeB = c("B", "C", "D", "D"), score = c(0.9, 0.8, 0.7, 0.6))
  Network <- graph_from_data_frame(links, directed = FALSE, vertices = nodes)

  # Verify error handling for unspecified method
  expect_error(GraphCompression(Network, links, method = NULL), "You must specify a compression method")

  # Verify error handling for empty method
  expect_error(GraphCompression(Network, links, method = ""), "You must specify a compression method")

  # Verify error handling for invalid method
  expect_error(GraphCompression(Network, links, method = "invalid_method"),
               "Invalid 'method' specified. It should be one of 'betweenness', 'closeness', 'degree', 'eigen', 'pagerank', 'kmeans', 'dbscan'.")
})


test_that("GraphCompression correctly compresses network using closeness method", {

  # Creation of example graph
  nodes <- data.frame(name = c("A", "B", "C", "D"))
  links <- data.frame(nodeA = c("A", "A", "B", "C"), nodeB = c("B", "C", "D", "D"), score = c(0.9, 0.8, 0.7, 0.6))
  Network <- graph_from_data_frame(links, directed = FALSE, vertices = nodes)

  expect_warning(
    result <- GraphCompression(Network, links, method = "closeness", top = 0.5),
    "Compression too strong, all closeness values are < or = threshold"
  )

  expect_true(inherits(result$filters_data, "data.frame"))
  expect_true(inherits(result$values, "kableExtra"))

})

test_that("GraphCompression handles different values for 'top' and 'size'", {

  # Creation of example graph
  nodes <- data.frame(name = c("A", "B", "C", "D"))
  links <- data.frame(nodeA = c("A", "A", "B", "C"), nodeB = c("B", "C", "D", "D"), score = c(0.9, 0.8, 0.7, 0.6))
  Network <- graph_from_data_frame(links, directed = FALSE, vertices = nodes)

  # Test with top = 0.2
  expect_warning(
    result <- GraphCompression(Network, links, method = "closeness", top = 0.2),
    "Compression too strong, all closeness values are < or = threshold")

  expect_true(inherits(result$filters_data, "data.frame"))
  expect_true(inherits(result$values, "kableExtra"))


  # Test with different size
  expect_warning(
    result <- GraphCompression(Network, links, method = "closeness", size = 50),
    "Compression too strong, all closeness values are < or = threshold")

  expect_true(inherits(result$filters_data, "data.frame"))
  expect_true(inherits(result$values, "kableExtra"))

})

test_that("GraphCompression handles non-igraph Complete_network", {

  # Creation of a non-igraph object
  non_graph_object <- list()
  links <- data.frame(nodeA = c("A", "A", "B", "C"), nodeB = c("B", "C", "D", "D"), score = c(0.9, 0.8, 0.7, 0.6))

  # Verify that non-igraph input triggers an error
  expect_error(GraphCompression(non_graph_object, links, method = "closeness"),
               "The input network is not an igraph object")
})
