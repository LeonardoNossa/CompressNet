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


test_that("GraphCompression correctly compresses network using eigen method", {

  # Creation of example graph
  nodes <- data.frame(name = c("A", "B", "C", "D"))
  links <- data.frame(nodeA = c("A", "A", "B", "C"), nodeB = c("B", "C", "D", "D"), score = c(0.9, 0.8, 0.7, 0.6))
  Network <- graph_from_data_frame(links, directed = FALSE, vertices = nodes)

  result <- GraphCompression(Network, links, method = "eigen", top = 0.5)

  expect_true(inherits(result$edges_data, "data.frame"))
  expect_true(inherits(result$filters_data, "data.frame"))
  expect_true(inherits(result$values, "kableExtra"))
  expect_true(inherits(result$edges, "kableExtra"))
  expect_true(inherits(result$sub_graph, "igraph"))


})

test_that("GraphCompression handles different values for 'top' and 'size'", {

  # Creation of example graph
  nodes <- data.frame(name = c("A", "B", "C", "D"))
  links <- data.frame(nodeA = c("A", "A", "B", "C"), nodeB = c("B", "C", "D", "D"), score = c(0.9, 0.8, 0.7, 0.6))
  Network <- graph_from_data_frame(links, directed = FALSE, vertices = nodes)

  # Test with top = 0.2
  result <- GraphCompression(Network, links, method = "eigen", top = 0.2)

  expect_true(inherits(result$edges_data, "data.frame"))
  expect_true(inherits(result$filters_data, "data.frame"))
  expect_true(inherits(result$values, "kableExtra"))
  expect_true(inherits(result$edges, "kableExtra"))
  expect_true(inherits(result$sub_graph, "igraph"))


  # Test with different size
  result <- GraphCompression(Network, links, method = "eigen", size = 50)

  expect_true(inherits(result$edges_data, "data.frame"))
  expect_true(inherits(result$filters_data, "data.frame"))
  expect_true(inherits(result$values, "kableExtra"))
  expect_true(inherits(result$edges, "kableExtra"))
  expect_true(inherits(result$sub_graph, "igraph"))

})

test_that("GraphCompression handles non-igraph Complete_network", {

  # Creation of a non-igraph object
  non_graph_object <- list()
  links <- data.frame(nodeA = c("A", "A", "B", "C"), nodeB = c("B", "C", "D", "D"), score = c(0.9, 0.8, 0.7, 0.6))

  # Verify that non-igraph input triggers an error
  expect_error(GraphCompression(non_graph_object, links, method = "eigen"),
               "The input network is not an igraph object")
})
