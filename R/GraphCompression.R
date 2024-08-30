#' Wrapper function for compression
#'
#' This function takes an igraph object and the related information
#' @param Network the igraph object
#' @param method the type of compression
#' @param ... other args
#' @param links the information related to the network (necessary only for some functions)
#' @return The network in compressed form with a specific method
#' @export
#' @examples
#'
#' # Example usage:
#' # Create an example dataset
#' data_example <- data.frame(
#'   nodeA = c("A", "B", "C", "D", "E", "F"),
#'   nodeB = c("B", "C", "A","A", "B", "E"),
#'   score = c(0.8, 0.9, 0.85, 0.6, 0.5, 0.75)
#' )
#' # Draw the complete network
#' complete_net = draw_complete_net(data_example, nodeA = "nodeA", nodeB = "nodeB", score = "score")
#'
#' # Use the function recalling a compression method.
#' # REMEMBER: This is a wrapper function, that can recall all the compression functions as methods
#' # in this first example using the betweenness method
#' compression = GraphCompression(complete_net$Complete_network,complete_net$edges_data, method = "betweenness")
#' # to watch all the results obtained
#' print(compression)
#' # in this second example using the kmeans method
#' compression = GraphCompression(complete_net$Complete_network, method = "kmeans")
GraphCompression <- function(Network, links = NULL, method = NULL, ...) {

  if (is.null(method)) {
    stop("You must specify a compression method")
  } else if (nchar(method) == 0){
    stop("You must specify a compression method")
  }

  method = tryCatch(
    match.arg(method, c("betweenness", "closeness", "degree", "eigen", "pagerank", "kmeans", "dbscan")),
    error = function(e) {
      stop("Invalid 'method' specified. It should be one of 'betweenness', 'closeness', 'degree', 'eigen', 'pagerank', 'kmeans', 'dbscan'.")
    }
  )

  if (method %in% c("betweenness", "closeness", "degree", "eigen", "pagerank")) {
    if (is.null(links)) {
      stop("Argument missing 'links'. The information related to the network are not specified")
    }
    switch(method,
           betweenness = return(do.call(betweenness_centrality_compression, c(list(Complete_network = Network, links = links), list(...)))),
           closeness = return(do.call(closeness_centrality_compression, c(list(Complete_network = Network, links = links), list(...)))),
           degree = return(do.call(degree_centrality_compression, c(list(Complete_network = Network, links = links), list(...)))),
           eigen = return(do.call(eigenvector_centrality_compression, c(list(Complete_network = Network, links = links), list(...)))),
           pagerank = return(do.call(pageRank_centrality_compression, c(list(Complete_network = Network, links = links), list(...))))
    )
  } else if (method == "kmeans") {
    return(do.call(kmeans_clustering, c(list(Complete_network = Network), list(...))))
  } else if (method == "dbscan") {
    return(do.call(dbscan_clustering, c(list(Complete_network = Network), list(...))))
  }
}
