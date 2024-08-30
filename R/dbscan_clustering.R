#' Draw the compressed network
#'
#' This function takes an igraph object and compresses the network based on clustering method, in particular
#' this method is DBSCAN clustering
#'
#' @param Complete_network A igraph object with the information
#' @param eps  Is the epsilon value, that idicate the max distance between two nodes for put both in the same cluster (default: 0.8)
#' @param opacity For the network inks and nodes (default : 0.5)
#' @param size Is the size of nodes (default: 25)
#' @param fontSize Is the font size of node labels (default : 0.7)
#' @importFrom igraph layout_with_fr V E layout_with_graphopt contract simplify
#' @importFrom scales alpha
#' @importFrom dbscan dbscan
#' @importFrom grDevices rainbow
#' @return The network in compressed form based on the DBSCAN clustering
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
#' # Use the compression function recalling the dbscan method
#' compression = GraphCompression(complete_net$Complete_network, method = "dbscan", eps = 1.2)
#' # to watch all the results obtained
#' print(compression)

dbscan_clustering <- function(Complete_network, eps = 0.8, opacity = 0.5, size = 25, fontSize = 0.7 ){

  if (inherits(Complete_network, "igraph") == TRUE){

    coordinates <- layout_with_fr(Complete_network)

    dbscan_result <- dbscan(coordinates, eps = eps, minPts = 1)

    num_clusters <- length(unique(dbscan_result$cluster))

    cluster_labels <- dbscan_result$cluster
    cluster_colors <- alpha(rainbow(num_clusters), opacity)
    igraph::V(Complete_network)$color <- cluster_colors[cluster_labels]

    set.seed(123)

    par(mar = c(0, 0, 0, 0))

    layout <- layout_with_graphopt(Complete_network)

    plot(Complete_network, vertex.label.color = "black", layout = layout)

    compressed_graph <- contract(Complete_network, cluster_labels)

    par(mar = c(0, 0, 0, 0))

    layout <- layout_with_graphopt(compressed_graph)

    plot(compressed_graph, vertex.size = size, vertex.label.color = "black", vertex.label.cex = fontSize, layout = layout, vertex.color = cluster_colors)

    compressed_graph_simplified <- simplify(compressed_graph)

    par(mar = c(0, 0, 0, 0))

    layout <- layout_with_graphopt(compressed_graph_simplified)

    plot(compressed_graph_simplified, vertex.size = size, vertex.label.color = "black", vertex.label.cex = fontSize, layout = layout, vertex.color = cluster_colors)

    cluster_composition <- igraph::V(compressed_graph)$name

    names(cluster_composition) <- cluster_colors

    return(list(cluster = cluster_composition, info = dbscan_result, sub_graph = compressed_graph_simplified))

  } else{
    stop("The input network is not an igraph object")
  }
}
