#' Draw the compressed network
#'
#' This function takes an igraph object and compresses the network based on clustering method, in particular
#' this method is K-means clustering
#'
#' @param Complete_network A igraph object with the information
#' @param num_clusters  Is the number of the clusters, so the nodes in the compressed graph (default: 3)
#' @param opacity For the network inks and nodes (default : 0.5)
#' @param size Is the size of nodes (default: 25)
#' @param fontSize Is the font size of node labels (default : 0.7)
#' @importFrom igraph as_adjacency_matrix V E layout_with_graphopt contract simplify
#' @importFrom scales alpha
#' @importFrom methods as
#' @importFrom grDevices rainbow
#' @importFrom stats kmeans
#' @return The network in compressed form based on the K-means clustering
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
#' # Use the compression function recalling the kmeans method
#' compression = GraphCompression(complete_net$Complete_network, method = "kmeans")
#' # to watch all the results obtained
#' print(compression)
kmeans_clustering <- function(Complete_network, num_clusters = 3, size = 25, fontSize = 0.7, opacity = 0.5){

  # check if the input object is an igraph one
  if (inherits(Complete_network, "igraph") == TRUE){

    # creation of the adjacency matrix of the igraph object, it is a square matrix that define the pairs of
    # vertices are adjacent or not
    adjacency_matrix <- as_adjacency_matrix(Complete_network)

    # calculus of the K-means of all nodes in the network
    kmeans_result <- kmeans(adjacency_matrix, centers = num_clusters)

    # defining the clusters and the colors related to them
    cluster_labels <- kmeans_result$cluster
    cluster_colors <- alpha(rainbow(num_clusters), opacity)
    igraph::V(Complete_network)$color <- cluster_colors[cluster_labels]

    set.seed(123) # set a seed to have always the same network (costrains nodes to certain positions)

    par(mar = c(0, 0, 0, 0))  # set the margins around the graph

    # set the layout of the network graph
    layout <- layout_with_graphopt(Complete_network)

    plot(Complete_network, vertex.label.color = "black", layout = layout)

    # compression of the network
    compressed_graph <- contract(Complete_network, cluster_labels)

    par(mar = c(0, 0, 0, 0)) # set the margins around the graph

    # set the layout of the network graph
    layout <- layout_with_graphopt(compressed_graph)

    plot(compressed_graph, vertex.size = size, vertex.label.color = "black", vertex.label.cex = fontSize, layout = layout, vertex.color = cluster_colors)

    # delete multiple edges and loop edges from the graph
    compressed_graph_simplified <- simplify(compressed_graph)

    par(mar = c(0, 0, 0, 0)) # set the margins around the graph

    # using in par() all '0' I'm defining the max space for the graph

    # set the layout of the network graph
    layout <- layout_with_graphopt(compressed_graph_simplified)

    plot(compressed_graph_simplified, vertex.size = size, vertex.label.color = "black", vertex.label.cex = fontSize, layout = layout, vertex.color = cluster_colors)

    # here there is the creation of the dataframe 'info'
    cluster_labels_matrix <- as(cluster_labels, "matrix")    # need this function to transform colors label in a matrix for
    cluster_labels2 <- as.data.frame(cluster_labels_matrix)  # transform it in a dataframe
    cluster_labels2$colors <- V(Complete_network)$color

    return(list(relationship = adjacency_matrix, info = cluster_labels2, cluster = igraph::V(compressed_graph)$name, sub_graph = compressed_graph_simplified))

  } else{
    stop("The input network is not an igraph object")
  }
}


