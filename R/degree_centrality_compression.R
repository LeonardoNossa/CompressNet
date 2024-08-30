#' Draw the compressed network
#'
#' This function takes an igraph object and compresses the network based on the degree of the nodes
#'
#' @param Complete_network A igraph object with the information
#' @param links Is the data frame with the nodes interaction information
#' @param top A percentage of the top nodes (default: 0.1) --> so take the top 90% nodes
#' @param size Is the size of nodes (default: 25)
#' @importFrom igraph V E layout_with_graphopt degree induced_subgraph
#' @importFrom kableExtra kbl kable_styling column_spec scroll_box
#' @importFrom stats quantile
#' @importFrom graphics par
#' @return The network in compressed form based on degree centrality
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
#' # Use the compression function recalling the degree method
#' compression = GraphCompression(complete_net$Complete_network,complete_net$edges_data, method = "degree")
#' # to watch all the results obtained
#' print(compression)
degree_centrality_compression <- function(Complete_network, links, top = 0.1, size = 25){

  # check if the input network is an igraph object and check if the relative information correspond
  # to the input network
  if (inherits(Complete_network, "igraph") == TRUE){
    if (all(unique(as.data.frame(links)[,1]) %in% V(Complete_network)$name) && all(unique(as.data.frame(links)[,2]) %in% V(Complete_network)$name)){

      set.seed(123)  # set a seed to have always the same network (constrains nodes to certain positions)

      degree_c <- degree(Complete_network)       # calculus of the nodes degree
      threshold <- quantile(degree_c, top)       # establish a threshold to filter the nodes

      # creation of a table for a good visualization of values, if they pass the condition establish by threshold are green if not
      # are red, so who is green is in the compressed graph
      table_degree <- kbl(degree_c, align = "c") %>%
        kable_styling() %>%
        column_spec(2, color = "black", background = ifelse(degree_c > threshold , "#90ee90","#ffbfaa"))

      names(threshold) <- "Threshold"
      filters_data <- data.frame(Threshold = threshold, Top = 1 - top)    # set the filtering (initially default, it also can be changed)
      rownames(filters_data) <- "Filters"

      # check part to verify the compression (if is good or not)
      if(all(degree_c > threshold )) {
        warning("No compression, beacuse all nodes have degree > threshold")
        return(list(filters_data = filters_data, values = table_degree))
      } else if(all(degree_c < threshold) | all(degree_c == threshold)){
        warning("Compression too strong, all degree values are < or = threshold")
        return(list(filters_data = filters_data, values = table_degree))
      } else{
        # creation of the igraph object in compressed form -- vids: are the nodes that remain the the network graph
        # cause respect the condition
        compressed_nodes <- names(which(degree_c > threshold))
        compressed_graph <- induced_subgraph(Complete_network, vids = compressed_nodes)

        edges_data <- links[((as.data.frame(links)[,1] %in% compressed_nodes & as.data.frame(links)[,2] %in% compressed_nodes)),]
        edges <- kbl(edges_data, align = "c") %>%   # creation of a the table with the information of the ntwoek compressed
          kable_styling() %>%
          scroll_box(height = "300px")
      }

      par(mar = c(0, 0, 0, 0))

      # set the layout of the network graph
      layout <- layout_with_graphopt(compressed_graph)

      plot(compressed_graph, vertex.size = size,vertex.label.color = "black", layout = layout)

      return(list(filters_data = filters_data, values = table_degree, sub_graph = compressed_graph, edges = edges, edges_data = edges_data))
    } else {
      stop("The input information don't correspond to the input network")
    }

  } else {
    stop("The input network is not an igraph object")
  }
}

