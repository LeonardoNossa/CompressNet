#' Draw the compressed network
#'
#' This function takes an igraph object and compresses the network based on the pageRank centrality
#'
#' @param Complete_network A igraph object with the information
#' @param links Is the data frame with the nodes interaction information
#' @param top A percentage of the top nodes (default: 0.1) --> so take the top 90% nodes
#' @param size Is the size of nodes (default: 25)
#' @importFrom igraph V E layout_with_graphopt page_rank induced_subgraph
#' @importFrom kableExtra kbl kable_styling column_spec scroll_box
#' @importFrom stats quantile
#' @importFrom graphics par
#' @return The network in compressed form based on pageRank centrality
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
#' # Use the compression function recalling the pagerank method
#' compression = GraphCompression(complete_net$Complete_network,complete_net$edges_data, method = "pagerank")
#' # to watch all the results obtained
#' print(compression)
pageRank_centrality_compression <- function(Complete_network, links, top = 0.1, size = 25){

  # check if the input object is an igraph one and
  # check if the information are related to the input network
  if (inherits(Complete_network, "igraph") == TRUE){
    if (all(unique(as.data.frame(links)[,1]) %in% V(Complete_network)$name) && all(unique(as.data.frame(links)[,2]) %in% V(Complete_network)$name)){
      set.seed(123)  # set a seed to have always the same network (constrains nodes to certain positions)

      # calculus of the page rank
      pageRank_c <- page_rank(Complete_network)

      pageRank_values <- pageRank_c$vector             # the previously function returns different components/values,
      # need only $vector, are the values of page rank
      threshold <- quantile(pageRank_values, top)      # establish a threshold to filter the nodes

      # creation of a table for a good visualization of values, if they pass the condition establish by threshold are green if not
      # are red, so who is green is in the compressed graph
      table_pagerankvalues <- kbl(pageRank_values, align = "c") %>%
        kable_styling() %>%
        column_spec(2, color = "black", background = ifelse(pageRank_values > threshold , "#90ee90","#ffbfaa"))

      names(threshold) <- "Threshold"
      filters_data <- data.frame(Threshold = threshold, Top = 1 - top)
      rownames(filters_data) <- "Filters"

      if(all(pageRank_values > threshold)) {
        warning("No compression, beacuse all nodes have pageRank value > threshold")
        return(list(filters_data = filters_data, values = table_pagerankvalues))
      } else if(all(pageRank_values < threshold) | all(pageRank_values == threshold)){
        warning("Compression too strong, all pageRank values are < or = threshold")
        return(list(filters_data = filters_data, values = table_pagerankvalues))
      } else{
        # creation of the igraph object in compressed form -- vids: are the nodes that remain the the network graph
        # cause respect the condition
        compressed_nodes <- names(which(pageRank_values > threshold))
        compressed_graph <- induced_subgraph(Complete_network, vids = compressed_nodes)

        edges_data <- links[((as.data.frame(links)[,1] %in% compressed_nodes & as.data.frame(links)[,2] %in% compressed_nodes)),]
        edges <- kbl(edges_data, align = "c") %>%
          kable_styling() %>%
          scroll_box(height = "300px")
      }

      par(mar = c(0, 0, 0, 0))

      # set the layout of the network graph
      layout <- layout_with_graphopt(compressed_graph)

      plot(compressed_graph, vertex.size = size, vertex.label.color = "black", layout = layout)

      return(list(filters_data = filters_data, values = table_pagerankvalues, sub_graph = compressed_graph, edges = edges, edges_data = edges_data))
    } else{
      stop("The input information don't correspond to the input network")
    }
  } else{
    stop("The input network is not an igraph object")
  }
}
