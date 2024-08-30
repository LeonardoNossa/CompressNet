#' Draw the complete network
#'
#' This function takes a dataframe that describe the interaction between protein and with these information plot the network
#'
#' @param dataframe_interaction A dataframe with the necessary information
#' @param nodeA The first column of dataframe referring to the nodes that are inside the network (interact with nodeB)
#' @param nodeB The second column of dataframe referring to the nodes that are inside the network (interact with nodeA)
#' @param score The score column of dataframe that defines the links between the nodes
#' @param fontSize Is the font size of node labels (default : 0.7)
#' @param opacity For the network inks and nodes (default : 0.5)
#' @param size Is the size of nodes (default: 25)
#' @importFrom dplyr distinct select %>%
#' @importFrom scales alpha rescale
#' @importFrom igraph graph_from_data_frame layout_with_graphopt
#' @importFrom kableExtra kbl kable_styling scroll_box
#' @importFrom grDevices rainbow
#' @return The complete network visualization
#' @examples
#'
#' # Example usage:
#' # Create an example dataset
#' data_example <- data.frame(
#'   nodeA = c("A", "B", "C"),
#'   nodeB = c("B", "C", "A"),
#'   score = c(0.8, 0.9, 0.85)
#' )
#' # Draw the complete network
#' draw_complete_net(data_example, nodeA = "nodeA", nodeB = "nodeB", score = "score")
#' @export
draw_complete_net <- function(dataframe_interaction, nodeA, nodeB, score, fontSize = 0.7, opacity = 0.5, size = 25){

  # check if the data object is a data frame
  # check if the selected columns are in the data frame
  # also is raised by default errors if you don't put as input all the 3 main parameters (dataframe_interaction, nodeA, nodeB)
  if (is.data.frame(dataframe_interaction)) {
    if (nodeA %in% colnames(dataframe_interaction) & nodeB %in% colnames(dataframe_interaction) & score %in% colnames(dataframe_interaction)) {

      # Check if the dataframe is empty
      if (nrow(dataframe_interaction) == 0) {
        stop("The dataframe is empty. Returning NULL.")
      }

      set.seed(123)  # set a seed to have always the same network (constrains nodes to certain positions)

      # Need to create two dataframe: one with the unique participant of the network and
      # one with the pairs of participant (for establish the edge)
      nodes <- distinct(data.frame(name = unique(c(dataframe_interaction[[nodeA]], dataframe_interaction[[nodeB]]))))  # [[]] is a tool to recall the column name

      links <- dataframe_interaction %>%                      # %>% powerful command, it means that the code is referring to the dataframe or object specified, in this case dataframe_interaction
        select({{nodeA}}, {{nodeB}}, score = {{score}})      # create a data frame with the useful columns

      edges <- kbl(links, align = "c") %>%                  # table creation (kable object) about the network information (node-link)
        kable_styling() %>%
        scroll_box(height = "300px")

      # Creation of igraph object, need it for plot the network
      Complete_network <- graph_from_data_frame(links, directed=FALSE, vertices = nodes)

      # Set the nodes properties and characteristics
      igraph::V(Complete_network)$color = alpha(rainbow(length(V(Complete_network))), opacity )  # nodes color
      igraph::V(Complete_network)$size = size   # nodes size
      igraph::V(Complete_network)$label.cex = fontSize  #font size of nodes label

      # Set the edges properties and characteristics, with check for empty score vector
      if(length(igraph::E(Complete_network)$score) > 0){
        igraph::E(Complete_network)$width = rescale(igraph::E(Complete_network)$score, to = c(1,5))   # set the weighted edge (based on score)
      } else {
        igraph::E(Complete_network)$width = 1  # default width for empty edge score
      }
      igraph::E(Complete_network)$color = alpha("grey", opacity) #edges color

      par(mar = c(0, 0, 0, 0))

      # set the layout of the network graph
      layout = layout_with_graphopt(Complete_network)

      # Call all the components for the plot
      plot(Complete_network,
           vertex.label = igraph::V(Complete_network)$name, vertex.size = igraph::V(Complete_network)$size,
           edge.width = igraph::E(Complete_network)$width, edge.color = igraph::E(Complete_network)$color,
           vertex.color = igraph::V(Complete_network)$color,vertex.label.color = "black", layout = layout)

      return(list(Complete_network = Complete_network, edges = edges, nodes = nodes, edges_data = links))
    } else {
      stop("Something went wrong with the columns name, check if these columns are inside the dataframe")
    }
  } else {
    stop("Your object with data is not a dataframe")
  }
}
