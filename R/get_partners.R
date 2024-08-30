#' Get the participant of the network
#'
#' This function takes a protein name and retrieves network information from the STRING database via API
#'
#' @param protein_name A character string representing the protein name.
#' @param species The numeric ID for the species, default is 9606 (Human).
#' @importFrom httr GET status_code content
#' @importFrom readr read_tsv
#' @return A data frame describing the network.
#' @examples
#'
#' # Example usage:
#' # Retrieve network information for the protein TP53 in humans
#' network_data <- get_partners("TP53")
#' print(network_data)
#'
#' #' # Retrieve network information for a different species
#' network_data_mouse <- get_partners("TP53", species = 10090)
#' print(network_data_mouse)
#' @export
get_partners <- function(protein_name, species = 9606) {

  if (nchar(protein_name) == 0) {
    stop("Protein name must not be empty")                  # raising an error in case of empty protein name
  }

  STRING_partner <- "https://string-db.org/api/tsv/network"  # API of the the STRING DB, precisely refers to the network information
  parameters_partners <- list(                               # parameters that I use to find the protein
    identifiers = protein_name,
    species = species
  )

  http_response <- GET(STRING_partner, query = parameters_partners)

  if (status_code(http_response) == 200) {                                 # check if all is good, in particular if I have the information I have
    content_data <- content(http_response, "text", encoding = "UTF-8")      # status code 200 and i create a data frame with them
    df <- read_tsv(content_data, col_names = TRUE, show_col_types = FALSE)
    return(df)
  } else {
    stop("Couldn't find the protein. Error:", status_code(http_response), "\n Ensure to have internet connection and check if you write the protein name correctly [the uniprot name is preferred]")
  }
}

# In case of status code different from 200 an error is raised
