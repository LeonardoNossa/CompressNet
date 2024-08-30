#' This is a data set example, useful to practice with the package's function
#'
#' This function takes import a data set as an example
#'
#' @return A data frame (representing a network) that correspond to the data set example.
#'
#' @examples
#'
#' # Example usage of Netty function
#' network_data <- Netty()
#' print(network_data)
#'
#' @export

Netty <- function(){


  # data frame creation with some data, these data are referring to SHH
  # the data are taken on the STRIING DB
  SHH_data <- data.frame(
    preferredName_A = c("GLI1", "GLI1", "GLI1", "GLI1", "GLI1", "GLI1", "GLI1", "GLI1", "GLI1",
                        "SMO", "SMO", "SMO", "SMO", "SMO", "SMO", "SMO", "SMO", "BOC", "BOC",
                        "BOC", "BOC", "BOC", "BOC", "BOC", "BOC", "DISP1", "DISP1", "DISP1",
                        "DISP1", "DISP1", "HHIP", "HHIP", "HHIP", "HHIP", "HHIP", "SHH", "SHH",
                        "SHH", "SHH", "SHH", "GAS1", "GAS1", "GAS1", "GAS1", "PTCH1", "PTCH1",
                        "PTCH1", "PTCH2", "PTCH2", "CDON" ),
    preferredName_B = c("DISP1", "BOC", "CDON", "GAS1", "PTCH2", "HHIP", "PTCH1", "SHH", "SMO",
                        "DISP1", "GAS1", "CDON", "HHIP", "BOC", "PTCH2", "SHH", "PTCH1", "DISP1",
                        "HHIP", "LRP2", "CDON", "PTCH2", "GAS1", "PTCH1", "SHH", "PTCH1", "PTCH2",
                        "CDON", "GAS1", "SHH", "CDON", "PTCH2", "GAS1", "PTCH1", "SHH",  "LRP2",
                        "GAS1", "PTCH1", "CDON", "PTCH2", "LRP2", "PTCH2", "CDON", "PTCH1", "PTCH2",
                        "LRP2", "CDON", "LRP2", "CDON", "LRP2" ),
    score = c(0.413, 0.481, 0.552, 0.586, 0.842, 0.898, 0.978, 0.979, 0.988, 0.598, 0.758, 0.798,
              0.848,0.963, 0.993, 0.999, 0.999, 0.476, 0.619, 0.916, 0.942, 0.974, 0.982, 0.993,
              0.996, 0.506, 0.561, 0.672, 0.694, 0.996, 0.592, 0.690, 0.736, 0.969, 0.999, 0.992,
              0.999, 0.999, 0.999, 0.999, 0.935, 0.955, 0.995, 0.998, 0.928, 0.932, 0.998, 0.924,
              0.968, 0.924)
    )

  return(SHH_data)
}
