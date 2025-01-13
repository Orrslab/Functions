
# Load the required packages
required_packages <- c("DBI", "RMySQL")
sapply(required_packages, library, character.only = TRUE)

#' Connect to an ATLAS Database
#'
#' This function establishes a connection to any ATLAS database, given the required credentials.
#'
#' @param db_username A character string representing the database username.
#' @param db_pass A character string representing the database password.
#' @param db_host_ip A character string representing the database host IP.
#' @param db_port_number An integer representing the database port number.
#' @param db_name A character string representing the name of the database.
#'
#' @return A database connection object (\code{dbc}) if the connection is successful.

connect_to_atlas_db <- function(atlas_db_credentials) {
  
  # Get the credentials of the ATLAS database you need to connect to
  
  if (atlas_db_credentials$system_name != "Harod") {
    warning(paste("Warning: system", atlas_db_credentials$system_name, "is not defined"))
  }
  
  # Establish a connection to the specified ATLAS database
  dbc <- dbConnect(RMySQL::MySQL(),
                   user = atlas_db_credentials$db_username,
                   password = atlas_db_credentials$db_pass,
                   host = atlas_db_credentials$db_host_ip,
                   port = atlas_db_credentials$db_port_number,
                   dbname = atlas_db_credentials$db_name)
  
  return(dbc)
}