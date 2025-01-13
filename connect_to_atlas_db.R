
#' Connect to an ATLAS Database
#'
#' This function establishes a connection to an ATLAS database using the provided credentials.
#'
#' @param atlas_db_credentials A named list containing the database connection details. 
#'   The list must include the following elements:
#'   \itemize{
#'     \item{\code{system_name}} {A character string indicating the system name (e.g., "Harod").}
#'     \item{\code{db_username}} {A character string representing the database username.}
#'     \item{\code{db_pass}} {A character string representing the database password.}
#'     \item{\code{db_host_ip}} {A character string representing the database host IP.}
#'     \item{\code{db_port_number}} {An integer representing the database port number.}
#'     \item{\code{db_name}} {A character string representing the name of the database.}
#'   }
#'
#' @return A database connection object (\code{DBI::DBIConnection}) if the connection is successful.
#'
#' @details 
#' This function checks whether the specified system is recognized (e.g., "Harod"). 
#' If the system is not recognized, a warning is issued. 
#' It then attempts to establish a connection to the ATLAS database using `RMySQL::dbConnect()`. 
#' 
#' **Assumptions:**
#' - The database must be accessible via the provided host, port, and credentials.
#' - The user must have the necessary permissions to connect to the database.
#' - The required packages (`DBI` and `RMySQL`) must be installed and loaded.
#'
#' @import DBI
#' @import RMySQL
#' @export
connect_to_atlas_db <- function(atlas_db_credentials) {
  
  # Load the required packages
  required_packages <- c("DBI", "RMySQL")
  invisible(lapply(required_packages, library, character.only = TRUE))
  
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