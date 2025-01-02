
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

connect_to_atlas_db <- function(SYS=system_name_harod, 
                                db_username,
                                db_pass,
                                db_host_ip,
                                db_port_number,
                                db_name) {
  
  # Get the credentials of the ATLAS database you need to connect to
  
  if (SYS!=system_name_harod) {
    stop(glue("Warning: system {SYS} is not defined"))
  }
  
  # Establish a connection to the specified ATLAS database
  dbc <- dbConnect(RMySQL::MySQL(),
                   user = db_username,
                   password = db_pass,
                   host = db_host_ip,
                   port = db_port_number,
                   dbname = db_name)
  
  return(dbc)
}