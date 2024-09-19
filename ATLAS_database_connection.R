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

connect_to_atlas_db <- function(db_username, db_pass, db_host_ip, db_port_number, db_name) {
  # Establish a connection to the specified ATLAS database
  dbc <- dbConnect(RMySQL::MySQL(),
                   user = db_username,
                   password = db_pass,
                   host = db_host_ip,
                   port = db_port_number,
                   dbname = db_name)
  
  return(dbc)
}


disconnect_from_db <- function(dbc) {
  dbDisconnect(dbc)
}