
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

connect_to_atlas_db <- function(SYS=system_name_harod) {
  
  # Get the credentials of the ATLAS database you need to connect to
  
  if (SYS==system_name_harod) {
    # Get the Harod database credentials from the configuration file
    db_username = db_username_harod         # username
    db_pass = db_pass_harod                 # password
    db_host_ip = db_host_ip_harod           # host ip address
    db_port_number = db_port_number_harod   # port Number
    db_name = db_name_harod                 # name of data base
    
  } else {
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