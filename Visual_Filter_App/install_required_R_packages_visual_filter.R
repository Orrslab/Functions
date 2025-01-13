# Install all the requires R libraries and packages 
# to run the Visual Filter Shiny App.

# If you need to install other packages, add them to the required_packages list:
required_packages <- c("roxygen2", "crayon", "lubridate",
                       "DBI", "RMySQL", "RSQLite", 
                       "Rcpp", "dplyr", 
                       "leaflet", "sf", "RColorBrewer", "htmltools", 
                       "leaflet.extras")

# Function that installs the packages that have not been installed before
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(sprintf("Installing package: %s\n", pkg))
      install.packages(pkg)
    } else {
      cat(sprintf("Package %s is already installed.\n", pkg))
    }
  }
}

# Install the new required packages
install_if_missing(required_packages)
