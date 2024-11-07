# Install all the requires R libraries and packages 
# to run the ATLAS data analysis pipeline.

# Packages from the R repository:

# If you need to install other packages, add them to the required_packages list:
required_packages <- c("roxygen2", "crayon", "lubridate",
                       "DBI", "RMySQL", "RSQLite", 
                       "Rcpp", "dplyr", 
                       "leaflet", "sf", "RColorBrewer", "htmltools")

# List from Eitam's code- DELETE LATER
# required_packages <- list("data.table",  "dplyr",  "lubridate" ,  "dbscan" ,
#              "sp" ,  "leaflet" ,"htmltools", "ggpubr","ggmap","ggplot2",
#              "mapview",  "suncalc","toolsForAtlas")


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

# Packages from the ATLAS repository
# install.packages(paste0(path2func,"/toolsForAtlas_1.0.tar.gz"), repos = NULL, type = "source")
# devtools::install_github("pratikunterwegs/atlastools")
# install.packages(paste0(general_path,"/Workshop/toolsForAtlas_1.0.tar.gz"), repos = NULL, type = "source")
# install.packages(paste0(path2func,"/toolsForAtlas_1.0.tar.gz"), repos = NULL, type = "source")
# devtools::install_github("pratikunterwegs/atlastools")

