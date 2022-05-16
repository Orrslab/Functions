# Sourcing packages and local functions
# packages are assumed to be installed (Installing packages is commented and can be used before first use)
# if no input is given, looks for a "functions" directory in current directory   
ConnectLib <- function (path2func="/functions/") {
  pcks <- list("data.table",  "dplyr",  "lubridate" ,  "dbscan" ,
               "sp" ,  "leaflet" ,"htmltools" ,  "RSQLite", "ggpubr","ggmap","ggplot2",
                "mapview",  "suncalc","toolsForAtlas")
  sapply(pcks, require, char = TRUE)        #sourcing these packages
  funcFiles <- list.files(path2func,pattern = "\\.R$")  
  funcFiles <- funcFiles[-which(funcFiles=="ATLAS_main.R" )]
  invisible(lapply(paste0(path2func,funcFiles), source))        #sourcing these packages
}

#  ----------------------------- 
# install.packages()
# install.packages(paste0(path2func,"/toolsForAtlas_1.0.tar.gz"), repos = NULL, type = "source")
# devtools::install_github("pratikunterwegs/atlastools")
# install.packages(paste0(general_path,"/Workshop/toolsForAtlas_1.0.tar.gz"), repos = NULL, type = "source")
# install.packages(paste0(path2func,"/toolsForAtlas_1.0.tar.gz"), repos = NULL, type = "source")
# devtools::install_github("pratikunterwegs/atlastools")