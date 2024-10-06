
## Paths
# path of the data analysis repo ("Functions") in your computer
path_to_atlas_data_analysis_repo <- "C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/"
path_to_scripts <- paste0(path_to_atlas_data_analysis_repo,"Scripts/")
path_to_sqlite_files <- ("C:/Users/netat/Documents/Movement_Ecology/ATLAS/ATLAS_data_sqlite/")

## Define if to retrieve data from the ATLAS server (TRUE),
# or load data from an existing .sqlite file (FALSE)
retrieve_data_from_server <- TRUE

# Choose if to save or not to save new data retrieved from the ATLAS server
# The code checks this variable only if load_data_from_sqlite_file == FALSE
save_data_to_sqlite_file <- FALSE


## Credentials of the Harod ATLAS database
system_name_harod="Harod"
# username 
db_username_harod = 'roatlasharod'            
# password
db_pass_harod = 'roatlasHarodOrr5678#'
# host ip address
db_host_ip_harod = '132.66.79.21' 
# port Number
db_port_number_harod=5900   
# name of data base
db_name_harod='harod'

# ATLAS time information
atlas_time_format <- "%Y-%m-%d %H:%M:%S"
atlas_time_zone <- "UTC"


# Paths from Eitam's code atlas main
# general_path <- "C:/Users/97254/Google Drive/POST/ATLAS/" # my dell computer directory
# # general_path <- "C:/Users/eitam.arnon/OneDrive - GTIIT/POST/ATLAS/" # my GT computer
# setwd       (paste0(general_path,"Harod"))
# path2func <- paste0(general_path,"functions/")
# path2data <- paste0(general_path,"Harod/data/")