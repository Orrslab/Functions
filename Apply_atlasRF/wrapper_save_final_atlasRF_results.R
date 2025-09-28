
source(file.path(getwd(), "get_global_time_range_of_data_requests.R"))
source(file.path(getwd(), "create_filename_without_extension.R"))

#' Save Final atlasRF Results with Outlier Labels
#'
#' This wrapper function saves the final localization data annotated with
#' atlasRF outlier labels as an RDS file. The output file name is generated
#' automatically based on the species, tag numbers, and the global time range
#' of the dataset.
#'
#' @param localization_data_with_outlier_labels A data frame containing
#'   localization points with an \code{Outliers_atlasRF} (or equivalent) column
#'   indicating outlier classification results.
#' @param config A list containing configuration settings, including:
#'   \itemize{
#'     \item \code{data_requests}: List of data request objects with start and end times.
#'     \item \code{atlas_time_and_coordinates_info}: List with elements
#'           \code{atlas_time_zone} and \code{atlas_time_format}.
#'     \item \code{animal_name_code}: Identifier for the species/animal.
#'     \item \code{paths$folder_atlasRF_results}: Output directory for results.
#'   }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Extracts the global start and end times across all data requests.
#'   \item Constructs a descriptive file name based on species, tag numbers,
#'         and the time range using \code{create_filename_without_extension()}.
#'   \item Saves the final dataset with outlier labels as an RDS file in
#'         \code{config$paths$folder_atlasRF_results}.
#' }
#'
#' @return
#' The function does not return an object. It saves an RDS file to disk.
#'
#' @seealso \code{\link{get_global_time_range_of_data_requests}},
#'   \code{\link{create_filename_without_extension}}
#'
#' @export
#' 
wrapper_save_final_atlasRF_results <- function(localization_data_with_outlier_labels, config) {
  
  ## Generate the file name
  
  # Get the global start and end times of all the data
  global_times_data <- get_global_time_range_of_data_requests(
    data_requests = config$data_requests,
    data_time_zone = config$atlas_time_and_coordinates_info$atlas_time_zone,
    data_time_format = config$atlas_time_and_coordinates_info$atlas_time_format)
  
  # Generate the file name without extension- to save the ATLAS data
  filename_without_extension <- create_filename_without_extension(
    animal_name_code = config$animal_name_code, 
    tag_numbers = sapply(config$data_requests, function(x) x$tag), 
    start_time = global_times_data$start_time, 
    end_time = global_times_data$end_time)
  
  saveRDS(localization_data_with_outlier_labels, file = file.path(config$paths$folder_atlasRF_results, paste0(filename_without_extension, "_with_outlier_labels.rds")))
  
  message("*** Saved localization data with Outlier labels. ***")
  
}