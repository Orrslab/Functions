# This file is for setting the tag number and date ranges 
# for the data retrieval from the ATLAS server, 
# or to load data from previously-saved SQLite files, to visualize on the visual filter shiny app

# An example for a possible input

# data_requests <- list(
#   list(tag = 972006000430, start_time = '2021-08-23 17:00:00', end_time = '2021-08-24 05:00:00')
# )

# The times need to be in UTC
# Only one tag number should be inserted at a time

data_requests <- list(
  list(tag = 972006000430, 
       start_time = '2021-08-23 17:00:00', 
       end_time = '2021-08-24 05:00:00')
  )

# data_requests <- list(
#   list(tag = 972006000683, start_time = '2022-07-26 15:00:00', end_time = '2022-07-27 08:00:00')
# )
