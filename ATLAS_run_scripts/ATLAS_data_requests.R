# This file is for setting the tag numbers and date ranges 
# for the data retrieval from the ATLAS server, 
# or to load data from previously-saved SQLite files.

# Each item in the 'data_requests' list specifies one batch of tag numbers, start time, and end time.
# The user can modify the number of elements in 'data_requests', 
# Also, the user can create different files for different projects.
# If doing so, make sure to specify in the config file, from where to read the relevant data_requests file.

# An example for possible inputs
data_requests <- list(
  list(tag = 972006000836, start_time = '2023-12-24 00:00:01', end_time = '2023-12-25 00:00:01'),
  list(tag = c(972006000837, 972006000841, 972006000847), start_time = '2024-09-05 08:00:00', end_time = '2024-09-05 09:00:00')
)
