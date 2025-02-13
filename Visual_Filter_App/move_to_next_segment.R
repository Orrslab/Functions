# helper function to move to the next segment when the Next Segment button is pushed
move_to_next_segment <- function(data_segment_action, current_segment_index, validate_data_for_days, reactive_num_points, data_for_filter_sf) {
  if (data_segment_action == "days") {
    # Increment the current day number
    next_day_number <- current_segment_index() + 1
    day_numbers <- validate_data_for_days()  # Retrieve day numbers from the reactive
    if (next_day_number <= length(day_numbers)) {
      current_segment_index(next_day_number)
    }
  } else if (data_segment_action == "number_of_points") {
    num_points <- reactive_num_points()  # Number of points for the next segment
    start_point <- current_segment_index()  # Get the current start point
    # Calculate the new start point for the next segment
    new_start_point <- start_point + num_points
    # Ensure that we don't exceed the total number of points
    if (new_start_point <= nrow(data_for_filter_sf)) {
      current_segment_index(new_start_point)
    }
  }
}