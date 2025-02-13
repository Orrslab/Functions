# Helper function to move to the previous data segment when the Previous Segment button is pushed.
move_to_previous_segment <- function(data_segment_action, current_segment_index, validate_data_for_days, reactive_num_points, data_for_filter_sf) {
  if (data_segment_action == "days") {
    # Decrement the current day number
    previous_day_number <- current_segment_index() - 1
    if (previous_day_number >= 1) {
      current_segment_index(previous_day_number)
    }
  } else if (data_segment_action == "number_of_points") {
    num_points <- reactive_num_points()
    # Get the current start point for the previous segment
    start_point <- current_segment_index()
    # Calculate the new start point for the previous segment
    new_start_point <- max(1, start_point - num_points)  # Prevent going below 1
    current_segment_index(new_start_point)
  }
}