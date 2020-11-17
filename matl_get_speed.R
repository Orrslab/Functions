matl_get_speed <- function (data, x = "x", y = "y", time = "time", type = "in", step=1) 
{
  # atlastools::atl_check_data(data, names_expected = c(x, y, time))
  data.table::setorderv(data, time)
  distance <- matl_simple_dist(data, x, y,step)
  # distance <- distance[(step+1):length(distance)]
  dtime <- data[[time]][(step+1):nrow(data)]-data[[time]][1:(nrow(data)-step)]
  # time <- c(NA, diff(data[[time]]))
  speed <- distance/dtime
  if (type == "in") {
    speed <- c(rep(NA,step),speed)
  }
  else if (type == "out") {
    speed <-c(speed,rep(NA,step))
  }
  return(speed)
}
