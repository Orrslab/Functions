matl_simple_dist <- function (data, x = "x", y = "y",step=1) 
{
  assertthat::assert_that(is.data.frame(data), is.character(x), 
                          is.character(y), msg = "simpleDist: some data assumptions are not met")
  if (nrow(data) > 1) {
    x1 <- data[[x]][seq_len(nrow(data) - step)]
    x2 <- data[[x]][(1+step):nrow(data)]
    y1 <- data[[y]][seq_len(nrow(data) - step)]
    y2 <- data[[y]][(1+step):nrow(data)]
    dist <- c(sqrt((x1 - x2)^2 + (y1 - y2)^2))
  }
  else {
    dist <- NA_real_
  }
  return(dist)
}
