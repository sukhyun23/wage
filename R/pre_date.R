pre_date <- function(x) {
  y <- lubridate::year(x)
  m <- lubridate::month(x)
  
  result <- as.Date(
    paste(
      ifelse(m == 1, y - 1, y),
      ifelse(m >= 2, m - 1, 12),
      '01',
      sep = '-'
    )
  )
  return(result)
}
