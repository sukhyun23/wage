is.plus <- function(x) grepl('\\+', x)
is.commute <- function(x) {
  grepl('^[0-9]{2}\\:[0-9]{2}$|\\+[0-9]{2}\\:[0-9]{2}$', x)
}
is.date <- function (x) {
  class_x <- class(x)
  result <- any(class_x %in% c("Date", "POSIXct", "POSIXlt", "POSIXt"))
  return(result)
}