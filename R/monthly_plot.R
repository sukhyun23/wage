monthly_plot <- function(data) {
  gdat <- data
  gdat$x <- lubridate::day(gdat$start)
  gdat$y <- -as.numeric(gdat$id)
  gdat$base_hour <- gdat$base_hour_day + gdat$base_hour_night
  gdat$y <- ifelse(gdat$base_hour <= 0, NA, gdat$y)
  
  ydat <- unique(gdat[!is.na(y), .(y, 이름)])
  ydat$x <- -0.8
  
  tmp_date <- as.Date(gdat$date)
  xdat <- data.frame(date = seq(min(tmp_date), max(tmp_date), by = 1))
  xdat$day <- weekdays(xdat$date)
  xdat$x <- lubridate::day(xdat$date)
  xdat$day <- stringr::str_sub(xdat$day, 1, 1)
  xdat$y <- min(ydat$y) - 1.3
  
  segdat <- xdat[xdat$day %in% c('월', '금'), ]
  segdat$yend <- max(ydat$y) + 1
  segdat$y <- min(ydat$y) - 1
  
  g <- ggplot(gdat, aes(x = x, y = y)) + 
    geom_segment(
      aes(x = x, xend = x, y = y, yend = yend), 
      color = 'grey60', 
      data = segdat
    ) + 
    geom_tile(aes(fill = night), color = 'black') + 
    scale_fill_manual(values = c('yellow', 'navy')) + 
    scale_x_continuous(breaks = min(gdat$x):max(gdat$x)) + 
    scale_y_continuous(breaks = min(gdat$y, na.rm = T):max(gdat$y, na.rm = T)) + 
    geom_text(aes(x = x, y = y, label = 이름), data = ydat) +
    geom_text(aes(x = x, y = y, label = day), data = xdat) +
    guides(fill = F) + 
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(fill = 'white'),
      panel.grid.minor = element_blank()
      # panel.grid.major = element_line(color = 'grey85')
    )
  g
}