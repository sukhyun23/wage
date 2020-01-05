calendar <- function(start, end) {
  calendar_df <- data.table::data.table(date = seq(start, end, by = 1)) 
  calendar_df$weekdays <- calendar_df$date %>% weekdays()
  calendar_df$weekdays <- substr(calendar_df$weekdays, 1, 1)
  calendar_df$weekdays <- factor(
    x = calendar_df$weekdays, 
    levels = calendar_df$weekdays[1:7]
  )
  no <- unlist(lapply(1:5, function(x) rep(x, 7)))
  calendar_df$no <- no[1:nrow(calendar_df)]
  
  calendar_df_t <- data.table::dcast.data.table(
    calendar_df, 
    formula = no~weekdays, 
    value.var = 'date'
  )
  calendar_df_t <- calendar_df_t[,-1]
  for (i in 1:ncol(calendar_df_t)) {
    calendar_df_t[[i]] <- as.Date(
      calendar_df_t[[i]], 
      origin = '1970-1-1'
    )
  }
  return(calendar_df_t)
}