preprocess_time <- function(data) {
  agg <- function(x, FUN, group, name) {
    result <- aggregate(
      x = x,
      by = group,
      FUN
    )
    names(result) <- c('id', '이름', name)
    return(result)
  }
  
  # work day
  idx <- !data$day %in% c('일요일', '토요일')
  x <- data$date[idx]
  group <- list(data$id[idx], data$이름[idx])
  d9 <- agg(x, function(x) length(unique(x)), group, '근무일')
  
  # 기본 근로 시간 : 평일 8시간 이내
  # 평일 주간 일반
  idx <- !data$holiday
  x <- data$base_hour_day[idx]
  group <- list(data$id[idx], data$이름[idx])
  d1 <- agg(x, sum, group, 'week_day_norm')
  
  
  # 평일 야간 일반
  x <- data$base_hour_night[idx]
  d3 <- agg(x, sum, group, 'week_night_norm')
  
  # 연장 근로 시간 : 평일 8시간 초과
  # 평일 주간 연장
  x <- data$over_hour_day[idx]
  d2 <- agg(x, sum, group, 'week_day_over')
  
  
  # 평일 야간 연장
  x <- data$over_hour_night[idx]
  d4 <- agg(x, sum, group, 'week_night_over')
  
  # 기본 근로 시간 (휴일) : 8시간 이내
  # 휴일 주간 일반
  idx <- data$holiday
  x <- data$base_hour_day[idx]
  group <- list(data$id[idx], data$이름[idx])
  d5 <- agg(x, sum, group, 'holi_day_norm')
  
  # 휴일 야간 일반
  x <- data$base_hour_night[idx]
  d7 <- agg(x, sum, group, 'holi_night_norm')
  
  # 연장 근로 시간 (휴일) : 8시간 초과
  # 휴일 주간 연장
  x <- data$over_hour_day[idx]
  d6 <- agg(x, sum, group, 'holi_day_over')
  
  # 휴일 야간 연장
  x <- data$over_hour_night[idx]
  d8 <- agg(x, sum, group, 'holi_night_over')
  
  result <- Reduce(
    function(...) merge(..., by = c('id', '이름'), all = T), 
    list(d1, d2, d3, d4, d5, d6, d7, d8, d9)
  )
  
  result <- data.table::data.table(result)
  for (i in 3:10) {
    result[[i]] <- ifelse(is.na(result[[i]]), 0, result[[i]])
  }
  
  result$기본근로시간 <- result$week_day_norm + result$week_night_norm
  result$연장근로시간 <- result$week_day_over + result$week_night_over
  result$휴일근로시간 <- result$holi_day_norm + result$holi_night_norm
  result$휴일연장시간 <- result$holi_day_over + result$holi_night_over
  result$야간근로시간 <- result$week_night_norm + result$holi_night_norm
  
  result <- result[, c(
    'id', '이름', '근무일', '기본근로시간', '연장근로시간',
    '휴일근로시간', '휴일연장시간', '야간근로시간'
  )]
  
  return(result)
}