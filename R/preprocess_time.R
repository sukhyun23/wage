preprocess_time <- function(data) {
  data <- data.table::data.table(data)
  
  # work day
  d9 <- data[
    !day %in% c('일요일', '토요일'), 
    .(근무일 = length(unique(date))), 
    by = .(id, 이름)
    ]
  
  # 기본 근로 시간 : 평일 8시간 이내
  # 평일 주간 일반
  d1 <- data[
    holiday == F, 
    .(week_day_norm = sum(base_hour_day, na.rm = T)), 
    by = .(id, 이름)
    ]
  # 평일 야간 일반
  d3 <- data[
    holiday == F, 
    .(week_night_norm = sum(base_hour_night, na.rm = T)), 
    by = .(id, 이름)
    ]
  
  # 연장 근로 시간 : 평일 8시간 초과
  # 평일 주간 연장
  d2 <- data[
    # type == '주간' & holiday == F, 
    holiday == F,
    .(week_day_over = sum(over_hour_day, na.rm = T)), 
    by = .(id, 이름)
    ]
  # 평일 야간 연장
  d4 <- data[
    # type == '야간' & holiday == F, 
    holiday == F,
    .(week_night_over = sum(over_hour_night, na.rm = T)), 
    by = .(id, 이름)
    ]
  
  # 기본 근로 시간 (휴일) : 8시간 이내
  # 휴일 주간 일반
  d5 <- data[
    # type == '주간' & holiday == T, 
    holiday == T, 
    .(holi_day_norm = sum(base_hour_day, na.rm = T)), 
    by = .(id, 이름)
    ]
  # 휴일 야간 일반
  d7 <- data[
    # type == '야간' & holiday == T,
    holiday == T, 
    .(holi_night_norm = sum(base_hour_night, na.rm = T)),
    by = .(id, 이름)
    ]
  
  # 연장 근로 시간 (휴일) : 8시간 초과
  # 휴일 주간 연장
  d6 <- data[
    # type == '주간' & holiday == T, 
    holiday == T, 
    .(holi_day_over = sum(over_hour_day, na.rm = T)), 
    by = .(id, 이름)
    ]
  # 휴일 야간 연장
  d8 <- data[
    # type == '야간' & holiday == T, 
    holiday == T, 
    .(holi_night_over = sum(over_hour_night, na.rm = T)),
    by = .(id, 이름)
    ]
  
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
  
  result <- result[, .(
    id, 이름, 근무일, 기본근로시간, 연장근로시간,
    휴일근로시간, 휴일연장시간, 야간근로시간
  )]
  
  return(result)
}