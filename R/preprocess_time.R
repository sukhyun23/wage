preprocess_time <- function(data) {
  agg <- function(x, FUN, group, name) {
    result <- aggregate(
      x = x,
      by = group,
      FUN
    )
    names(result) <- c('ID', '이름', name)
    return(result)
  }
  data_empty <- unique(data.frame(ID = data$ID, 이름 = data$이름))
  
  # work day
  idx <- (!data$요일 %in% c('일요일', '토요일'))
  if (any(idx)) {
    x <- data$근무일자[idx]
    group <- list(data$ID[idx], data$이름[idx])
    d_workday <- agg(x, function(x) length(unique(x)), group, '근무일')  
  } else {
    x <- data$근무일자
    group <- list(data$ID, data$이름)
    d_workday <- agg(x, function(x) length(unique(x)), group, '근무일')
  }
  
  # 기본 근로 시간 : 평일 8시간 이내
  # 평일 주간 일반
  idx <- (!data$휴일)
  if (any(idx)) {
    x <- data$주간기본근로[idx]
    group <- list(data$ID[idx], data$이름[idx])
    d_wd_day_base <- agg(x, sum, group, '평일주간기본근로합계')
    
    # 평일 야간 일반
    x <- data$야간기본근로[idx]
    d_wd_night_base <- agg(x, sum, group, '평일야간기본근로합계')
    
    # 연장 근로 시간 : 평일 8시간 초과
    # 평일 주간 연장
    x <- data$주간연장근로[idx]
    d_wd_day_over <- agg(x, sum, group, '평일주간연장근로합계')
    
    # 평일 야간 연장
    x <- data$야간연장근로[idx]
    d_wd_night_over <- agg(x, sum, group, '평일야간연장근로합계')
    
    # 평일 야간 수당 근로 **
    x <- data$야간수당근로[idx]
    d_wd_night_pay <- agg(x, sum, group, '평일야간수당근로합계')
  } else {
    d_wd_day_base <- data_empty
    d_wd_day_base$평일주간기본근로합계 <- 0
    
    d_wd_night_base <- data_empty
    d_wd_night_base$평일야간기본근로합계 <- 0
    
    d_wd_day_over <- data_empty
    d_wd_day_over$평일주간연장근로합계 <- 0
    
    d_wd_night_over <- data_empty
    d_wd_night_over$평일야간연장근로합계 <- 0
    
    d_wd_night_pay <- data_empty
    d_wd_night_pay$평일야간수당근로합계 <- 0
  }
  
  # 기본 근로 시간 (휴일) : 8시간 이내
  # 휴일 주간 일반
  idx <- (data$휴일)
  if (sum(idx, na.rm = T) >= 1) {
    x <- data$주간기본근로[idx]
    group <- list(data$ID[idx], data$이름[idx])
    d_hol_day_base <- agg(x, sum, group, '휴일주간기본근로합계')
    
    # 휴일 야간 일반
    x <- data$야간기본근로[idx]
    d_hol_night_base <- agg(x, sum, group, '휴일야간기본근로합계')
    
    # 연장 근로 시간 (휴일) : 8시간 초과
    # 휴일 주간 연장
    x <- data$주간연장근로[idx]
    d_hol_day_over <- agg(x, sum, group, '휴일주간연장근로합계')
    
    # 휴일 야간 연장
    x <- data$야간연장근로[idx]
    d_hol_night_over <- agg(x, sum, group, '휴일야간연장근로합계')  
    
    # 휴일 야간 수당 근로 **
    x <- data$야간수당근로[idx]
    d_hol_night_pay <- agg(x, sum, group, '휴일야간수당근로합계')
    
  } else {
    d_hol_day_base <- data_empty
    d_hol_day_base$휴일주간기본근로합계 <- 0
    
    d_hol_night_base <- data_empty
    d_hol_night_base$휴일야간기본근로합계 <- 0
    
    d_hol_day_over <- data_empty
    d_hol_day_over$휴일주간연장근로합계 <- 0
    
    d_hol_night_over <- data_empty
    d_hol_night_over$휴일야간연장근로합계 <- 0
    
    d_hol_night_pay <- data_empty
    d_hol_night_pay$휴일야간수당근로합계 <- 0
  }
  
  # 지각 조퇴
  x <- data$지각조퇴시간
  group <- list(data$ID, data$이름)
  d_late <- agg(x, sum, group, '지각조퇴')
  
  result <- list(
    d_workday,
    d_wd_day_base, d_wd_night_base, d_wd_day_over, d_wd_night_over, 
    d_hol_day_base, d_hol_night_base, d_hol_day_over, d_hol_night_over,
    d_wd_night_pay, d_hol_night_pay, d_late
  )
  result <- result[vapply(result, nrow, 1) >= 1]
  result <- Reduce(
    function(...) merge(..., by = c('ID', '이름'), all = T), 
    result
  )
  for (i in 3:ncol(result)) {
    result[[i]] <- ifelse(is.na(result[[i]]), 0, result[[i]])
  }
  
  result$기본근로시간 <- result$평일주간기본근로합계 + result$평일야간기본근로합계
  result$연장근로시간 <- result$평일주간연장근로합계 + result$평일야간연장근로합계
  result$휴일근로시간 <- result$휴일주간기본근로합계 + result$휴일야간기본근로합계
  result$휴일연장시간 <- result$휴일주간연장근로합계 + result$휴일야간연장근로합계
  result$야간근로시간 <- result$평일야간기본근로합계 + result$휴일야간기본근로합계
  result$야간수당시간 <- result$평일야간수당근로합계 + result$휴일야간수당근로합계
  
  varnames <- c(
    'ID', '이름', '근무일', '기본근로시간', '연장근로시간', '지각조퇴',
    '휴일근로시간', '휴일연장시간', '야간근로시간', '야간수당시간'
  )
  
  result <- result[, varnames]
  return(result)
}