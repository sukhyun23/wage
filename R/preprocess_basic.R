preprocess_basic <- function(data, holiday_date = NA) {
  # change names
  data <- data[,1:7]
  data$비고 <- '정상'
  
  # fix words
  data$근무일자 <- as.Date(data$근무일자)
  data$요일 <- weekdays(data$근무일자)
  data$근무일명칭[data$근무일명칭 == '평일'] <- '주간'
  
  # error : 근무일 명칭
  data$비고 <- ifelse(
    !data$근무일명칭 %in% c('평일', '주간', '야간'),
    '근무일명칭', data$비고
  )
  
  data$출근_raw <- data$출근
  data$퇴근_raw <- data$퇴근
  
  # night duty
  data$야간 <- grepl('\\+', data$퇴근)
  data$퇴근 <- gsub('\\+', '', data$퇴근)
  data$출근 <- gsub('\\+', '', data$출근)
  
  # error : 근무일 명칭과 시간이 맞는지
  data$비고 <- ifelse(
    data$근무일명칭 == '야간' & (substr(data$출근, 1, 2) < 10), # 야간인데 오전 10시 이전 출근
    '근무일명칭', data$비고
  )
  data$비고 <- ifelse(
    data$근무일명칭 == '주간' & (substr(data$출근, 1, 2) > 17), # 주간인데 오후 5시(17시) 이후 출근
    '근무일명칭', data$비고
  )
  
  # error : 출퇴근은 "00:00"으로 되어있어야함.
  data$비고 <- ifelse(
    (!is.commute(data$출근) & !is.commute(data$퇴근)) | is.plus(data$출근),
    '출퇴근형식', data$비고
  )
  
  # holiday
  data$휴일 <- ifelse(data$근무일자 %in% holiday_date, T, F)
  
  # fix start, end
  data$출근 <- ifelse(is.na(data$출근), '00:00', data$출근)
  data$퇴근 <- ifelse(is.na(data$퇴근), '00:00', data$퇴근)
  data$출근 <- as.POSIXct(paste(data$근무일자, data$출근), tz = 'KST')
  data$퇴근 <- as.POSIXct(paste(data$근무일자, data$퇴근), tz = 'KST')
  data[data$야간 == T, ]$퇴근 <- data[data$야간 == T, ]$퇴근 + 60*60*24
  
  # 출근
  hour_on <- lubridate::hour(data$출근) + 
    ifelse(lubridate::minute(data$출근) > 5, 1, 0)
  idx_nextday <- ifelse(hour_on >= 25, 1, 0)
  day_on <- lubridate::day(data$출근)
  if (any(idx_nextday == 1)) {
    day_on <- day_on + idx_nextday
  }
  date_on <- paste(
    lubridate::year(data$출근),
    lubridate::month(data$출근),
    day_on, 
    sep = '-'
  )
  on_time <- as.POSIXct(
    paste(date_on, paste(hour_on, '00', sep = ':')), tz = 'KST'
  )
  data$실출근시간 <- on_time
  
  # 퇴근
  hour_off <- lubridate::hour(data$퇴근) + 
    ifelse(lubridate::minute(data$퇴근) >= 50, 1, 0)
  idx_nextday <- ifelse(hour_off >= 25, 1, 0)
  day_off <- lubridate::day(data$퇴근)
  if (any(idx_nextday == 1)) {
    day_off <- day_off + idx_nextday
  }
  date_off <- paste(
    lubridate::year(data$퇴근),
    lubridate::month(data$퇴근),
    day_off, 
    sep = '-'
  )
  off_time <- as.POSIXct(
    paste(date_off, paste(hour_off, '00', sep = ':')),
    tz = 'KST'
  )
  data$실퇴근시간 <- off_time
  
  # remove na 
  data <- data[data$근무일명칭 != '휴일', ]
  data <- data[!is.na(data$출근) | !is.na(data$퇴근), ]
  data <- data[data$출근 != data$퇴근, ]
  
  # total work hour 
  data$근로시간 <- difftime(data$실퇴근시간, data$실출근시간, units = 'hours')
  data$근로시간 <- round(as.numeric(data$근로시간))
  
  # day time
  idx_day <- data$근무일명칭 == '주간'
  data$주간기본근로 <- 0
  data$주간기본근로[idx_day] <- ifelse(
    data$근로시간[idx_day] >= 8, 8, data$근로시간[idx_day]
  )
  data$주간연장근로 <- 0
  data$주간연장근로[idx_day] <- data$근로시간[idx_day] - 
    data$주간기본근로[idx_day]
  
  ture_start_hour <- lubridate::hour(data$실출근시간)
  true_end_hour <- lubridate::hour(data$실퇴근시간)
  # 13시 이후 퇴근 & 근로시간 5시간 이상
  idx_break <- (true_end_hour[idx_day] >= 13 & data$근로시간[idx_day] >= 5) |
    (data$근로시간[idx_day] >= 13)
  # deduction break time
  data$주간기본근로[idx_day] <- ifelse(
    data$주간연장근로[idx_day] == 0 & idx_break,
    data$주간기본근로[idx_day] - 1,
    data$주간기본근로[idx_day]
  )
  data$주간연장근로[idx_day] <- ifelse(
    data$주간연장근로[idx_day] > 0 & idx_break,
    data$주간연장근로[idx_day] - 1,
    data$주간연장근로[idx_day]
  )
  
  # night
  idx_night <- data$근무일명칭 == '야간'
  data$야간기본근로 <- 0
  data$야간기본근로[idx_night] <- ifelse(
    data$근로시간[idx_night] >= 8, 8, data$근로시간[idx_night]
  )
  data$야간연장근로 <- 0
  data$야간연장근로[idx_night] <- data$근로시간[idx_night] - 
    data$야간기본근로[idx_night]
  # 01시 이후 퇴근 & 근로시간 5시간 이상
  idx_break <- (true_end_hour[idx_night] >= 1 & data$근로시간[idx_night] >= 5) |
    (data$근로시간[idx_night] >= 13)
  # deduction break time
  data$야간기본근로[idx_night] <- ifelse(
    data$야간연장근로[idx_night] == 0 & idx_break,
    data$야간기본근로[idx_night] - 1,
    data$야간기본근로[idx_night]
  )
  data$야간연장근로[idx_night] <- ifelse(
    data$야간연장근로[idx_night] > 0 & idx_break,
    data$야간연장근로[idx_night] - 1,
    data$야간연장근로[idx_night]
  )
  
  # 야간 수당 (주간)
  start_tmp <- as.POSIXct(
    paste(data[idx_day, ]$근무일자, '22:00:00'), tz = 'KST'
  )
  data$야간수당근로 <- 0
  data$야간수당근로[idx_day] <- difftime(
    data[idx_day, ]$실퇴근시간, start_tmp, units = 'hours'
  )
  data$야간수당근로 <- ifelse(data$야간수당근로 <= 0, 0, data$야간수당근로)
  data$야간수당근로 <- ifelse(data$야간수당근로 > 8, 8, data$야간수당근로) # 10~06가 8시간이므로 8시간 넘으면 그냥 8적용.
  
  # 야간 수당 (야간)
  if (length(data[idx_night, ]$근무일자) >= 1) {
    start_tmp <- as.POSIXct(
      paste(data[idx_night, ]$근무일자, '22:00:00'), tz = 'KST'
    )
    end_tmp <- as.Date(
      paste(
        lubridate::year(data[idx_night, ]$퇴근),
        lubridate::month(data[idx_night, ]$퇴근),
        lubridate::day(data[idx_night, ]$퇴근),
        sep = '-'
      )
    )
    end_tmp <- as.POSIXct(paste(end_tmp, '06:00:00'), tz = 'KST')
    start_tmp <- as.character(start_tmp)
    end_tmp <- as.character(end_tmp)
    
    start_tmp <- ifelse(
      as.character(data[idx_night, ]$실출근시간) >= start_tmp,
      as.character(data[idx_night, ]$실출근시간),
      start_tmp
    )
    end_tmp <- ifelse(
      as.character(data[idx_night, ]$실퇴근시간) <= end_tmp,
      as.character(data[idx_night, ]$실퇴근시간),
      end_tmp
    )
    data$야간수당근로[idx_night] <- 
      as.numeric(difftime(end_tmp, start_tmp, units = 'hours'))  
  }
  
  # 지각 : 기본근로가 8시간 안되는 경우만, 평일만
  data$지각조퇴시간 <- 0
  idx_normal <- data$휴일 == F
  data[idx_day & idx_normal, ]$지각조퇴시간 <- ifelse(
    data[idx_day & idx_normal, ]$주간기본근로 < 8,
    8 - data[idx_day & idx_normal, ]$주간기본근로,
    data[idx_day & idx_normal, ]$지각조퇴시간
  )
  data[idx_day & idx_normal, ]$주간기본근로 <- 
    data[idx_day & idx_normal, ]$주간기본근로 + 
    data[idx_day & idx_normal, ]$지각조퇴시간
  
  data[idx_night & idx_normal, ]$지각조퇴시간 <- ifelse(
    data[idx_night & idx_normal, ]$야간기본근로 < 8,
    8 - data[idx_night & idx_normal, ]$야간기본근로,
    data[idx_night & idx_normal, ]$지각조퇴시간
  )
  data[idx_night & idx_normal, ]$야간기본근로 <- 
    data[idx_night & idx_normal, ]$야간기본근로 + 
    data[idx_night & idx_normal, ]$지각조퇴시간
  
  # 기타
  var_tmp <- c(
    '근로시간', '주간기본근로', '주간연장근로',
    '야간기본근로', '야간연장근로', '야간수당근로', 
    '지각조퇴시간'
  )
  time_test <- apply(
    data[, var_tmp], 1, function(x) any(x <= -1 | x >= 24)
  )
  data$비고 <- ifelse(time_test == T, '시간', data$비고)
  
  var_order <- c(
    '번호','ID','이름','근무일자','근무일명칭','출근_raw','퇴근_raw',
    '출근','퇴근', '요일','야간','휴일','실출근시간','실퇴근시간',
    '근로시간','주간기본근로','주간연장근로','야간기본근로',
    '야간연장근로','야간수당근로','지각조퇴시간','비고'
  )
  data <- data[, var_order]
  
  return(data)
}