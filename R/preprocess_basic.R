preprocess_basic <- function(data, holiday_date) {
  # data <- data_raw
  data <- data.table(data)
  
  # change names
  data <- data[
    , c('번호', 'ID', '이름', '근무일자', '근무일명칭', '출근', '퇴근'), 
    with = F
  ]
  names(data) <- c('no', 'id', '이름', 'date', 'type', 'start', 'end')
  
  # fix words
  data$day <- weekdays(as.Date(data$date))
  data$type[data$type == '평일'] <- '주간'
  
  # night duty
  data$night <- grepl('\\+', data$end)
  data$end <- gsub('\\+', '', data$end)
  
  # holiday
  data$date <- as.Date(data$date)
  data$holiday <- ifelse(
    data$date %in% holiday_date,
    T,
    F
  )
  
  # fix start, end
  data$start <- ifelse(is.na(data$start), '00:00', data$start)
  data$end <- ifelse(is.na(data$end), '00:00', data$end)
  
  data$start <- as.POSIXct(paste(data$date, data$start))
  data$end <- as.POSIXct(paste(data$date, data$end))
  data[data$night == T, ]$end <- data[data$night == T, ]$end + 60*60*24
  
  # remove na 
  data <- data[type != '휴일', ]
  data <- data[!is.na(start) | !is.na(end), ]
  data <- data[start != end, ]
  
  # total work hour 
  data$work_hour <- difftime(data$end, data$start, units = 'hours')
  data$work_hour <- round(data$work_hour) %>% as.numeric()
  
  # day 
  idx_tmp <- day(data$start) >= day(data$end)
  data$base_hour_day <- 0
  data$base_hour_day[idx_tmp] <- ifelse(
    data$work_hour[idx_tmp] >= 8, 8, data$work_hour[idx_tmp]
  )
  
  data$over_hour_day <- 0
  data$over_hour_day[idx_tmp] <- data$work_hour[idx_tmp] - 
    data$base_hour_day[idx_tmp]
  data$over_hour_day[idx_tmp] <- ifelse(
    data$over_hour_day[idx_tmp] <= 0, 
    0, 
    data$over_hour_day[idx_tmp] - 1
  )
  
  # night
  idx_tmp <- day(data$start) < day(data$end)
  end_tmp <- as.POSIXct(data[idx_tmp, ]$date) + 3600*21
  end_tmp <- as.POSIXct(
    ifelse(
      data[idx_tmp, ]$end >= end_tmp,
      end_tmp,
      data[idx_tmp, ]$end
    ),
    origin = '1970-01-01'
  )
  
  start_tmp <- as.POSIXct(data[idx_tmp, ]$date) + 3600*13
  start_tmp <- as.POSIXct(
    ifelse(
      data[idx_tmp, ]$start >= start_tmp,
      data[idx_tmp, ]$start,
      start_tmp
    ),
    origin = '1970-01-01'
  )
  
  data$base_hour_night <- 0
  data[idx_tmp, ]$base_hour_night <- difftime(
    end_tmp,
    start_tmp,
    units = 'hours'
  )
  
  data$base_hour_night <- round(data$base_hour_night)
  data$over_hour_night <- 0
  data$over_hour_night[idx_tmp] <- data$work_hour[idx_tmp] - 
    data$base_hour_night[idx_tmp] - 1
  
  data$over_hour_night <- ifelse(
    data$over_hour_night <= 0, 0, data$over_hour_night
  )
  
  return(data)
}