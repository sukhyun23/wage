summary_wage <- function(data) {
  for (i in 4:ncol(data)) {
    data[[i]] <- ifelse(is.na(data[[i]]), 0, data[[i]])
  }
  data <- data[!is.na(data$ID), ]
  data <- data[!is.na(data$국적), ]
  data <- data[!is.na(data$시급), ]
  
  # 1. pay
  # 기본급
  data$기본급 <- (data$기본근로시간/data$근무일 * data$시급) * 
    (data$근무일 + data$주휴)
  data$당월연차수당 <- (data$연차 * 8 * data$시급)
  data$기본급 <- data$기본급 - data$당월연차수당
  # 지각
  data$지각조퇴공제액 <- data$지각조퇴 * data$시급
  data$기본급 <- data$기본급 - data$지각조퇴공제액
  
  # 연장 수당
  data$연장수당 <- data$시급 * data$연장근로시간 * 1.5
  
  # 야간 수당
  data$야간수당 <- data$시급 * data$야간수당시간 * 0.5
  
  # 휴일 수당
  data$휴일수당 <- data$시급 * 
    (data$휴일근로시간 * 1.5 + data$휴일연장시간 * 2)
  
  # summation
  data$합계 <- (data$기본급 + data$연장수당 + data$야간수당 + 
                data$당월연차수당 +  data$휴일수당 + data$직책수당 + 
                data$연차수당)
  
  # 2. deduction
  # 공제 
  data$고용보험 <- 0
  data$고용보험[data$국적 == '내국인'] <- 
    data$합계[data$국적 == '내국인'] * 0.008
  data$고용보험 <- floor(data$고용보험)
  data$고용보험 <- data$고용보험 - 
    as.numeric(stringr::str_sub(data$고용보험, -1))
  
  data$공제합계 <- (
    data$고용보험 + data$국민연금 + 
      data$건강보험 + data$건강보험정산 + 
      data$장기요양 + data$장기요양정산 +
      data$기타공제금 + data$소득세정산 +
      data$관리비 + data$식대 + 
      data$소득세 + data$주민세
  )
  data$차인지급액 <- data$합계 - data$공제합계
  
  vars_order <- c(
    'ID', '이름', '시급', '기본근로시간', '근무일', '주휴', '지각조퇴',
    '지각조퇴공제액', '연차', '당월연차수당', '연차수당', '기본급', 
    '연장근로시간',
    '연장수당', '야간근로시간', '야간수당시간', '야간수당', '휴일근로시간', 
    '휴일연장시간',  '휴일수당', '직책수당', '합계', "고용보험", "국민연금",
    "건강보험", '건강보험정산', "장기요양", "장기요양정산", '기타공제금', 
    '소득세정산', "관리비", '식대', "소득세", "주민세", '공제합계', '차인지급액'
  )
  data <- data[, vars_order]
  
  for (i in which(sapply(data, class) == 'numeric')) {
    data[[i]] <- ifelse(is.na(data[[i]]), 0, data[[i]])
  }
  for (i in which(sapply(data, class) == 'numeric')) {
    data[[i]] <- floor(data[[i]])
  }
  
  return(data)
}