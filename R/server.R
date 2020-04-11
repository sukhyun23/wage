server <- function(input, output) {
  ## 1. side basic
  #  1.1 reactivity object : basic data 
  ere_basic_data <- shiny::eventReactive(input$basic_file_input, {
    infile <- input$basic_file_input
    
    file_paths <- infile[['datapath']]
    
    is.csv <- function(x) grepl('\\.csv$', x)
    is.xls <- function(x) grepl('\\.xls$|\\.xlsx$', x)
    
    data_list <- list()
    if (is.csv(file_paths[1])) {
      for (i in file_paths) data_list[[i]] <- read.csv(i)
    } else if (is.xls(file_paths[1])) {
      for (i in file_paths) data_list[[i]] <- readxl::read_excel(i)
    } 
    data <- data.table::rbindlist(data_list)
    data <- data.frame(data)
    names(data)[names(data) == 'id'] <- 'ID'
    data <- data[!is.na(data$ID), ]
    data <- data[!is.na(data$국적), ]
    data <- data[!is.na(data$시급), ]
    
    for (i in 1:ncol(data)) {
      data[[i]] <- ifelse(is.na(data[[i]]), 0, data[[i]])
    }
    
    return(data)
  })
  #  1.1 reactivity object : date range
  ere_date <- shiny::eventReactive(input$basic_date_input, {
    return(input$basic_date_input)
  })
  #  1.1 reactivity object : calendar data
  ere_calendar_data <- shiny::eventReactive(input$basic_date_input, { # input$action_basic
    if ((ere_date()[1] - ere_date()[2]) == 0) {
      return(NULL)
    } else {
      return(calendar(ere_date()[1], ere_date()[2]))  
    }
  })
  #  1.2 render : basic data
  output$basic_table_output <- shiny::renderTable({
    data <- ere_basic_data()[, 1:5]
    data[[4]] <- as.character(data[[4]])
    data[[5]] <- as.character(data[[5]])
    
    if (any(is.na(data))) {
      data <- data.frame('')
      data[[1]] <- '누락된 정보가 있습니다.\n사원정보 파일을 확인하세요.'
      names(data) <- ' '
    }
    return(data)
  })
  #  1.2 render : calendar
  output$basic_dt_output <- DT::renderDataTable(
    ere_calendar_data(), 
    server = FALSE, 
    selection = list(target = "cell")
  )
  
  # tmp
  output$tmp_print <- shiny::renderPrint({
    # ere_data()
    # data <- ere_prepro_data()
    # print(preprocess_time(data))
    # print(ere_basic_data())
    # merge(ere_basic_data(), preprocess_time(data), by = c('ID', '이름'), all = T)
    # print(summary_wage(ere_summary_data()))
  })
  
  ## 2. side data
  #  2.1 reactivity object : work data
  ere_data <- shiny::eventReactive(input$data_file_input, {
    infile <- input$data_file_input
    file_paths <- infile[['datapath']]
    
    is.csv <- function(x) grepl('\\.csv$', x)
    is.xls <- function(x) grepl('\\.xls$|\\.xlsx$', x)
    
    data_list <- list()
    if (is.csv(file_paths[1])) {
      for (i in file_paths) {
        data_list[[i]] <- read.csv(i)[,1:7]  
        data_list[[i]] <- data.frame(
          lapply(data_list[[i]], as.character), 
          stringsAsFactors = F
        )
      }
    } else if (is.xls(file_paths[1])) {
      for (i in file_paths) {
        data_list[[i]] <- readxl::read_excel(i)[,1:7]
        data_list[[i]] <- data.frame(
          lapply(data_list[[i]], as.character), 
          stringsAsFactors = F
        )
      }
    }
    
    data <- do.call(rbind, data_list)
    rownames(data) <- NULL
    data <- data.frame(data)
    data <- data[!is.na(data$출근)&!is.na(data$퇴근), ]
    data <- data[!data$ID == '총합', ]
    
    return(data)
  })
  #  2.2 render : error text
  output$data_text_output <- shiny::renderText({
    # data <- ere_data()
    data <- ere_prepro_data()
    
    msg_worktype <- NULL
    msg_time <- NULL
    msg_commutetime <- NULL
    msg_datatype <- NULL
    
    error_worktype <- any(data$비고 == '근무일명칭')
    if (error_worktype) {
      msg_worktype <- '근무일 명칭이 올바르지 않습니다. 근무일 명칭은 "야간", "주간", "평일"만 가능합니다. 근무일 명칭과 출근 시간도 일치 해야합니다.'  
    }
    
    error_time <- any(data$비고 == '시간')
    if (error_time) {
      msg_time <- '근무시간이 24시간을 넘거나 0보다 작습니다. 익일에는 시간 앞에 "+"가 붙어야 합니다.'  
    }
    
    error_commutetime <- any(data$비고 == '출퇴근형식')
    if (error_commutetime) {
      msg_commutetime <- '출근시간에 형식이 맞지 않습니다. 출퇴근시간은 "00:00"의 형태의 텍스트 타입이어야 합니다.'  
    }
    
    col_number <- sum(
      names(data) %in% 
        c('번호', 'ID', '이름', '근무일자', '근무일명칭', '출근', '퇴근')
    )
    if (error_datatype <- col_number < 7) {
      msg_datatype <- '올바른 데이터가 아닙니다. 번호, ID, 이름, 근무일자, 근무일명칭, 출근, 퇴근 총 7개열이 있어야 합니다.'  
    }
    
    msg_normal <- '데이터가 정상적으로 입력 되었습니다.'
    msg_normal <- paste("<font size = 5><font><b>", msg_normal, "</b></font>")
    
    txt_option <- function(x) {
      paste("<font color=\"#FF0000\" size = 5><font><b>", x, "</b></font>")
    }
    
    if (error_commutetime | error_time | error_worktype | error_datatype) {
      msg_error <- paste(
        c(msg_datatype, msg_commutetime, msg_worktype, msg_time),
        collapse = '<br>'
      )
      msg_error <- txt_option(msg_error)
      return(msg_error)
    } else {
      return(msg_normal)
    }
  })
  #  2.2 render : work data
  output$data_dt_output <- DT::renderDataTable({
    # DT::formatStyle(table = DT::datatable(data))
    # data <- ere_data()
    data <- ere_prepro_data()
    
    error_worktype <- any(data$비고 == '근무일명칭')
    error_time <- any(data$비고 == '시간')
    error_commutetime <- any(data$비고 == '출퇴근형식')
    data$is_error <- ifelse(!data$비고 %in% '정상', T, F)
    
    error_order <- factor(
      data$비고, levels = c('출퇴근형식', '근무일명칭', '시간', '정상')
    )
    
    data_order <- order(-data$is_error, as.numeric(error_order), data$근무일자)
    data <- data[data_order, ]
    
    DT::formatStyle(
      table = DT::datatable(
        data[, c(1:10,22,23)],
        options = list(pageLength = 25)
      ),
      columns = 'is_error',
      target = 'row',
      backgroundColor = DT::styleEqual(c(1, 0), c('pink', 'white'))
    )
  })
  #  2.3 etc : download button
  output$data_down_button <- shiny::downloadHandler(
    filename = function() {
      ds <- pre_date(Sys.Date())
      y <- lubridate::year(ds)
      m <- lubridate::month(ds)
      paste(y, '_', m, '_', '근로시간_데이터', '.xls', sep = '')
    },
    content = function(file) {
      writexl::write_xlsx(x = ere_data(), path = file)
      # write.csv(ere_summary_data(), file)
    }
  )
  
  ## 3. side summary
  #  3.1.1 reactivity object : preprocess data
  ere_prepro_data <- shiny::eventReactive(input$data_file_input, {
    # holiday reactivity
    s_mat <- input$basic_dt_output_cells_selected
    re_holiday <- shiny::eventReactive(input$basic_date_input, {
      s_mat <- as.data.frame(input$basic_dt_output_cells_selected)
      if (nrow(s_mat) == 0) {
        return(NA)
      } else {
        s_matrix <- shiny::eventReactive(input$basic_date_input, s_mat)
        ed <- ere_calendar_data()
        holiday <- Map(
          function(x, y) ed[[y]][x],
          x = s_matrix()[[1]], y = s_matrix()[[2]]
        )
        return(unlist(holiday))
      }
    })
    
    data <- ere_data()
    data <- preprocess_basic(data, holiday_date = re_holiday())
    return(data)
  })
  #  3.1.2 render : summary data
  output$summary_dt_output1 <- DT::renderDataTable({
    data <- ere_prepro_data()
    data <- data[order(data$비고), ]
    
    DT::formatStyle(
      table = DT::datatable(
        data,
        options = list(
          pageLength = 10, 
          scrollX = TRUE
        )
      ),
      columns = '비고',
      target = 'row',
      backgroundColor = DT::styleEqual(
        c('입력오류', '정상'), c('pink', 'white')
      )
    )
  })
  #  3.1.3 etc : download button
  output$summary_down_button1 <- shiny::downloadHandler(
    filename = function() {
      ds <- pre_date(Sys.Date())
      y <- lubridate::year(ds)
      m <- lubridate::month(ds)
      paste(y, '_', m, '_', '가공데이터', '.xls', sep = '')
    },
    content = function(file) {
      writexl::write_xlsx(x = ere_prepro_data(), path = file)
    }
  )
  
  #  3.2.1 reactivity object : summary data
  ere_summary_data <- shiny::eventReactive(input$data_file_input, {
    data <- ere_prepro_data()
    data <- preprocess_time(data)
    
    data <- merge(ere_basic_data(), data, by = c('ID', '이름'), all = T)
    result <- summary_wage(data)
    return(result)
  })
  #  3.2.2 render : summary data2
  output$summary_dt_output2 <- DT::renderDataTable({
    data <- ere_summary_data()
    
    table = DT::datatable(
      data = data,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  #  3.2.3 etc : download button2
  output$summary_down_button2 <- shiny::downloadHandler(
    filename = function() {
      ds <- pre_date(Sys.Date())
      y <- lubridate::year(ds)
      m <- lubridate::month(ds)
      paste(y, '_', m, '_', '임금요약', '.xls', sep = '')
    },
    content = function(file) {
      writexl::write_xlsx(x = ere_summary_data(), path = file)
    }
  )
  
  
  ## 4. side chart 
  output$side_chart_plot_1 <- shiny::renderPlot({
    monthly_plot(ere_data())
  })
}