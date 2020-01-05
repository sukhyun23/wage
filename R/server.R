# 5. server ---------------------------------------------------------------
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
    ere_basic_data()[, 1:5]
  })
  #  1.2 render : calendar
  output$basic_dt_output <- DT::renderDataTable(
    ere_calendar_data(), 
    server = FALSE, 
    selection = list(target = "cell")
  )
  
  # tmp
  output$tmp_print <- shiny::renderPrint({
    # print(ere_date())
    # print(class(ere_calendar_data()))
    # print(input$basic_dt_output_cells_selected)
    # print(ere_basic_data())
    # print(re_holiday())
    print(ere_data()[holiday == T, ])
  })
  # 1. 
  # observeEvent(input$action_basic, {
  #   showNotification(
  #     '정보가 정상적으로 반영되었습니다.', 
  #     type = 'message'
  #   )
  # })
  
  
  ## 2. side data
  #  2.1 reactivity object : work data
  ere_data <- shiny::eventReactive(input$data_file_input, {
    infile <- input$data_file_input
    
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
    
    s_mat <- input$basic_dt_output_cells_selected
    re_holiday <- shiny::eventReactive(input$basic_date_input, {
      if (nrow(s_mat) == 0) {
        return(NA)
      } else {
        holiday <- c()
        for (i in 1:nrow(s_mat)) {
          holiday[i] <- ere_calendar_data()[s_mat[i, 1], s_mat[i, 2]]
        }
        holiday <- as.Date(holiday, origin = '1970-01-01')
        return(holiday)
      }
    })
    
    data_pr <- preprocess_basic(data, re_holiday()) # re_holiday()
    return(data_pr)
  })
  #  2.2 render : error text
  output$data_text_output <- shiny::renderText({
    result <- ''
    data <- ere_data()
    if (any(data$work_hour < 0)) { #  | any(data$work_hour > 20)
      result <- '입력에 문제가 있습니다. 파일을 확인 하세요.'
      result <- paste("<font color=\"#FF0000\" size = 5><font><b>", result, "</b></font>")
    } else if (!any(data$work_hour < 0)) {
      result <- '데이터가 성공적으로 입력 되었습니다.'
      result <- paste("<font size = 5><font><b>", result, "</b></font>")
    }
    result
  })
  #  2.2 render : work data
  output$data_dt_output <- DT::renderDataTable({
    data <- ere_data()
    data$normal <- data$work_hour >= 0 # & data$work_hour <= 20
    data <- data[order(data$normal, data$date), ]
    
    DT::formatStyle(
      table = DT::datatable(
        data[
          , 
          c(
            'id', '이름', 'date', 'type', 'start',
            'end', 'day', 'work_hour', 'normal'
          ),
          with = F
        ],
        options = list(pageLength = 25)
      ),
      columns = 'normal',
      target = 'row',
      backgroundColor = DT::styleEqual(c(1, 0), c('white', 'pink'))
    )
  })
  
  
  ## 3. side summary
  #  3.1 reactivity object : summary data
  ere_summary_data <- shiny::eventReactive(input$data_file_input, {
    data <- ere_data()
    data <- preprocess_time(data)
    data <- merge(ere_basic_data(), data, by = c('id', '이름'), all = T)
    # data <- ere_basic_data()[data, on = c('id', '이름'), nomatch = 0]
  
    summary_data <- summary_wage(data)
    return(summary_data)
  })
  
  #  3.2 render : summary data
  output$summary_dt_output <- DT::renderDataTable({
    data <- ere_summary_data()
    return(data)
  },
  options = list(
    scrollX = TRUE, 
    pageLength = 35,
    autoWidth = TRUE,
    columnDefs = list(list(width = '85', targets = '_all'))
  )
  )
  #  3.3 etc : download button
  output$summary_down_button <- shiny::downloadHandler(
    filename = function() {
      y <- lubridate::year(Sys.Date())
      m <- lubridate::month(Sys.Date())
      paste(y, '_', m, '임금요약', '.xls', sep = '')
    },
    content = function(file) {
      writexl::write_xlsx(x = ere_summary_data(), path = file)
      # write.csv(ere_summary_data(), file)
    }
  )
  
  ## 4. side chart 
  output$side_chart_plot_1 <- shiny::renderPlot({
    monthly_plot(ere_data())
  })
  # output$side_chart_plot_2 <- renderPlot({
  #   gdat <- ere_summary_data()
  #   ggplot(gdat, aes(x = id, y = sum)) + geom_bar(stat = 'identity', fill = 'red')
  # })
  # output$side_chart_plot_3 <- renderPlot({
  #   gdat <- ere_summary_data()
  #   ggplot(gdat, aes(x = id, y = sum)) + geom_bar(stat = 'identity', fill = 'blue')
  # })
}