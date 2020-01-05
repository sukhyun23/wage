ui_app <- function() {
  # 2. header ---------------------------------------------------------------
  header <- shinydashboard::dashboardHeader(title = '현장 근로 관리')
  
  
  
  # 3. sidebar --------------------------------------------------------------
  sidebar <-   shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        text = "기본 정보 입력", 
        tabName = "side_basic", 
        icon = shiny::icon('address-book')
      ),
      shinydashboard::menuItem(
        text = "데이터 입력", 
        tabName = "side_data", 
        icon = shiny::icon("file")
      ),
      shinydashboard::menuItem(
        text = "요약", 
        tabName = "side_summary", 
        icon = shiny::icon("list-alt")
      ),
      shinydashboard::menuItem(
        text = "차트", 
        tabName = "side_chart", 
        icon = shiny::icon("bar-chart-o")
      )
    )
  )
  
  
  
  # 4. body -----------------------------------------------------------------
  # 4.1 (side bar) basic contents
  basic_file_input <- shiny::fileInput(
    inputId = "basic_file_input",
    label = "사원 정보 엑셀 파일을 선택 하십시오.",
    multiple = T,
    buttonLabel = '파일 선택',
    placeholder = '선택된 파일이 없습니다.'
    # accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
  )
  s <- pre_date(Sys.Date())
  e <- seq(s, length = 2, by = "months")[2] - 1
  basic_date_input <- shiny::dateRangeInput(
    inputId = "basic_date_input", 
    label = '기간을 입력하세요',
    format = "yyyy-mm-dd",
    language = 'ko', 
    start = s,
    end = e 
  )
  
  side_basic_content <- shinydashboard::tabItem(
    tabName = "side_basic",
    shiny::fluidRow(
      shiny::column(
        width = 4, 
        basic_file_input,
        shiny::tableOutput(outputId = 'basic_table_output')
        # action_basic
      ),
      shiny::column(
        width = 8,
        basic_date_input,
        DT::DTOutput(outputId = 'basic_dt_output'),
        shiny::verbatimTextOutput('tmp_print')
      )
    )
  )
  
  
  # 4.2 (side bar) input contents
  data_file_input <- shiny::fileInput(
    inputId = "data_file_input", 
    label = "계산할 엑셀 파일을 선택 하십시오.",
    multiple = T,
    buttonLabel = '파일 선택',
    placeholder = '선택된 파일이 없습니다.'
    # accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
  )
  
  side_data_content <- shinydashboard::tabItem(
    tabName = "side_data",
    shiny::fluidPage(
      data_file_input,
      shiny::htmlOutput(outputId = 'data_text_output'),
      # textOutput(outputId = 'data_text_output'),
      shiny::br(),
      shiny::column(
        width = 12,
        DT::dataTableOutput(outputId = 'data_dt_output')
      )
    )
  )
  
  # 4.3 (side bar) summary contents
  side_summary_content <- shinydashboard::tabItem(
    tabName = "side_summary",
    shiny::fluidPage(
      shiny::downloadButton(outputId = 'summary_down_button', label = '다운받기'),
      shiny::column(
        width = 12,
        DT::dataTableOutput(outputId = 'summary_dt_output')
      )
    )
  )
  
  # 4.4 (side bar) chart contents
  side_chart_content <- shinydashboard::tabItem(
    tabName = "side_chart",
    shiny::fluidPage(
      shiny::column(
        width = 12,
        shiny::plotOutput(outputId = 'side_chart_plot_1', height = 850)
      )
      # column(
      #   width = 6,
      #   plotOutput(outputId = 'side_chart_plot_2')
      # ),
      # column(
      #   width = 6,
      #   plotOutput(outputId = 'side_chart_plot_3')
      # )
    )
  )
  
  
  # Body content
  body <- shinydashboard::dashboardBody( 
    shinydashboard::tabItems(
      side_basic_content,
      side_data_content, 
      side_summary_content,
      side_chart_content
    )
  )
  
  return(shinydashboard::dashboardPage(header, sidebar, body))
}