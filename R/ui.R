# 2. header ---------------------------------------------------------------
header <- dashboardHeader(title = '현장 근로 관리')



# 3. sidebar --------------------------------------------------------------
sidebar <-   dashboardSidebar(
  sidebarMenu(
    menuItem(
      text = "기본 정보 입력", 
      tabName = "side_basic", 
      icon = icon('address-book')
    ),
    menuItem(
      text = "데이터 입력", 
      tabName = "side_data", 
      icon = icon("file")
    ),
    menuItem(
      text = "요약", 
      tabName = "side_summary", 
      icon = icon("list-alt")
    ),
    menuItem(
      text = "차트", 
      tabName = "side_chart", 
      icon = icon("bar-chart-o")
    )
  )
)



# 4. body -----------------------------------------------------------------
# 4.1 (side bar) basic contents
basic_file_input <- fileInput(
  inputId = "basic_file_input",
  label = "사원 정보 엑셀 파일을 선택 하십시오.",
  multiple = T,
  buttonLabel = '파일 선택',
  placeholder = '선택된 파일이 없습니다.'
  # accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
)
s <- pre_date(Sys.Date())
e <- seq(s, length = 2, by = "months")[2] - 1
basic_date_input <- dateRangeInput(
  inputId = "basic_date_input", 
  label = '기간을 입력하세요',
  format = "yyyy-mm-dd",
  language = 'ko', 
  start = s,
  end = e 
)
# action_basic <- actionButton(
#   inputId = "action_basic", 
#   label = "확인"
# )
side_basic_content <- tabItem(
  tabName = "side_basic",
  fluidRow(
    column(
      width = 4, 
      basic_file_input,
      tableOutput(outputId = 'basic_table_output')
      # action_basic
    ),
    column(
      width = 8,
      basic_date_input,
      DTOutput(outputId = 'basic_dt_output'),
      verbatimTextOutput('tmp_print')
    )
  )
)


# 4.2 (side bar) input contents
data_file_input <- fileInput(
  inputId = "data_file_input", 
  label = "계산할 엑셀 파일을 선택 하십시오.",
  multiple = T,
  buttonLabel = '파일 선택',
  placeholder = '선택된 파일이 없습니다.'
  # accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
)
side_data_content <- tabItem(
  tabName = "side_data",
  fluidPage(
    data_file_input,
    htmlOutput(outputId = 'data_text_output'),
    # textOutput(outputId = 'data_text_output'),
    br(),
    column(
      width = 12,
      DT::dataTableOutput(outputId = 'data_dt_output')
    )
  )
)

# 4.3 (side bar) summary contents
side_summary_content <- tabItem(
  tabName = "side_summary",
  fluidPage(
    downloadButton(outputId = 'summary_down_button', label = '다운받기'),
    column(
      width = 12,
      DT::dataTableOutput(outputId = 'summary_dt_output')
    )
  )
)

# 4.4 (side bar) chart contents
side_chart_content <- tabItem(
  tabName = "side_chart",
  fluidPage(
    column(
      width = 12,
      plotOutput(outputId = 'side_chart_plot_1', height = 850)
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
body <- dashboardBody( 
  tabItems(
    side_basic_content,
    side_data_content, 
    side_summary_content,
    side_chart_content
  )
)
