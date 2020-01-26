# devtools::install_github('sukhyun23/wage')
# 
# library(shiny)
# library(shinydashboard)
# library(ggplot2)
# library(plotly)
# library(DT)
# library(reshape2)
# library(lubridate)
# library(data.table)
# 
# shinyApp(wage::ui_app(), server)
# 
# library(shinydashboard)
# 
# 
# 
# library(dplyr)
# shiny::shinyApp(ui_app(), server)
# 
# 
# 
# 
# setwd('/home/sukhyun/wage_summary_shiny/m12')
# fnames <- list.files()
# fnames <- fnames[grepl('^[0-9]{1,}.xls', fnames)]
# # fnames <- fnames[-29]
# 
# # file load
# file_list <- list()
# for (i in fnames) {
#   file_list[[i]] <- data.table(readxl::read_xls(i))
# }
# names(file_list) <- paste('data_', gsub('.xls', '', names(file_list)), sep = '')
# data <- rbindlist(file_list)
# data_raw <- data
# 
# # prepro
# data <- data_raw
# data <- data.frame(data)
# holiday_date <- as.Date('2019-12-01')
# holiday_date <- seq(holiday_date, as.Date('2019-12-31'), by = 7)
# holiday_date <- c(
#   holiday_date, 
#   as.Date(c('2019-12-07', '2019-12-14', '2019-12-21', '2019-12-28'))
# )
# holiday_date <- holiday_date[order(holiday_date)]
# 
# data <- preprocess_basic(data, holiday_date)
# 
# # montly_plot(data)
# data <- preprocess_time(data)
# 
# # worker_info <- readxl::read_xlsx('/home/sukhyun/wage_summary_shiny/worker_info.xlsx')
# # data <- merge(worker_info, data, by = c('id', '이름'), all = T)
# 
# summary_wage(data)
# ###
# 