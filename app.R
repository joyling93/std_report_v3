options(shiny.maxRequestSize = 500*1024^2)
#library(ggplot2)
source("./bin/report2.R",encoding = 'UTF8')
source('./bin/archive.R',encoding = 'UTF8')
library(shiny)
library(officer)
library(stringr)
library(dplyr)
library(flextable)
library(tidyr)
library(viridis)
library(magick)
library(stringr)
library(lubridate)
library(ggplot2)
library(RSQLite)
temp_dir <- tempdir()
#zipfile <- tempfile(fileext = ".zip")
#fontname <- "Arial"
# Define UI for data upload app ----
ui <- fluidPage(
        
        # App title ----
        title = "Standar Output",
        hr(),
        fluidPage(
                tabsetPanel(
                        tabPanel('归档系统',
                                 br(),
                                 br(),
                                 sidebarPanel(
                                         selectInput('input_type1','选择归档信息种类',
                                                     c('实验信息表')),
                                         fileInput('excel_file',
                                                   label = '上传excel文件',
                                                   multiple = T),
                                         #actionButton('report_download',label = '下载结题报告'),
                                         #hr(),
                                         actionButton('archive',label='点此开始归档'),

                                 ),
                                 mainPanel(
                                         textOutput('Update_info'),
                                         textOutput('file_list'),
                                         hr(),
                                         #textOutput('feedback_info')
                                 )
                        ),
                        tabPanel('统计系统',
                                 br(),
                                 br(),
                                 sidebarPanel(
                                         selectInput('input_type2','选择统计种类',
                                                     c('实验信息表')),
                                         #dateInput('report_date',label = '选择统计日期',value = date('2020-09-01')),
                                         actionButton('statistic',label='统计'),
                                 ),
                                 mainPanel(
                                        shinycssloaders::withSpinner(
                                                DT::DTOutput('report1')
                                        )
                                 )
                        )
                )
                )
        )
                
        
server <- function(input, output) {
        ##文件上传1
        output$Update_info <- renderText({
                "更新说明。"
        })
        ##文件上传列表1
        output$file_list <- renderText({
                input$excel_file$name
                input$excel_file$datapath
        })
        
        ##文件归档
        observeEvent(input$archive, {
                progress <- shiny::Progress$new()
                on.exit(progress$close)
                progress$set('读取文件',value=0.5)
                progress$set('归档中。。。',value=0.75)
                db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
                archive_files(
                        filepath=input$excel_file$datapath,
                        filename=input$excel_file$name,
                        db=db
                        )
                DBI::dbDisconnect(db)
                progress$set('生成完成',value=1)
                
        })

        output$report1 <- DT::renderDT({
                db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
                dt <- dbReadTable(db,'分子信息表')
                DBI::dbDisconnect(db)
                dt
        })
        
}
        
shinyApp(ui, server)      

        