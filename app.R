options(shiny.maxRequestSize = 10000*1024^2)

source("./bin/report_generate.R",encoding = 'UTF8')
source('./bin/archive.R',encoding = 'UTF8')
source('./bin/input_test.R',encoding = 'UTF8')
source('./bin/statistic.R',encoding = 'UTF8')
source('./bin/tb_query.R',encoding = 'UTF8')
source('./bin/utility.R',encoding = 'UTF8')
library(shiny)
library(shinydashboard)
library(officer)
#library(tidyverse)
library(stringr)
library(dplyr)
library(flextable)
library(tidyr)
library(viridis)
library(magick)
library(lubridate)
library(ggplot2)
library(RSQLite)
library(shinyauthr)
library(shinyjs)
library(purrr)
library(webshot)
library(openxlsx)
library(readr)
library(forcats)
library(tibble)


temp_dir <- tempdir()

user_base <- data.frame(
        user = c("user1", "user2"),
        password = c("pass1", "pass2"), 
        permissions = c("admin", "standard"),
        name = c("User One", "User Two"),
        stringsAsFactors = FALSE,
        row.names = NULL
)

DT_options_list <- list(
        paging = F,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'tB',
        keys=TRUE,
        # rownames = FALSE,
        # scrollY = T,
        buttons = c('copy','excel')
)
# Define UI for data upload app ----
ui <- dashboardPage(
        # App title ----
        dashboardHeader(title = "Standar Output"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("结题报告系统", tabName = "结题报告系统", icon = icon("adn")),
                        menuItem("归档系统", tabName = "归档系统", icon = icon("archive")),
                        menuItem("统计系统", tabName = "统计系统", icon = icon("chart-line"))
                )
        ),
        dashboardBody(
                tabItems(
                        tabItem(tabName = '结题报告系统',
                                fluidRow(
                                        br(),
                                        br(),
                                        sidebarPanel(
                                                # tags$a(href = 'https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5f842098c24dc10044227f64/work/5f84229c003a3500449ba6ae',
                                                #        "点此链接下载报告模板", target = "_blank"),
                                                selectInput('vector_fun','选择载体种类',
                                                            c('过表达','干扰','基因编辑')),
                                                selectInput('vector_type','选择业务种类',
                                                            c('载体构建','病毒包装','载体构建和病毒包装')),
                                                fileInput('info_file',
                                                          label = '上传实验记录表',
                                                          multiple = T
                                                ),
                                                hr(),
                                                # actionButton('input_exame',label='信息表完整性检查'),
                                                # hr(),
                                                actionButton('report_generate',label='点此生成报告'),
                                                hr(),
                                                downloadButton('download',label = '点此下载结题报告')
                                        ),
                                        mainPanel(
                                                h3(textOutput('info')),
                                                hr(),
                                                textOutput('Update_info'),
                                                hr(),
                                                uiOutput('pic_upload1'),
                                                uiOutput('upload1_list'),
                                                br(),
                                                uiOutput('pic_upload2'),
                                                uiOutput('upload2_list'),
                                                hr(),
                                                #uiOutput('pic_upload3'),
                                                #uiOutput('upload3_list'),
                                                hr(),
                                                #img(src='tutorial.png',width=580,height=300)
                                        )  
                                )
                        ),
                        
                        tabItem(tabName = '归档系统',
                                fluidRow(
                                        br(),
                                        br(),
                                        sidebarPanel(
                                                actionButton('archive_auto',label='点此开始自动归档'),
                                                hr(),
                                                selectInput('archieve_type','选择归档信息种类',
                                                            c('TB任务','实验记录（信息）表','TB任务2020','企管统计辅助表')),
                                                fileInput('db_file',
                                                          label = '上传excel文件',
                                                          multiple = T),
                                                #actionButton('report_download',label = '下载结题报告'),
                                                #hr(),
                                                actionButton('archive',label='点此开始手动归档'),
                                                
                                        ),
                                        mainPanel(
                                                h3(textOutput('update_status')),
                                                textOutput('file_list'),
                                                hr(),
                                                h3(textOutput('archieve_status')),
                                                hr(),
                                                textOutput('feedback_info')
                                        )
                                )
                        ),
                        
                        
                        tabItem(tabName = '统计系统',
                                fluidRow(
                                        br(),
                                        br(),
                                        
                                        sidebarPanel(
                                                selectInput('selected_db','选择统计种类',
                                                            c('生产任务','销售任务','企管统计','实验记录表')),
                                                dateInput('time_span',label = '选择统计日期',value = today()),
                                                selectInput('period_type','选择统计种类',
                                                            c('周度','月度','年度'),selected='周度'),
                                                selectInput('tag','特殊条件',
                                                            c('无','不筛选特定时间'),selected='无'),
                                                actionButton('statistic',label='统计'),
                                                downloadButton('download_stat',label = '点此下载统计数据'),
                                                hr()
                                                
                                        ),
                                        mainPanel(
                                                # must turn shinyjs on
                                                shinyjs::useShinyjs(),
                                                # add logout button UI 
                                                div(class = "pull-right", logoutUI(id = "logout")),
                                                # add login panel UI function
                                                loginUI(id = "login"),
                                                hr(),
                                                shinycssloaders::withSpinner(
                                                        DT::DTOutput('DT1')
                                                ),
                                                shinycssloaders::withSpinner(
                                                        DT::DTOutput('DT2')
                                                ),
                                                shinycssloaders::withSpinner(
                                                        DT::DTOutput('DT3')
                                                ),
                                                shinycssloaders::withSpinner(
                                                        DT::DTOutput('DT4')
                                                ),
                                                shinycssloaders::withSpinner(
                                                        DT::DTOutput('DT5')
                                                )
                                                
                                        )
                                )
                        )
                )
                
        )
)
        
                        
                       
                
        
server <- function(input, output) {
        # call the logout module with reactive trigger to hide/show
        logout_init <- callModule(shinyauthr::logout, 
                                  id = "logout", 
                                  active = reactive(credentials()$user_auth))
        
        # call login module supplying data frame, user and password cols
        # and reactive trigger
        credentials <- callModule(shinyauthr::login, 
                                  id = "login", 
                                  data = user_base,
                                  user_col = user,
                                  pwd_col = password,
                                  log_out = reactive(logout_init()))
        
        # pulls out the user information returned from login module
        user_data <- reactive({credentials()$info})
        
        
        ##文件上传1
        output$Update_info <- renderText({
                "2021.02.01更新：1、修改了标签打印表格的表头顺序；2、分子实验记录表 载体类型 删除、病毒实验记录表 载体类型 变为多选一。"
        })
        output$pic_upload1 <- renderUI({
                if(is.null(input$vector_fun)){
                        return()
                }else{
                        switch(input$vector_fun,
                               '过表达'=fileInput('pic1',
                                               label = '上传酶切鉴定结果',
                                               multiple = T),
                               '干扰'=fileInput('pic1',
                                              label = '上传靶序列测序结果',
                                              multiple = T),
                               '基因编辑'=fileInput('pic1',
                                                label = '上传靶序列测序结果',
                                                multiple = T)
                        )
                }
        })
        ##文件上传列表1
        output$upload1_list <- renderUI({
                if(is.null(input$vector_fun)){
                        return()
                }else{
                        print(str_c(input$pic1$name,collapse = '\t'))
                }
        })
        ##文件上传2
        output$pic_upload2 <- renderUI({
                if(is.null(input$vector_type)){
                        return()
                }else{
                        switch(input$vector_type,
                               '载体构建'=fileInput('pic2',
                                                label = '上传载体图谱',
                                                multiple = T),
                               
                               '载体构建和病毒包装'=fileInput('pic2',
                                                     label = '上传载体图谱',
                                                     multiple = T)
                        )
                }
        })
        ##文件上传列表2 显示上传文件名
        output$upload2_list <- renderUI({
                if(is.null(input$vector_type)){
                        return()
                }else{
                        print(str_c(input$pic2$name,collapse = '\t'))
                }
        })
        
        ##信息表检查
        
        output$info <- renderText({
                input_test(file_name=input$info_file$name,
                           file_path=input$info_file$datapath,
                           project2=input$vector_type)
        })

        
        ##生成结题报告
        observeEvent(input$report_generate, {
                progress <- shiny::Progress$new()
                on.exit(progress$close)
                progress$set('读取信息表',value=0.5)
                pic_name <- c(input$pic1$name,input$pic2$name,input$pic3$name)
                pic_path <- c(input$pic1$datapath,input$pic2$datapath,input$pic3$datapath)
                
                progress$set('报告生成中。。。',value=0.75)
                report_generate(
                        file_name = input$info_file$name,
                        file_path = input$info_file$datapath,
                        pic_name=pic_name,
                        pic_path=pic_path,
                        project1=input$vector_fun,
                        project2=input$vector_type,
                        temp_dir=temp_dir
                )
                progress$set('生成完成',value=1)
        })
        ##报告下载
        output$download <- downloadHandler(
                filename=function(){
                        y <- paste0('report','.zip')
                },
                content=function(file){
                        zipfile <- dir(temp_dir,'\\.zip',full.names = T)
                        file.copy(zipfile, file)
                        file.remove(zipfile)
                }
        )
        
        
        
        ###归档系统
        ##文件上传列表1
        output$update_status <- renderText({
                db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
                info <- dbGetQuery(db, "SELECT DISTINCT `import.time` FROM db ORDER BY `import.time` DESC")
                dbDisconnect(db)
                paste0('数据库最新更新时间：',as.POSIXct(info[1,1],origin = "1970-01-01"))
        })
        output$file_list <- renderText({
                display.list <- input$db_file$name
                if(length(display.list)>5){
                        display.list <- c(input$db_file$name[1:10],'...')
                }
                paste0('上传文件列表：',str_c(display.list,collapse = '、'))
        })
        
        ##文件归档
        observeEvent(input$archive_auto, {
                output$archieve_status <- renderText({
                        progress <- shiny::Progress$new()
                        on.exit(progress$close)
                        progress$set('归档准备',value=0.25)
                        source('bin/auto_archieve.R')
                        source('bin/auto_archieve2.R')
                        progress$set('新流程归档中。。。',value=0.5)
                        info1 <- auto_archieve()
                        progress$set('2020旧流程归档中。。。',value=0.75)
                        info2 <- auto_archieve2()
                        progress$set('归档完成',value=1)
                        info <- paste(info1,info2,sep='；')
                })
        })
        
        observeEvent(input$archive, {
                output$archieve_status <- renderText({
                        db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
                        status <- archive_files(
                                type=input$archieve_type,
                                filepath=input$db_file$datapath,
                                db=db
                        )
                        DBI::dbDisconnect(db)
                        status
                })
        })
        
        ###统计系统
        observeEvent(input$statistic,{
                if(input$selected_db=='生产任务'){
                        dt <- db_clean('product_sec')
                        output_list <- delay_cal(dt,input$time_span,input$period_type)
                        
                        output$DT1 <-  DT::renderDT({
                                #req(credentials()$user_auth)
                                output_list[[1]]
                        },
                        extensions = c('Buttons','Responsive','KeyTable'),
                        options = DT_options_list)
                        
                        output$DT2 <-  DT::renderDT({
                                #req(credentials()$user_auth)
                                output_list[[2]]
                        },
                        extensions = c('Buttons','Responsive','KeyTable'),
                        options = DT_options_list)
                        
                        output$DT3 <-  DT::renderDT({
                                #req(credentials()$user_auth)
                                output_list[[3]]
                        },
                        extensions = c('Buttons','Responsive','KeyTable'),
                        options = DT_options_list)
                        
                        output$download_stat <- downloadHandler(
                                filename=function(){
                                        y <- paste0('统计数据.xlsx')
                                },
                                content=function(file){
                                        write.xlsx(output_list[[4]], file)
                                }
                        ) 
                        

                }else if(input$selected_db=='销售任务'){
                        dt <- db_clean('seal_sec')
                        output_list <- seal_cal(dt,input$time_span,input$period_type,input$tag)
                        output$DT1 <-  DT::renderDT({
                                #req(credentials()$user_auth)
                                output_list[[1]][1:10,]
                        },
                        extensions = c('Buttons','Responsive','KeyTable'),
                        options = DT_options_list)
                        
                        output$download_stat <- downloadHandler(
                                filename=function(){
                                        y <- paste0('统计数据.xlsx')
                                },
                                content=function(file){
                                        write.xlsx(output_list[[1]], file)
                                }
                        )
                        
                        
                }else if(input$selected_db=='企管统计'){
                        dt <- db_clean('management_sec')
                        output_list <- management_data_cal(dt,input$time_span,input$period_type,input$tag)
                        output$DT1 <-  DT::renderDT({
                                #req(credentials()$user_auth)
                                output_list[[1]]
                        },
                        extensions = c('Buttons','Responsive','KeyTable'),
                        options = DT_options_list)
                        
                        output$download_stat <- downloadHandler(
                                filename=function(){
                                        y <- paste0('统计数据.xlsx')
                                },
                                content=function(file){
                                        write.xlsx(output_list[[2]], file)
                                }
                        )
                                
                }else if(input$selected_db=='实验记录表'){
                        dt <- db_clean('exp_info')
                        output$DT1 <-  DT::renderDT({
                                #req(credentials()$user_auth)
                                dt %>% 
                                        slice_head(n=5)
                        },
                        extensions = c('Buttons','Responsive','KeyTable'),
                        options = DT_options_list)
                        
                        output$download_stat <- downloadHandler(
                                filename=function(){
                                        y <- paste0('统计数据.xlsx')
                                },
                                content=function(file){
                                        write.xlsx(dt, file)
                                }
                        )
                }else{
                        dt <- db_clean('record_db')
                }
                
                
        })
        
        
}
        
shinyApp(ui, server)      

        