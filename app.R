options(shiny.maxRequestSize = 10000*1024^2)

source("./bin/report_generate.R",encoding = 'UTF8')
source('./bin/archive.R',encoding = 'UTF8')
source('./bin/input_test.R',encoding = 'UTF8')
library(shiny)
library(shinydashboard)
library(officer)
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

temp_dir <- tempdir()

user_base <- data.frame(
        user = c("user1", "user2"),
        password = c("pass1", "pass2"), 
        permissions = c("admin", "standard"),
        name = c("User One", "User Two"),
        stringsAsFactors = FALSE,
        row.names = NULL
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
                                                tags$a(href = 'https://www.teambition.com/project/58081fe94863251f4269aaf3/works/5f842098c24dc10044227f64/work/5f84229c003a3500449ba6ae',
                                                       "点此链接下载报告模板", target = "_blank"),
                                                selectInput('vector_fun','选择载体种类',
                                                            c('过表达','干扰','基因编辑')),
                                                selectInput('vector_type','选择业务种类',
                                                            c('载体构建','病毒包装','载体构建和病毒包装')),
                                                fileInput('info_file',
                                                          label = '上传实验信息表',
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
                                                textOutput('info'),
                                                textOutput('Update_info'),
                                                hr(),
                                                uiOutput('pic_upload1'),
                                                uiOutput('upload1_list'),
                                                br(),
                                                uiOutput('pic_upload2'),
                                                uiOutput('upload2_list'),
                                                hr(),
                                                uiOutput('pic_upload3'),
                                                uiOutput('upload3_list'),
                                                hr(),
                                                img(src='tutorial.png',width=580,height=300)
                                        )  
                                )
                        ),
                        
                        tabItem(tabName = '归档系统',
                                fluidRow(
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
                                                #textOutput('Update_info'),
                                                textOutput('file_list'),
                                                hr(),
                                                #textOutput('feedback_info')
                                        )
                                )
                        ),
                        
                        
                        tabItem(tabName = '统计系统',
                                fluidRow(
                                        br(),
                                        br(),
                                        
                                        sidebarPanel(
                                                selectInput('input_type2','选择统计种类',
                                                            c('实验信息表')),
                                                #dateInput('report_date',label = '选择统计日期',value = date('2020-09-01')),
                                                actionButton('statistic',label='统计'),
                                        ),
                                        mainPanel(
                                                # must turn shinyjs on
                                                shinyjs::useShinyjs(),
                                                # add logout button UI 
                                                div(class = "pull-right", logoutUI(id = "logout")),
                                                # add login panel UI function
                                                loginUI(id = "login"),
                                                shinycssloaders::withSpinner(
                                                        DT::DTOutput('report1')
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
                "2020.12.03更新：更换了新的实验记录模板，修复了此前模板有时无法生成报告的bug。"
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
        ##文件上传3 上传质粒和病毒感染图片
        output$pic_upload3 <- renderUI({
                if(is.null(input$vector_type)){
                        return()
                }else{
                        switch(input$vector_type,
                               '病毒包装'=fileInput('pic3',
                                                label = '上传质粒和病毒感染图片',
                                                multiple = T),
                               
                               '载体构建和病毒包装'=fileInput('pic3',
                                                     label = '上传质粒和病毒感染图片',
                                                     multiple = T)
                        )
                }
        })
        ##文件上传列表3 显示上传图片名称
        output$upload3_list <- renderUI({
                if(is.null(input$vector_type)){
                        return()
                }else{
                        print(str_c(input$pic3$name,collapse = '\t'))
                }
        })
        
        ##信息表检查
        
        output$info <- renderText({
                input_test(input$info_file$name,
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
        
        ###统计系统
        observeEvent(input$statistic,{
                output$report1 <- DT::renderDT({
                        req(credentials()$user_auth)
                        db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
                        dt <- dbReadTable(db,'分子信息表')
                        DBI::dbDisconnect(db)
                        dt
                })
        })
        
}
        
shinyApp(ui, server)      

        