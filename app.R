options(shiny.maxRequestSize = 500*1024^2)
#library(ggplot2)
source("./bin/report2.R",encoding = 'UTF8')
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
                        tabPanel('结题报告系统',
                                 br(),
                                 br(),
                                 sidebarPanel(
                                         selectInput('input_type1','选择载体种类',
                                                     c('过表达','干扰','基因编辑')),
                                         selectInput('input_type2','选择业务种类',
                                                     c('载体构建','病毒包装','载体构建和病毒包装')),
                                         fileInput('word_file',
                                                   label = '上传excel文件'),
                                         #actionButton('report_download',label = '下载结题报告'),
                                         #hr(),
                                         actionButton('report_generate',label='点此生成报告'),
                                         hr(),
                                         downloadButton('download',label = '点此下载结题报告')
                                 ),
                                 mainPanel(
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
                                         #img(src='tutorial.png',width=580,height=300)
                                         #textOutput('value1')
                                 )
                        ),
                        tabPanel('统计系统',
                                 br(),
                                 br(),
                                 sidebarPanel(
                                         fileInput('excel_file',
                                                   label = '上传excel文件'),
                                         dateInput('report_date',label = '选择统计日期',value = date('2020-09-01')),
                                         actionButton('statistic',label='统计'),
                                 ),
                                 mainPanel(
                                        shinycssloaders::withSpinner(
                                                DT::DTOutput('report1')
                                        ),
                                        br(),
                                        br(),
                                        hr(),
                                        DT::DTOutput('report2'),
                                        br(),
                                        br(),
                                        hr(),
                                        DT::DTOutput('report3'),
                                        br(),
                                        br(),
                                        hr(),
                                        plotOutput('report4')
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
        output$pic_upload1 <- renderUI({
                if(is.null(input$input_type1)){
                        return()
                }else{
                        switch(input$input_type1,
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
                if(is.null(input$input_type1)){
                        return()
                }else{
                        print(str_c(input$pic1$name,collapse = '\t'))
                }
        })
        ##文件上传2
        output$pic_upload2 <- renderUI({
                if(is.null(input$input_type2)){
                        return()
                }else{
                        switch(input$input_type2,
                               '载体构建'=fileInput('pic2',
                                               label = '上传载体图谱',
                                               multiple = T),
                               
                               '载体构建和病毒包装'=fileInput('pic2',
                                                label = '上传载体图谱',
                                                multiple = T)
                        )
                }
        })
        ##文件上传列表2
        output$upload2_list <- renderUI({
                if(is.null(input$input_type2)){
                        return()
                }else{
                        print(str_c(input$pic2$name,collapse = '\t'))
                }
        })
        ##文件上传3
        output$pic_upload3 <- renderUI({
                if(is.null(input$input_type2)){
                        return()
                }else{
                        switch(input$input_type2,
                               '病毒包装'=fileInput('pic3',
                                                label = '上传质粒和病毒感染图片',
                                                multiple = T),
                               
                               '载体构建和病毒包装'=fileInput('pic3',
                                                     label = '上传质粒和病毒感染图片',
                                                     multiple = T)
                        )
                }
        })
        ##文件上传列表3
        output$upload3_list <- renderUI({
                if(is.null(input$input_type2)){
                        return()
                }else{
                        print(str_c(input$pic3$name,collapse = '\t'))
                }
        })
        ##文件下载处理
        observeEvent(input$report_generate, {
                progress <- shiny::Progress$new()
                on.exit(progress$close)
                progress$set('读取word文件',value=0.5)
                pic_name <- c(input$pic1$name,input$pic2$name,input$pic3$name)
                pic_path <- c(input$pic1$datapath,input$pic2$datapath,input$pic3$datapath)
                
                progress$set('报告生成中。。。',value=0.75)
                report_generate(
                        raw_data=input$word_file$datapath,
                        pic_name=pic_name,
                        pic_path=pic_path,
                        project1=input$input_type1,
                        project2=input$input_type2,
                        temp_dir=temp_dir
                        )
                
                progress$set('生成完成',value=1)
                

        })
        ##报告下载
        output$download <- downloadHandler(
                filename=function(){
                        y <- paste0(input$word_file$name,'.zip')
                },
                content=function(file){
                        zipfile <- dir(temp_dir,'\\.zip',full.names = T)
                        file.copy(zipfile, file)
                        file.remove(zipfile)
                }
        )
        
        data_dt <- eventReactive(input$statistic,{
                #'tb_dashboard/副本签单回款.xlsx'
                source('bin/statistic.R')
                tidy_data(input$excel_file$datapath,input$report_date)
        })
        
        output$report1 <- DT::renderDT({
                data_dt()[[1]]
        })
        
        output$report2 <- DT::renderDT({
                data_dt()[[2]]
        })
        
        output$report3 <- DT::renderDT({
                data_dt()[[3]]
        })
        
        output$report4 <- renderPlot({
                ggplot(data_dt()[[4]],aes(病毒完成人,病毒滴度))+
                        geom_boxplot()+
                        theme(text=element_text(family="Songti SC"))+
                        labs(title = '包装慢病毒滴度分布')
        })
}
        
shinyApp(ui, server)      

        