# library(dplyr)
# file_name <- '【谢卓明 的项目】任务信息表_20201208 (6).csv'
# raw_data <- readr::read_csv(file.path('./debug',file_name))
        
report_generate <- 
        function(raw_data,pic_name,pic_path,project1,project2,temp_dir){
                raw_data <- readr::read_csv(raw_data)
                
                dt <- tidyr::fill(data=raw_data,10:24,.direction=c("downup"))
                t2 <- 
                        dt[1,] %>% 
                        mutate(across(7:9,~strsplit(.x,split = '[\f\n\r\t\v]'))) %>% 
                        tidyr::unnest() %>% 
                        filter(载体描述!='')
                
                
                title_info <- 
                        t2 %>% 
                        select(客户姓名,合同编号) %>% 
                        distinct()
                
                vector_info <- 
                        t2 %>% 
                        select("载体编号","载体描述","载体类型")
                
                pre_read <- 
                        vector_info
                pre_read_ft <- 
                        vector_info %>% 
                        flextable() %>% 
                        autofit()
                
                
                my_doc <- read_docx('./data/vector&vrius_templete.docx')
                my_doc %>%
                        cursor_bookmark("contract_num")%>%
                        body_add_par(title_info$合同编号,style  = 'Subtitle')%>%
                        cursor_bookmark("date")%>%
                        body_add_par(value = Sys.Date(),style  = 'Subtitle')%>%
                        cursor_bookmark("theme")%>%
                        body_add_par(value = paste0('项目名称:',title_info$客户姓名),style  = 'Subtitle')%>%
                        cursor_bookmark("pre_read_ft")%>%
                        body_add_flextable(pre_read_ft,align='center')
                
                #source('./dir_creator.R')
                source('./bin/dir_creator.R')
                ##建立文件夹
                base_dir <- dir_creator(temp_dir = temp_dir,
                                        contract_num = title_info$合同编号,
                                        vector_list = unique(pre_read$载体编号),
                                        project = project2
                )
                
                print(my_doc,
                      file.path(temp_dir,
                                       title_info$合同编号,
                                       paste(title_info$合同编号,
                                             title_info$客户姓名,
                                             '结题报告.docx',
                                             sep = '-')
                )
                )
                
                #压缩文件
                zip::zipr(
                        zipfile = file.path(temp_dir,
                                            paste0(title_info$合同编号,'.zip')), 
                        files = file.path(temp_dir,title_info$合同编号),
                        include_directories = T
                )
        }







