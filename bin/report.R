#raw_data <- read_docx('./debug/病毒实验记录新模板20201012.docx')
#pic_name <- c('载体图谱.png','载体图谱2.png','酶切鉴定结果.png')
#pic_path <- c('./debug/载体图谱.png','./debug/载体图谱2.png','./debug/酶切鉴定结果.png')
#project1 <- '干扰'
#project2 <- '载体构建和病毒包装'
#temp_dir <- '.'
report_generate <- 
function(raw_data,pic_name,pic_path,project1,project2,temp_dir){
        ##word 解析
        fontname <- "Arial"
        pic_insert <-function(my_doc,p2i,bookmark_id){
                index <- str_which(pic_name,p2i)
                index <- sort(index,decreasing = T)
                for(pic in index){
                        test <- image_scale(image_read(pic_path[pic]),'800')
                        width <- image_info(test)$width/220##以word默认分辨率220dpi计算
                        height <- image_info(test)$height/220##以word默认分辨率220dpi计算
                        image_write(test,pic_path[pic])
                        cursor_bookmark(my_doc,bookmark_id)
                        body_add_img(my_doc,src = pic_path[pic], 
                                     width = width, height = height,
                                     pos = 'before',style = "pic_style")
                }
        }
        
        
        ##转换解析格式为数据框
        table_format <- function(x){
                y <- x%>%
                        split(.$cell_id)%>%
                        purrr::reduce(bind_cols)%>%##多表格两两归并
                        select(starts_with('text'))
        }
        
        raw_data <- read_docx(raw_data)
        content <- docx_summary(raw_data)
        
        title_info <- content%>%
                filter(style_name=='heading 1')%>%
                select(text)
        
        table.body <- content%>%
                filter(style_name=='Grid Table Light')%>%
                select(doc_index,cell_id,text)%>%
                split(.$doc_index)%>%
                purrr::map(.f=table_format)
        
        title_info <- table.body[[1]]
        colnames(title_info) <- title_info[1,]
        title_info <- title_info[-1,]
        
        vector_info <- table.body[[2]]        
        colnames(vector_info) <- vector_info[1,]
        vector_info <- vector_info[-1,]
        
        pre_read <- table.body[[3]]
        colnames(pre_read) <- pre_read[1,]
        pre_read <- pre_read[-1,]
        
        ##交付预览表        
        pre_read_ft <- pre_read%>%
                select(-7)%>%
                flextable()%>%
                add_header_lines("产品信息速览表")%>%
                add_header_lines(title_info$合同编号)%>%
                bold(part = 'header')%>%
                border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
                align(align = 'center',part = 'all')%>%
                fontsize(i=1:2,size = 20,part = 'header')%>%
                fontsize(i=3,size = 12,part = 'header')%>%
                fontsize(j=-2,size = 10,part = 'body')%>%
                fontsize(j=1:3,size = 7.5,part = 'body')%>%
                autofit(add_h = 0,add_w = 0)%>%
                height(height = 1,part = 'body')%>%
                height(height = 0.4,part = 'header')%>%
                width(j=1,width = 1.5)%>%
                width(j=2,width = 2.7)%>%
                width(j=4,width = 0.9)%>%
                width(j=6,width = 0.5)%>%
                font(fontname = fontname, part = "all")
        ##引物表
        primer_seq_ft <- vector_info%>%
                select(载体编号,引物序列)%>%
                flextable()%>%
                add_header_lines("测序引物序列")%>%
                border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
                align(align = 'center',part = 'all')%>%
                fontsize(size = 7.5,part = 'all')%>%
                autofit(add_h = 0.2,add_w=0.5)
        #font(fontname = fontname, part = "all")
        ##滴度表
        titer_data_ft <- pre_read%>%
                filter(stringr::str_detect(载体类型,'病毒'))%>%
                select(载体编号,载体类型,滴度)%>%
                flextable()%>%
                add_header_lines(values = '病毒滴度测定结果')%>%
                border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
                align(align = 'center',part = 'all')%>%
                fontsize(size = 10.5,part = 'body')%>%
                fontsize(size = 12,part = 'header')%>%
                bold(part = 'header')%>%
                autofit(add_h = 0.2,add_w=0.2)%>%
                font(fontname = fontname, part = "all")
        
        ##报告生成
        my_doc <- read_docx('./data/vector&vrius_templete.docx')
        my_doc %>%
                cursor_bookmark("contract_num")%>%
                body_add_par(title_info$合同编号,style  = 'Subtitle')%>%
                cursor_bookmark("date")%>%
                body_add_par(value = Sys.Date(),style  = 'Subtitle')%>%
                cursor_bookmark("theme")%>%
                body_add_par(value = paste0('项目名称:',title_info$项目类型2),style  = 'Subtitle')%>%
                cursor_bookmark("pre_read_ft")%>%
                body_add_flextable(pre_read_ft,align='center')
        
        #project1 <- title_info$项目类型1
        #project2 <- title_info$项目类型2
        if(project2=='载体构建'){
                ##添加靶序列引物
                my_doc %>%
                        cursor_bookmark('primer_seq')%>%
                        body_add_flextable(value=primer_seq_ft,align='center')%>%
                        cursor_bookmark('seq')
                ##添加靶序列
                
                tibble::tibble(vector_info[,c(2,3)])%>%
                        rename(x=插入片段信息,y=插入片段序列)%>%
                        purrr::pwalk(function(x,y){
                                body_add_par(my_doc,x,style = 'Normal')
                                body_add_par(my_doc,y,style = 'seq')
                        }
                        )
                
                if(project1=='过表达'){
                        my_doc %>%
                                cursor_reach(keyword = "分子实验数据")%>%
                                body_add_par(value = "分子实验数据文件夹内含有质粒图谱、质粒示意图、测序比对文件及酶切鉴定照片等分子实验原始数据。", style = "chinese_style")%>%
                                cursor_reach(keyword = "病毒实验数据")%>%
                                body_remove()%>%
                                cursor_reach(keyword = "测序比对验证")%>%
                                body_remove()%>%
                                cursor_reach(keyword = "病毒滴度测定")%>%
                                body_remove()
                        pic_insert(my_doc,'酶切鉴定结果','enzyme_cut')
                        pic_insert(my_doc,'载体图谱','vector_map')
                }else{
                        my_doc %>%
                                cursor_reach('基因序列')%>%
                                body_remove()%>%
                                body_add_par('靶序列',style='heading 2',pos = 'before')%>%
                                cursor_reach(keyword = "分子实验数据")%>%
                                body_add_par(value = "分子实验数据文件夹内含有质粒图谱、质粒示意图、测序比对文件及酶切鉴定照片等分子实验原始数据。", style = "chinese_style")%>%
                                cursor_reach(keyword = "病毒实验数据")%>%
                                body_remove()%>%
                                cursor_reach(keyword = "质粒酶切验证")%>%
                                body_remove()%>%
                                cursor_reach(keyword = "病毒滴度测定")%>%
                                body_remove()
                        pic_insert(my_doc,'靶序列测序结果','seq_map')
                        pic_insert(my_doc,'载体图谱','vector_map') 
                }
        }else if(project2=='病毒包装'){
                #picture_list <- list('酶切鉴定结果')
                my_doc %>%
                        cursor_reach('基因序列')%>%
                        body_remove()%>%
                        body_add_par('靶序列',style='heading 2',pos = 'before')%>%
                        cursor_reach(keyword = "^载体构建信息$")%>%
                        body_remove()%>%
                        cursor_reach(keyword = "^载体$")%>%
                        body_remove()%>%
                        cursor_reach(keyword = "测序比对验证")%>%
                        body_remove()%>%
                        body_bookmark('titration')%>%
                        body_add_flextable(value=titer_data_ft)%>%
                        cursor_reach(keyword = "测序引物序列")%>%
                        body_remove()%>%
                        cursor_reach(keyword = "分子实验数据")%>%
                        body_remove()%>%
                        cursor_reach(keyword = "病毒实验数据")%>%
                        body_add_par(value = "病毒实验数据文件夹内含有质粒转染和慢病毒感染的原始图片。（注：质粒转染图片和滴度检测图片命名规则：载体编号-时间-物镜倍数-荧光类型。eg. LW429-48h-4×-G（G:绿光，R:红光，W：白光））"
                                     , style = "chinese_style")
                pic_insert(my_doc,'酶切鉴定结果','enzyme_cut')
                
        }else{
                my_doc %>%
                        cursor_bookmark('primer_seq')%>%
                        body_add_flextable(value=primer_seq_ft,align='center')%>%
                        cursor_bookmark('seq')
                ##添加靶序列
                #test.t <- 
                tibble::tibble(vector_info[,c(2,3)])%>%
                        rename(x=插入片段信息,y=插入片段序列)%>%
                        purrr::pwalk(function(x,y){
                                body_add_par(my_doc,x,style = 'Normal')
                                body_add_par(my_doc,y,style = 'seq')
                        }
                        )
                
                if(project1=='过表达'){
                        #确定报告结果类别
                        #picture_list <- list('载体图谱',"酶切鉴定结果")
                        my_doc %>% cursor_reach(keyword = "测序比对验证")%>%
                                body_remove()
                        pic_insert(my_doc,'酶切鉴定结果','enzyme_cut')
                        pic_insert(my_doc,'载体图谱','vector_map') 
                }else{
                        picture_list <- list('载体图谱',"靶序列测序结果")
                        my_doc %>% 
                                cursor_reach('基因序列')%>%
                                body_remove()%>%
                                body_add_par('靶序列',style='heading 2',pos = 'before')%>%
                                cursor_reach(keyword = "质粒酶切验证")%>%
                                body_remove()
                        pic_insert(my_doc,'载体图谱','vector_map')
                        pic_insert(my_doc,'靶序列测序结果','seq_map')
                }
                my_doc %>%
                        body_replace_flextable_at_bkm('titration',value=titer_data_ft)%>%
                        cursor_reach(keyword = "分子实验数据")%>%
                        body_add_par(value = "分子实验数据文件夹内含有质粒图谱、质粒示意图、测序比对文件及酶切鉴定照片等分子实验原始数据。"
                                     , style = "chinese_style")%>%
                        cursor_reach(keyword = "病毒实验数据")%>%
                        body_add_par(value = "病毒实验数据文件夹内含有质粒转染和慢病毒感染的原始图片。（注：质粒转染图片和滴度检测图片命名规则：载体编号-时间-物镜倍数-荧光类型。eg. LW429-48h-4×-G（G:绿光，R:红光，W：白光））"
                                     , style = "chinese_style")
        }
        
        source('./bin/dir_creator.R')
        ##建立文件夹
        base_dir <- dir_creator(temp_dir = temp_dir,
                                contract_num = title_info$合同编号,
                                vector_list = unique(pre_read$载体编号),
                                project = project2
        )

        ##图片整理
        pic_organize <- 
                function(pic_name,pic_path){
                        if(stringr::str_detect(pic_name,'载体图谱'))
                                file.copy(pic_path,
                                          file.path(base_dir,'分子实验数据','骨架载体',pic_name))
                        else if(str_detect(pic_name,regex('\\dUL-',ignore_case = T))){
                                parts <- str_split(pic_name,'-',simplify = T)
                                dir3 <- str_subset(unique(pre_read$载体编号),parts[1])
                                dir2 <- '病毒感染48h'
                                file.copy(pic_path,
                                          file.path(base_dir,'病毒实验数据',dir2,dir3,pic_name))
                        }else if(str_detect(pic_name,regex('-\\d+X-',ignore_case = T))){
                                parts <- str_split(pic_name,'-',simplify = T)
                                dir3 <- str_subset(unique(pre_read$载体编号),parts[1])
                                dir2 <- str_subset(c('质粒转染48h','质粒转染72h'),regex(parts[2],ignore_case = T))
                                file.copy(pic_path,
                                          file.path(base_dir,'病毒实验数据',dir2,dir3,pic_name)) 
                        }
                        }
        tibble(pic_name=pic_name,
               pic_path=pic_path)%>%
                purrr::pwalk(pic_organize)
        
        #生成报告
        print(my_doc,file.path(temp_dir,
                               title_info$合同编号,
                               paste(title_info$合同编号,
                                      title_info$客户姓名,
                                      title_info$项目类型1,
                                      title_info$项目类型2,
                                      '结题报告.docx',
                                     sep = '-')
                               )
              )
        #生成标签用数据
        label.file <- pre_read%>%
                mutate(订单编号=title_info$合同编号,
                           客户姓名=title_info$客户姓名,
                           储存条件=if_else(stringr::str_detect(载体类型,'病毒',),
                                        '-80℃','-20℃'
                                        ))%>%
                select(1:5,8:9,7,6,10)
        openxlsx::write.xlsx(label.file,
                             file.path(temp_dir,
                                       title_info$合同编号,
                                       '标签打印用（正式报告中须删除）.xlsx')
                             )
        #置入说明书
        if(any(stringr::str_detect(pre_read$载体类型,'慢病毒'))){
                virus_type <- '慢病毒'
        }else if(any(stringr::str_detect(pre_read$载体类型,'腺病毒'))){
                virus_type <- '腺病毒'
        }else if(any(stringr::str_detect(pre_read$载体类型,'腺相关病毒'))){
                virus_type <- '腺相关病毒'
        }else {
                virus_type <- '载体构建'
        }
        
        manual_list <- dir('./data/manual','.pdf',full.names = T)%>%
                stringr::str_subset(paste0(project1,virus_type))
        
        if (project2=='载体构建'){
                manual_path <- manual_list%>%
                        stringr::str_subset('包装',negate = T)
        }else {
                manual_path <- manual_list%>%
                        stringr::str_subset('包装')
        }
        
        #if(length(manual_path)==0){
        #        stop("载体或业务种类选择错误")
        #}
        
        file.copy(manual_path,file.path(temp_dir,
                                        title_info$合同编号))
        
        #压缩文件
        zip::zipr(
                zipfile = file.path(temp_dir,
                                    paste0(title_info$合同编号,'.zip')), 
                files = file.path(temp_dir,title_info$合同编号),
                include_directories = T
        )
}

        













