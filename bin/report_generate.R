# file_name <- dir('/Users/zhuomingx/Downloads/fw-765-慢病毒培养基减量测试-lw483/','.xlsx')
# file_path <- dir('/Users/zhuomingx/Downloads/fw-765-慢病毒培养基减量测试-lw483/','.xlsx',full.names = T)
# project1 <- '过表达'
# project2 <- '病毒包装'

report_generate <- function(file_name,file_path,pic_name,pic_path,project1,project2,temp_dir){
        fontname <- "Arial"
        dt <- 
                tibble::tibble(
                        dt_type = c('分子实验记录表','病毒实验记录表','细胞实验记录表')) %>% 
                mutate(data = map(dt_type,function(dt_type){
                        filelist <- file_path[str_which(file_name,dt_type)]
                        map_df(filelist,openxlsx::read.xlsx)
                })
                )
        
        dt2 <- dt$data %>% 
                keep(function(x){length(x)>0}) %>% 
                reduce(left_join)
        
        
        #从主标题中提取id
        id <- str_extract(unique(dt2$生产主任务标题),'fw-\\d+')
        custom <- str_match(unique(dt2$生产主任务标题)[1],'生产主任务-(.*)-.*')[,2]
        
        if(str_detect(project2,'载体')){
                t1 <- 
                        dt2 %>% 
                        select(载体编号,载体描述,质粒规格,质粒数量,抗性) %>% 
                        mutate(抗性=paste0('质粒',抗性)) %>% 
                        rename(规格=质粒规格,数量=质粒数量,载体类型=抗性)
                
                t2 <-  
                        dt2 %>% 
                        select(载体编号,载体描述,菌种,甘油菌规格,甘油菌数量) %>% 
                        tidyr::drop_na() %>% 
                        mutate(菌种=paste0('甘油菌(',菌种,')')) %>% 
                        rename(规格=甘油菌规格,数量=甘油菌数量,载体类型=菌种) 
                
                vector_info <- 
                        dt2 %>% 
                        select(基因信息,序列信息) %>% 
                        tidyr::drop_na() %>% 
                        filter(!基因信息=='/')
                
                primer_seq_ft <- dt2 %>% 
                        select(载体编号,测序引物)%>%
                        tidyr::drop_na() %>% 
                        flextable()%>%
                        add_header_lines("测序引物序列")%>%
                        border_inner_h(border = fp_border(color="black", width = 1),part = 'body' )%>%
                        align(align = 'center',part = 'all')%>%
                        fontsize(size = 7.5,part = 'all')%>%
                        autofit(add_h = 0.2,add_w=0.5)
        }else{
                t1 <- data.frame(载体编号=character(),
                                     载体描述=character(), 
                                     载体类型=character(), 
                                     规格=character(),
                                     数量=double())
                
                t2 <- data.frame(载体编号=character(),
                                     载体描述=character(), 
                                     载体类型=character(), 
                                     规格=character(),
                                     数量=double()) 
        }
        
        
        if(str_detect(project2,'病毒')){
                t3 <- 
                        dt2 %>% 
                        select(载体编号,载体描述,载体类型,病毒类型,
                                   `病毒滴度(10的8次方)`,出库规格,出库数量) %>% 
                        tidyr::drop_na() %>% 
                        mutate(unit=map(病毒类型,function(x){
                                if(str_detect(x,'LV')){
                                        'TU/mL'
                                }else if(str_detect(x,'AAV')){
                                        'vg/mL'
                                }else if(str_detect(x,'AD')){
                                        'ifu/mL'
                                }}),
                               病毒滴度=`病毒滴度(10的8次方)`*1e8,
                               滴度 = paste0(病毒滴度,unit)
                        ) %>% 
                        select(载体编号,载体描述,载体类型,滴度,出库规格,出库数量) %>% 
                        rename(规格=出库规格,数量=出库数量)
        }else{
                t3 <- data.frame(载体编号=character(),
                                     载体描述=character(), 
                                     载体类型=character(), 
                                     滴度=character(),
                                     规格=character(),
                                     数量=double()) 
        }
        
        
        pre_read <- bind_rows(t1,t2,t3) %>% 
                arrange(滴度) 
        # drop_na() %>% 
        # arrange(滴度) 
        pre_read_ft <- pre_read %>% 
                mutate(滴度=map_chr(滴度,~ifelse(is.na(.x),'/',.x))) %>% 
                select(载体编号,载体描述,载体类型,滴度,规格,数量) %>% 
                flextable()%>%
                add_header_lines("产品信息速览表")%>%
                add_header_lines(id)%>%
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
        
        titer_data_ft <- 
                t3 %>% 
                select(载体编号,载体类型,滴度) %>% 
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
        
        my_doc %>%
                cursor_bookmark("contract_num")%>%
                body_add_par(id,style  = 'Subtitle')%>%
                cursor_bookmark("date")%>%
                body_add_par(value = Sys.Date(),style  = 'Subtitle')%>%
                cursor_bookmark("theme")%>%
                body_add_par(value = paste0('项目名称:',project1,project2),style  = 'Subtitle')%>%
                cursor_bookmark("pre_read_ft")%>%
                body_add_flextable(pre_read_ft,align='center')
        
        
        if(project2=='载体构建'){
                ##添加靶序列引物
                my_doc %>%
                        cursor_bookmark('primer_seq') %>%
                        body_add_flextable(value=primer_seq_ft,align='center') %>%
                        cursor_bookmark('seq')
                
                ##添加靶序列
                
                vector_info %>%
                        rename(x=基因信息,y=序列信息)%>%
                        purrr::pwalk(function(x,y){
                                body_add_par(my_doc,x,style = 'Normal')
                                body_add_par(my_doc,y,style = 'seq')}
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
                        body_replace_flextable_at_bkm('titration',value=titer_data_ft)%>%
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
                vector_info %>%
                        rename(x=基因信息,y=序列信息)%>%
                        purrr::pwalk(function(x,y){
                                body_add_par(my_doc,x,style = 'Normal')
                                body_add_par(my_doc,y,style = 'seq')}
                        )
                
                if(project1=='过表达'){
                        #确定报告结果类别

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
        
        dir.create(file.path(temp_dir,id))
        
        print(my_doc,file.path(temp_dir,
                               id,
                               paste(unique(dt2$生产主任务标题),
                                     '结题报告.docx',
                                     sep = '-')
        ))

        
        #生成标签用数据
        label.file <- pre_read%>%
                mutate(订单编号=id,
                           客户姓名=custom,
                           储存条件=if_else(stringr::str_detect(载体类型,'病毒',),
                                        '-80℃','-20℃'
                           )) %>% 
                select(载体编号,载体描述,载体类型,滴度,规格,订单编号,客户姓名,数量,储存条件)
        
        openxlsx::write.xlsx(label.file,
                             file.path(temp_dir,
                                       id,
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
        
        file.copy(manual_path,file.path(temp_dir,
                                        id))
        
        #压缩文件
        zip::zipr(
                zipfile = file.path(temp_dir,
                                    paste0(id,'.zip')), 
                files = file.path(temp_dir,id),
                include_directories = T
        )
        
        return(info)

}