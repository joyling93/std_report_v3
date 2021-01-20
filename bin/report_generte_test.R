library(stringr)
library(officer)
library(flextable)
library(dplyr)

getwd()
setwd('debug/模板文件')
file_name <- dir(all.files=F)[c(7)]
file_path <- dir(all.files=F,full.names = T)[c(7)]
project1 <- '过表达'
project2 <- '病毒包装'
fontname <- "Arial"
pic_name <- dir('.','.png')
pic_path <- dir('.','.png',full.names = T)
id <- str_extract(unique(dt2$生产主任务标题),'fw-\\d+')
custom <- '老王'

dt <- 
        tibble::tibble(
                dt_type = c('分子信息表','病毒信息表','细胞信息表')) %>% 
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

if(str_detect(project2,'载体')){
        t1 <- 
                dt2 %>% 
                select(载体编号,载体描述,质粒规格,质粒数量,抗性) %>% 
                mutate(抗性=paste0('质粒',抗性)) %>% 
                rename(规格=质粒规格,数量=质粒数量,载体类型=抗性)
        
        
        t2 <-  dt2 %>% 
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
                             数量=character())
        
        t2 <- data.frame(载体编号=character(),
                             载体描述=character(), 
                             载体类型=character(), 
                             
                             规格=character(),
                             数量=character()) 
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

# else if(project2=='病毒包装'){
#         t3 <- 
#                 dt2 %>% 
#                 select(载体编号,载体描述,载体类型,病毒类型,
#                            `病毒滴度(10的8次方)`,出库规格,出库数量) %>% 
#                 tidyr::drop_na() %>% 
#                 mutate(unit=map(病毒类型,function(x){
#                         if(str_detect(x,'LV')){
#                                 'TU/mL'
#                         }else if(str_detect(x,'AAV')){
#                                 'vg/mL'
#                         }else if(str_detect(x,'AD')){
#                                 'ifu/mL'
#                         }}),
#                        病毒滴度=`病毒滴度(10的8次方)`*1e8,
#                        滴度 = paste0(病毒滴度,unit)
#                 ) %>% 
#                 select(载体编号,滴度,出库规格,出库数量) %>% 
#                 rename(规格=出库规格,数量=出库数量)
# }
}else{
        t3 <- data.frame(载体编号=character(),
                             载体描述=character(), 
                             载体类型=character(), 
                             滴度=character(),
                             规格=character(),
                             数量=character()) 
}
print('test')

pre_read <- bind_rows(t1,t2,t3) %>% 
        arrange(滴度) 
# drop_na() %>% 
# arrange(滴度) 
pre_read_ft <- pre_read %>% 
        mutate(滴度=map_chr(滴度,~ifelse(is.na(.x),'/',.x))) %>% 
        select(载体编号,载体描述,载体类型,滴度,规格,数量) %>% 
        flextable()%>%
        add_header_lines("产品信息速览表")%>%
        add_header_lines('fw-43')%>%
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







