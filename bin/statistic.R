# library(RSQLite)
#  dt <- db_clean('product_db') %>% filter(是否是子任务=='Y') 
# dt2 <- dt %>% 
#         slice_head(n=5) %>% 
#         mutate(
#                 CD.组成产能=if_else(CD.产能类型=='基因合成载体','3200|500',CD.组成产能),
#                 CD.产能类型=if_else(CD.产能类型=='基因合成载体','慢病毒|基因合成载体',CD.产能类型)
#                    ) %>% 
#         select(CD.产能类型,CD.组成产能)
# 
# dt3 <- 
# dt2 %>% 
#         mutate(CD.产能类型 = strsplit(CD.产能类型,split="|", fixed=TRUE),
#                CD.组成产能 = strsplit(CD.组成产能,split="|", fixed=TRUE) 
#                )%>% 
#         tidyr::unnest() 

# 数据去重和日期处理
db_clean <- function(db_type){
        db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
        dt <- dbReadTable(db,db_type)
        DBI::dbDisconnect(db)
        dt <- dt %>% 
                arrange(desc(import_time)) %>% 
                filter(!duplicated(任务ID)) %>% 
                mutate(across(matches('时间|日期'),ymd_hm),
                       across(contains('姓名'),as.factor)
                       )
}


# 计算开始日期x到结束日期经历的工作日
workday_cal <- function(x,y){
        sum(wday(x+days(1:ceiling((y-x)/ddays(1))))%in%c(1:5))  
}

# 计算生产部门延期率，产能
delay_cal <- function(dt,time_span,period_type){
        time_filter <- switch(period_type,
                              '周度' = function(x){
                                      as.character(cut(x,'week'))
                              },
                              '月度' = month,
                              '年度' = year)

        dt <-
                dt %>%
                filter(是否是子任务=='Y') %>%
                mutate(
                        统计周期 = time_filter(Su.实验实际完成日期),
                        CD.组成产能 = as.character(CD.组成产能),
                        CD.产能类型 = strsplit(CD.产能类型,split="|", fixed=TRUE),
                        CD.组成产能 = strsplit(CD.组成产能,split="|", fixed=TRUE),
                        workday = unlist(purrr::map2(Su.实验实际开始日期,Su.实验实际完成日期,workday_cal)),
                        delay_ratio = (`D.任务周期.工作日.`-workday)/`D.任务周期.工作日.`,
                        project_delay = if_else(delay_ratio>0,0,1),
                        distribution_delay = if_else((开始时间-Su.实验实际开始日期)/ddays(1)>1,1,0)
                ) %>%
                filter(统计周期==time_filter(time_span))
        
        dt1 <- 
                dt %>%
                group_by(CE.实验执行人姓名,统计周期) %>%
                summarise(延期度 = round(sum(delay_ratio),digits = 1),
                             延期率 = round(sum(project_delay)/n(),digits = 1),
                             产能 = sum(CD.子产能),
                             平均产能 = round(产能/n(),digits = 1)
                             ) 
        
        dt2 <- 
                dt %>%
                group_by(CD.子任务类型,统计周期) %>%
                summarise(延期度 = round(sum(delay_ratio),digits = 1),
                             延期率 = round(sum(project_delay)/n(),digits = 1),
                             产能 = sum(CD.子产能),
                             平均产能 = round(产能/n(),digits = 1)
                ) 
        
        dt3 <- 
                dt %>% 
                select(CD.产能类型,CD.组成产能,统计周期) %>% 
                tidyr::unnest() %>% 
                mutate(CD.组成产能=as.numeric(CD.组成产能)) %>% 
                group_by(CD.产能类型,统计周期) %>% 
                summarise(产能=sum(CD.组成产能))
                
        dt4 <-         
                dt %>%
                group_by(Su.实验分配人姓名,统计周期) %>%
                summarise(
                        任务派发延期率 = round(sum(distribution_delay)/n(),digits = 1)
                )
        
        return(list(dt1,dt2,dt3,dt4))
}



