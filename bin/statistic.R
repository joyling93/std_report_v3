# period_type <- '月度'
# time_span <- today()-ddays(60)
# db_type <- 'product_sec'
# DBI::dbDisconnect(db)
# load('debug/test/test_env.Rds')
# save.image('debug/test/test_env.Rds')

# 数据去重和日期处理
db_clean <- function(db_type){
        db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
        dt <- dbReadTable(db,'db')
        DBI::dbDisconnect(db)
        col_clean <- 
                colnames(dt) %>% 
                str_replace_all('^X\\.','') %>% 
                str_replace_all('\\.$','')
        colnames(dt) <- col_clean
        if(db_type=='product_sec'){
                dt_extra <- dt %>% 
                        dplyr::filter(任务类型=='销售序列模板') %>% 
                        select(任务ID,A.方案设计者,A.方案指派日期,S.合同金额,S.消费金额) %>% 
                        rename(主任务ID=任务ID) %>% 
                        mutate(主任务ID=tolower(主任务ID))
                        
                dt2 <- dt %>% 
                        dplyr::filter(任务类型=='生产序列模板') %>% 
                        select(-c(A.方案设计者,A.方案指派日期,S.合同金额,S.消费金额)) %>% 
                        mutate(
                                主任务ID = unlist(map(标题,
                                                    ~tolower(str_extract(.x,regex('fw-\\d+|DS-\\d+', ignore_case = T)))))
                                  ) %>% 
                        left_join(dt_extra) %>% 
                        # arrange(desc(import.time)) %>% 
                        # dplyr::filter(!duplicated(任务ID)) %>% 
                        mutate(across(matches('时间|日期'),~as_date(ymd_hms(.x))),
                               across(contains('姓名'),as.factor),
                               across(ends_with('产能'),as.numeric),
                               across(contains('周期'),as.numeric)
                        )
        }else if(db_type=='seal_sec'){
                dt_extra <- dt %>% 
                        dplyr::filter(任务类型=='生产序列模板',是否是子任务=='Y') %>% 
                        mutate(主任务ID = unlist(map(标题,
                                                    ~tolower(str_extract(.x,regex('fw-\\d+', ignore_case = T))))),
                                  CD.子任务类型 = if_else(CD.子任务类型=='','字段未填',CD.子任务类型),
                                  CE.实验执行人姓名 = if_else(CE.实验执行人姓名=='','字段未填',CE.实验执行人姓名)) %>% 
                        select(主任务ID,CE.实验执行人姓名,CD.子任务类型,CD.子产能) %>% 
                        group_by(主任务ID) %>% 
                        mutate(CE.实验执行人姓名 = str_c(CE.实验执行人姓名,collapse = ';')) %>% 
                        drop_na() %>% 
                        group_by(主任务ID,CE.实验执行人姓名,CD.子任务类型) %>% 
                        summarise(CD.子产能=sum(as.numeric(CD.子产能))) %>% 
                        pivot_wider(names_from = CD.子任务类型,values_from=CD.子产能) %>% 
                        rename(任务ID=主任务ID,生产执行人=CE.实验执行人姓名)
                
                dt2 <- dt %>% 
                        dplyr::filter(任务类型=='销售序列模板') %>% 
                        left_join(dt_extra) %>% 
                        mutate(
                                across(matches('时间|日期'),ymd_hms),
                                across(contains('姓名'),as.factor),
                                across(ends_with('产能'),as.numeric),
                                across(contains('周期'),as.numeric)
                               )
        }else if(db_type=='exp_info'){
                db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
                dt <- dbReadTable(db,'exp_info')
                DBI::dbDisconnect(db)
                col_clean <- 
                        colnames(dt) %>% 
                        str_replace_all('^X\\.','') %>% 
                        str_replace_all('\\.$','')
                dt2 <- dt
        }
        else{
                dt2 <- dt %>% 
                        arrange(desc(import.time)) %>% 
                        dplyr::filter(!duplicated(任务ID)) %>% 
                        mutate(across(matches('时间|日期'),ymd_hms),
                               across(contains('姓名'),as.factor),
                        )
        }
        
        return(dt2)
}


# 计算开始日期x到结束日期经历的工作日
workday_cal <- function(x,y){
        sum(wday(x+days(1:ceiling((y-x)/ddays(1))))%in%c(2:6))
}

# 计算生产部门延期率，产能
delay_cal <- function(dt,time_span,period_type){
        time_filter <- switch(period_type,
                              '周度' = function(x){
                                      ymd(cut(x+ddays(2),'week',start.on.monday=F))-ddays(2)
                                      #延后实际日期使周数从周五开始
                                      },
                              '月度' = month,
                              '年度' = year)
        
        dt_capacity <-
                #test <- 
                dt %>%
                dplyr::filter(是否是子任务=='Y') %>% 
                drop_na(开始时间,截止时间,Su.实验实际开始日期,Su.实验实际完成日期) %>% 
                mutate(统计周期 = time_filter(Su.实验实际完成日期)) %>% 
                dplyr::filter(year(Su.实验实际完成日期)==year(time_span),
                              统计周期==time_filter(time_span)) %>% 
                mutate(
                        预期周期 = map2_dbl(开始时间,截止时间,workday_cal),
                        CD.组成产能 = as.character(CD.组成产能),
                        CD.产能类型 = strsplit(CD.产能类型,split="|", fixed=TRUE),
                        CD.组成产能 = strsplit(CD.组成产能,split="|", fixed=TRUE),
                        #S.消费金额 = strsplit(S.消费金额,split='[[:punct:]]'),
                        实际周期 = map2_dbl(Su.实验实际开始日期,Su.实验实际完成日期,workday_cal),
                        delay_ratio = (预期周期-实际周期)/预期周期,
                        project_delay = if_else(delay_ratio<0&CD.产能类型!='基因合成载体',1,0),
                        filter.tag = if_else(delay_ratio<0&CD.产能类型=='基因合成载体',1,0),#标记在计算延期时需要去除的项
                        distribution_delay = if_else((开始时间-Su.实验实际开始日期)/ddays(1)>1,1,0)
                ) 
                
        
        ##个人延期度、延期率
        indent_delay <- 
                dt_capacity %>%
                dplyr::filter(filter.tag==0) %>% 
                group_by(CE.实验执行人姓名,统计周期,CD.子任务类型) %>%
                summarise(
                        完成项目数 = n(),
                        延期度 = round((sum(预期周期)-sum(实际周期))/sum(预期周期)
                                    ,digits = 4)*100,
                        延期率 = round(sum(project_delay,na.rm = T)/完成项目数
                                         ,digits = 4)*100,
                        ) %>% 
                arrange(CD.子任务类型) %>% 
                rename(姓名=CE.实验执行人姓名)
        
        # indent_product <- 
        #         dt_capacity %>%
        #         dplyr::filter(是否是子任务=='Y') %>% 
        #         group_by(CE.实验执行人姓名,统计周期,CD.子任务类型) %>%
        #         summarise(
        #                 产值=sum(CD.子产能)
        #         ) %>% 
        #         arrange(CD.子任务类型) %>% 
        #         rename(姓名=CE.实验执行人姓名)
        
        #小组延期度、延期率
        team_delay <- 
                dt_capacity %>%
                #dplyr::filter(是否是子任务=='Y') %>% 
                group_by(CD.子任务类型,统计周期) %>% 
                summarise(
                        完成项目数 = n(),
                        延期度 = round((sum(预期周期)-sum(实际周期))/sum(预期周期)
                                    ,digits = 4)*100,
                        延期率 = round(sum(project_delay,na.rm = T)/完成项目数
                                    ,digits = 4)*100,
                        姓名 = 'total',
                        产值 = sum(CD.子产能)) 
        
        #个人按任务类型计算产值
        indent_production <- 
                dt_capacity %>% 
                #dplyr::filter(是否是子任务=='Y') %>%
                tidyr::unnest(c(CD.产能类型,CD.组成产能),keep_empty=T) %>% 
                mutate(CD.组成产能=as.numeric(CD.组成产能)) %>% 
                group_by(CE.实验执行人姓名,CD.产能类型,CD.子任务类型,统计周期) %>% 
                summarise(产值=sum(CD.组成产能)) %>% 
                arrange(CD.子任务类型,CE.实验执行人姓名) %>% 
                rename(姓名=CE.实验执行人姓名)
        
        dt_summary1 <- 
        left_join(indent_production,indent_delay) %>% 
                bind_rows(team_delay) %>% 
                arrange(CD.子任务类型,desc(姓名),产值) %>% 
                rename('延期度%'=延期度,'延期率%'=延期率)
                
        dt_summary2 <-         
                dt_capacity %>% 
                #dplyr::filter(是否是子任务=='Y') %>%
                group_by(Su.实验分配人姓名,统计周期) %>%
                summarise(
                        任务派发延期率 = round(sum(distribution_delay)/n(),digits = 1)
                )
        
        dt_design_capacity <- 
                dt %>% 
                dplyr::filter(是否是子任务=='N') %>%
                drop_na(开始时间,A.方案指派日期,A.方案设计者) %>%
                #dplyr::filter(A.方案设计者=='刘艳')
                mutate(
                        统计周期 = time_filter(A.方案指派日期),
                        S.消费金额 = 
                                strsplit(S.消费金额,split='[[:punct:]]')
                                ,
                        S.合同金额 = as.numeric(S.合同金额)
                        ) %>%
                dplyr::filter(
                        year(A.方案指派日期)==year(time_span),
                        统计周期==time_filter(time_span)
                )%>%
                #distinct(主任务ID,.keep_all=T) %>% 
                mutate(design_delay = if_else((开始时间-A.方案指派日期)/ddays(1)>2,1,0))
        #%>% dplyr::filter(dt_design_capacity,A.方案设计者=='张权')
        
        #计算除消费金额外的总计
        dt_summary <- 
        dt_design_capacity %>% 
                group_by(A.方案设计者) %>% 
                # select(开始时间,A.方案指派日期,A.方案设计者,S.合同金额,S.消费金额)
                summarise(
                        方案设计延期数 = sum(design_delay),
                        方案设计延期率 = round(方案设计延期数/n(),digits = 1),
                        延期任务ID = str_c(任务ID[design_delay==1],collapse = ','),
                        S.合同金额 = sum(S.合同金额*abs(design_delay-1),na.rm=T)
                )
        
        #合并消费金额
        dt_summary3 <- 
                dt_design_capacity %>% 
                group_by(A.方案设计者) %>% 
                tidyr::unnest(S.消费金额) %>% 
                # select(开始时间,A.方案指派日期,A.方案设计者,S.合同金额,S.消费金额)
                summarise(
                        S.消费金额 = sum(as.numeric(S.消费金额)*abs(design_delay-1),na.rm=T)
                ) %>% 
                right_join(dt_summary) %>% 
                mutate(方案设计产值=S.消费金额+S.合同金额) %>% 
                select(-c(S.消费金额,S.合同金额))
        
        dt.out <- list('生产相关'=dt_capacity,'方案设计相关'=dt_design_capacity)
        return(list(dt_summary1,dt_summary2,dt_summary3,dt.out))
}

seal_cal <- function(dt,time_span,period_type){
        time_filter <- switch(period_type,
                              '周度' = function(x){
                                      as.character(cut(x,'week',start.on.monday=F))
                              },
                              '月度' = month,
                              '年度' = year)
        
        dt <-
                dt %>%
                mutate(
                        D.任务周期.工作日 = as.numeric(D.任务周期.工作日),
                        统计周期 = time_filter(开始时间)
                        ) %>%
                filter(统计周期==time_filter(time_span))
                
        return(list(dt))
}

