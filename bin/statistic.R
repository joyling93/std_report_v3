# period_type <- '月度'
# time_span <- 2021-05-07
# db_type <- 'product_sec'
# DBI::dbDisconnect(db)
# load('debug/test/test_env.Rds')
# save.image('debug/test/test_env.Rds')

# 数据去重和日期处理
db_clean <- function(db_type,tag='无'){
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
                        select(任务ID,A.方案设计者,A.方案指派日期,S.合同金额,S.消费金额,S.客户姓名,S.客户单位) %>% 
                        rename(主任务ID=任务ID) %>% 
                        mutate(主任务ID=tolower(主任务ID))
                        
                dt2 <- dt %>% 
                        dplyr::filter(任务类型=='生产序列模板') %>% 
                        select(-c(A.方案设计者,A.方案指派日期,S.合同金额,S.消费金额,S.客户姓名,S.客户单位)) %>% 
                        mutate(
                                主任务ID = unlist(map(标题,
                                                    ~tolower(str_extract(.x,regex('fw-?\\d+|DS-?\\d+', ignore_case = T))))),
                                #为没有’-‘的任务ID添加’-‘，避免匹配丢失问题
                                主任务ID = if_else(str_detect(主任务ID,'-'),
                                                主任务ID
                                                ,paste0(str_sub(主任务ID,1,2),'-',str_extract(主任务ID,'\\d+')))
                                  ) %>% 
                        left_join(dt_extra) %>% 
                        mutate(across(matches('时间|日期'),~ymd_hms(.x,tz='Asia/Shanghai')),
                               across(contains('姓名'),as.factor),
                               #across(ends_with('产能'),as.numeric),
                               across(contains('周期'),as.numeric)
                        )
        }else if(db_type=='seal_sec'){
                dt_extra <- dt %>% 
                        dplyr::filter(任务类型=='生产序列模板',是否是子任务=='Y') %>% 
                        mutate(
                                主任务ID = unlist(map(标题,
                                                     ~tolower(str_extract(.x,regex('fw-?\\d+|DS-?\\d+', ignore_case = T))))),
                                #为没有’-‘的任务ID添加’-‘，避免匹配丢失问题
                                主任务ID = if_else(str_detect(主任务ID,'-'),
                                                主任务ID
                                                ,paste0(str_sub(主任务ID,1,2),'-',str_extract(主任务ID,'\\d+'))),
                                #CD.子任务类型 = if_else(is.na(CD.子任务类型),'字段未填',CD.子任务类型),
                                CE.实验执行人姓名 = if_else(CE.实验执行人姓名=='','字段未填',CE.实验执行人姓名)
                                ) %>% 
                        drop_na(CD.子任务类型) %>% 
                        select(
                                主任务ID,CE.实验执行人姓名,CD.子任务类型,CD.子产能,Su.实验实际开始日期,
                                Su.实验实际完成日期,延期原因
                                  ) %>% 
                        pivot_wider(names_from = CD.子任务类型,
                                    values_from=c(CD.子产能,Su.实验实际开始日期,CE.实验执行人姓名,
                                                  Su.实验实际完成日期,延期原因),
                                    names_glue = "{CD.子任务类型}_{.value}"
                                    ) %>% 
                        unnest() %>% 
                        rename(任务ID=主任务ID) %>% 
                        select(任务ID,contains('分子'),contains('病毒'),contains('细胞'))
                
                dt2 <- dt %>% 
                        dplyr::filter(任务类型=='销售序列模板') %>% 
                        select(
                                where(~!all(is.na(.x)))
                        ) %>% 
                        left_join(dt_extra) %>% 
                        mutate(
                                #across(matches('时间|日期'),ymd_hms),
                                #across(contains('姓名'),as.factor),
                                across(ends_with('产能'),as.numeric),
                                across(contains('周期'),as.numeric),
                                是否重复=if_else(
                                        任务ID%in%任务ID[duplicated(任务ID)]
                                        ,'重复','未重复'
                                )
                               )
        }else if(db_type=='management_sec'){
                if(tag=='无'){
                        dt2 <- data_extraction(dt,'management_sec')
                }else if(tag=='使用手动校正数据'){
                        db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
                        dt2 <- dbReadTable(db,'adjust_raw')
                        DBI::dbDisconnect(db)
                }
        }else if(db_type=='exp_info'){
                db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
                dt_info <- dbReadTable(db,'exp_info')
                DBI::dbDisconnect(db)
                dt_s <- dt %>% 
                        dplyr::filter(任务类型%in%c('销售序列模板','售后序列模板')) %>% 
                        select(任务ID)
                
                dt_p <- dt %>% 
                        dplyr::filter(任务类型%in%c('生产序列模板')) %>% 
                        dplyr::filter(是否是子任务=='Y') %>% 
                        mutate(
                                主任务ID = unlist(map(标题,
                                                     ~tolower(str_extract(.x,regex('fw-?\\d+|DS-?\\d+', ignore_case = T))))),
                                #为没有’-‘的任务ID添加’-‘，避免匹配丢失问题
                                主任务ID = if_else(
                                        str_detect(主任务ID,'-'),
                                        主任务ID,
                                        paste0(str_sub(主任务ID,1,2),'-',str_extract(主任务ID,'\\d+'))
                                )
                        ) %>% 
                        select(CD.子任务类型,Su.实验实际完成日期,主任务ID)
                
                dt.fin <- 
                        dt_p %>%         
                        right_join(dt_s,by=c('主任务ID'='任务ID'))
                
                dt2 <- 
                        dt_info %>% 
                        mutate(
                                CD.子任务类型=
                                        if_else(is.na(病毒类型),'分子','病毒'),
                                CD.子任务类型=
                                        if_else(!is.na(细胞名称),'细胞',CD.子任务类型)
                        ) %>% 
                        mutate(
                                主任务ID = unlist(map(生产主任务标题,
                                                          ~tolower(str_extract(.x,regex('fw-?\\d+|DS-?\\d+', ignore_case = T))))),
                                #为没有’-‘的任务ID添加’-‘，避免匹配丢失问题
                                主任务ID = if_else(
                                        str_detect(主任务ID,'-'),
                                        主任务ID,
                                        paste0(str_sub(主任务ID,1,2),'-',str_extract(主任务ID,'\\d+'))
                                )
                        ) %>% 
                        left_join(dt.fin)
        }else if(db_type=='sales_commission_cal'){
                dt2 <- data_extraction(dt,'tb_c_ledger')
        }else if(db_type=='admin_data_cal'){
                dt2 <- data_extraction(dt,'tb_c_ledger')
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

#2021调休表
special_days <- list(
        'holidays'=ymd(
                c("2021年1月1日", "2021年1月2日", "2021年1月3日", "2021年2月11日", "2021年2月12日", "2021年2月13日", "2021年2月14日", "2021年2月15日", "2021年2月16日", "2021年2月17日", "2021年4月3日", "2021年4月4日", "2021年4月5日", "2021年5月1日", "2021年5月2日", "2021年5月3日", "2021年5月4日", "2021年5月5日", "2021年6月12日", "2021年6月13日", "2021年6月14日", "2021年9月19日", "2021年9月20日", "2021年9月21日", "2021年10月1日", "2021年10月2日", "2021年10月3日", "2021年10月4日", "2021年10月5日", "2021年10月6日", "2021年10月7日")
        ),#法定假日
        'switch.days'=ymd(
                c("2021年2月7日", "2021年2月20日", "2021年4月25日", "2021年5月8日", "2021年9月18日", "2021年9月26日", "2021年10月9日")
        )#工作日调休
)

# 计算开始日期x到结束日期y经历的工作日
workday_cal <- function(x,y){
        x <- as.Date(x)
        y <- as.Date(y)
        period <- x+days(1:ceiling((y-x)/ddays(1)))
        offset1 <- sum(
                period%in%special_days[['holidays']][wday(special_days[['holidays']])%in%c(2:6)]
        )
        offset2 <- sum(period%in%special_days[['switch.days']])
        sum(wday(period)%in%c(2:6))-offset1+offset2
}

design_delay_cal <- function(x,y){
        #计算日期间小时数
        int.hour <- ceiling((y-x)/dhours(1))
        if(int.hour<=48){
                return(0)
        }else{
                #判定非工作日间隔,包括第一天
                
                int.days <- x+days(0:ceiling((y-x)/ddays(1)))
                offset1 <- sum(
                        int.days%in%special_days[['holidays']][wday(special_days[['holidays']])%in%c(2:6)]
                        )#计算除周末外的法定假日补偿
                offset2 <- sum(int.days%in%special_days[['switch.days']])#计算调休补偿
                weekend.days <- sum(wday(int.days)%in%c(1,7))
                weekday.hour <- int.hour-(weekend.days+offset1-offset2)*24
                if(weekday.hour<=48){
                        return(0)
                }else{
                        return(1)
                }
        }
        
}


# 计算生产部门延期率，产能
delay_cal <- function(dt,time_span,period_type){
        time_filter <- switch(period_type,
                              '周度' = function(x){
                                      ymd(cut(x+ddays(1),'week',start.on.monday=F))-ddays(1)
                                      #延后实际日期使周数从周五开始
                                      },
                              '月度' = month,
                              '年度' = year)
        #产能计算原始表
        dt_capacity <-
                #test <- 
                dt %>%
                dplyr::filter(是否是子任务=='Y') %>% 
                drop_na(开始时间,截止时间,Su.实验实际开始日期,Su.实验实际完成日期) %>% 
                mutate(统计周期 = time_filter(Su.实验实际完成日期),
                           CD.子产能 = as.numeric(CD.子产能)) %>% 
                dplyr::filter(year(Su.实验实际完成日期)==year(time_span),
                              统计周期==time_filter(time_span)) %>% 
                mutate(
                        预期周期 = map2_dbl(开始时间,截止时间,workday_cal),
                        实际周期 = map2_dbl(Su.实验实际开始日期,Su.实验实际完成日期,workday_cal),
                        delay_ratio = (预期周期-实际周期)/预期周期,
                        project_delay = if_else(delay_ratio<0&CD.产能类型!='基因合成载体'&截止时间<Su.实验实际完成日期
                                                ,1,0),#标记延期项目
                        filter.tag = if_else(delay_ratio<0&(str_detect(CD.产能类型,'基因合成载体')|截止时间>Su.实验实际完成日期)
                                             ,1,0),#标记在计算延期时需要去除的项
                        distribution_delay = if_else((开始时间-Su.实验实际开始日期)/ddays(1)>1,1,0),
                        CD.组成产能 = as.character(CD.组成产能),
                        CD.产能类型 = strsplit(CD.产能类型,split="[|,，]"),
                        CD.组成产能 = strsplit(CD.组成产能,split="[|,，]"),
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
                drop_na(CD.组成产能) %>% 
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
                mutate(design_delay = map2_dbl(A.方案指派日期,开始时间,design_delay_cal))
        
        #其他项目计算原始表
        dt_others <- 
                dt %>% 
                dplyr::filter(是否是子任务=='N') %>%
                mutate(
                        统计周期 = time_filter(Su.实验实际完成日期)
                )%>%
                dplyr::filter(
                        year(Su.实验实际完成日期)==year(time_span),
                        统计周期==time_filter(time_span)
                )
        
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
        
        #定期计算包装细胞盘数
        dt_summary4 <- 
                dt_others %>% 
                group_by(统计周期) %>% 
                summarise(
                        包装细胞盘数总计=sum(as.numeric(MD.包装细胞盘数),na.rm = T)
                )
        
        dt.out <- list('生产相关'=dt_capacity,'方案设计相关'=dt_design_capacity)
        return(list(dt_summary1,dt_summary2,dt_summary3,dt_summary4,dt.out))
}

seal_cal <- function(dt,time_span,period_type,tag){
        time_filter <- switch(period_type,
                              '周度' = function(x){
                                      as.character(cut(x,'week',start.on.monday=F))
                              },
                              '月度' = month,
                              '年度' = year)
        if(tag=='无'){
                dt <-
                        dt %>%
                        mutate(
                                #D.任务周期.工作日 = as.numeric(D.任务周期.工作日),
                                统计周期 = time_filter(A.合同签订日期)
                        ) %>%
                        dplyr::filter(统计周期==time_filter(time_span))
        }else if(tag=='不筛选特定时间'){
                dt
                        # dt %>%
                        # mutate(
                        #         D.任务周期.工作日 = as.numeric(D.任务周期.工作日),
                        # )
        }else{
                dt
        }
        
                
        return(list(dt))
}

##数据提取
data_extraction <- 
        function(dt,tag){
                if(tag=='management_sec'){
                        dt_s <- dt %>% 
                                dplyr::filter(任务类型%in%c('销售序列模板','售后序列模板'))
                        
                        dt_p <- dt %>% 
                                dplyr::filter(任务类型%in%c('生产序列模板')) %>% 
                                dplyr::filter(是否是子任务=='Y') %>% 
                                mutate(
                                        主任务ID = unlist(map(标题,
                                                             ~tolower(str_extract(.x,regex('fw-?\\d+|DS-?\\d+', ignore_case = T))))),
                                        #为没有’-‘的任务ID添加’-‘，避免匹配丢失问题
                                        主任务ID = if_else(
                                                str_detect(主任务ID,'-'),
                                                主任务ID,
                                                paste0(str_sub(主任务ID,1,2),'-',str_extract(主任务ID,'\\d+'))
                                        )
                                ) %>% 
                                select(CD.子任务类型,CD.子产能,CE.实验执行人姓名,主任务ID,CD.产能类型)
                        
                        dt.fin <- 
                                dt_p %>%         
                                group_by(主任务ID) %>% 
                                mutate(
                                        CD.子任务类型 = if_else(CD.子任务类型=='病毒',CD.产能类型,CD.子任务类型),#确定具体病毒子类
                                        CD.子任务类型 = if_else(CD.子任务类型=='','类型字段未填',CD.子任务类型)
                                ) %>% 
                                group_by(主任务ID,CE.实验执行人姓名,CD.子任务类型) %>% 
                                summarise(CD.子产能=sum(as.numeric(CD.子产能))) %>% 
                                rename(
                                        任务ID=主任务ID,
                                        生产执行人=CE.实验执行人姓名,
                                        产值类型=CD.子任务类型,
                                        产值=CD.子产能
                                ) %>% 
                                right_join(dt_s,by=c('任务ID'))
                        
                        #qc
                        output <- 
                                dt.fin %>% 
                                mutate(
                                        S.消费金额=map_int(S.消费金额,function(x){
                                                sum(as.integer(str_split(x, ',',simplify = T)),na.rm = T)
                                        }),#拆分逗号分隔的消费金额并计算消费总额
                                        S.合同金额=as.numeric(replace_na(S.合同金额,0)),
                                        销售额=S.合同金额,
                                        销售业务类别=str_replace(A.业务类别,'.* / ','')#去除业务类别的一级内容
                                ) %>% 
                                group_by(任务ID) %>% 
                                mutate(
                                        产值比例=产值/sum(产值,na.rm = T),
                                        销售额=if_else(is.na(产值比例),销售额,销售额*产值比例),
                                        销售业务类别=if_else(
                                                str_detect(销售业务类别,'分子|病毒|细胞')&!is.na(产值类型)&产值类型!='2020旧项目',
                                                产值类型,销售业务类别),
                                        qctag=if_else(
                                                S.客户单位=='合生生物'|is.na(S.客户单位),
                                                '统计排除项','pass'),
                                        销售业务类别=fct_collapse(销售业务类别,
                                                                  大综合=c('代理大综合','代理大综合'),
                                                                  细胞=c('细胞检测','细胞系构建')
                                        ),
                                        销售业务类别=fct_other(
                                                销售业务类别, 
                                                keep=c('分子','慢病毒','腺病毒','腺相关病毒','细胞','试剂','其他','预付款','代理','生信','大综合'),
                                                other_level = "类别存疑"),
                                        销售业务类别=fct_relevel(
                                                销售业务类别,
                                                c('分子','慢病毒','腺病毒','腺相关病毒','细胞','试剂','其他','预付款','代理','生信','大综合','类别存疑')
                                        )
                                        
                                ) %>% 
                                # mutate(
                                #         qctag=if_else(销售业务类别%in%c('分子','细胞','慢病毒','腺病毒','腺相关病毒')&销售业务类别%in%产值类型,qctag,'测试')
                                # ) %>% 
                                select(任务ID,产值比例,生产执行人,标题,任务类型,
                                         产值类型,销售业务类别,A.业务类别,产值,销售额,S.合同金额,S.消费金额,A.变动金额,A.合同签订日期,
                                         S.客户单位,S.销售姓名,qctag)
                        
                }else if(tag=='tb_c_ledger'){
                        db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
                        ledger_raw <- dbReadTable(db,'ledger_raw') %>% 
                                mutate(
                                        开票日期=as_date(开票日期),
                                        回款日期=as_date(回款日期)
                                )
                        DBI::dbDisconnect(db)
                        #tb数据清洗
                        tb.data <- 
                                dt %>% 
                                mutate(
                                        任务ID=if_else(str_detect(任务ID,'ds'),str_match(标题,regex('A-FW-\\d+',ignore_case = T)),任务ID),
                                        Ag.外包成本=map2_dbl(Ag.外包成本,Ag.整体实验固定费用,function(x,y){
                                                sum(as.numeric(x),as.numeric(y),na.rm = T)
                                        }),#Ag.外包成本和Ag.整体实验固定费用都属于外包成本，仅发生过名称改变
                                        link=tolower(str_replace_all(任务ID,'[ -]','')),#统一link格式
                                        延期计算方式=if_else(is.na(Su.实验实际完成日期),1,2)
                                ) %>% 
                                dplyr::filter(任务类型=='销售序列模板') %>% 
                                separate(A.业务类别,into = c('一级任务类别','二级任务类别'),sep = ' / ') %>% 
                                mutate(
                                        项目延期=if_else(延期计算方式==1,截止时间<=today(),Su.实验实际完成日期>截止时间),
                                        一级任务类别=if_else(一级任务类别=='预付款消费'&二级任务类别=='代理','预付款消费代理',一级任务类别),#将二级任务为代理的预付款消费任务改为’预付款消费代理‘
                                        S.消费金额=map_int(S.消费金额,function(x){
                                                sum(as.integer(str_split(x, '[,，]',simplify = T)),na.rm = T)}),
                                        across(matches('成本|金额'),as.numeric)
                                ) %>% 
                                select(标题,任务ID,A.合同签订日期,Ag.外包成本,S.销售姓名,一级任务类别,二级任务类别,
                                         S.客户姓名,S.合同金额,S.消费金额,F.未回款,F.已开票,截止时间,Su.实验实际完成日期,项目延期,link)
                        
                        #台账数据整合
                        dt.all <- 
                                ledger_raw %>% 
                                mutate(
                                        link=tolower(str_replace_all(合同号,'[ -]',''))
                                ) %>% 
                                full_join(tb.data) %>% 
                                mutate(
                                        销售类别=fct_collapse(销售姓名,
                                                              刘艳=c('刘艳'),
                                                              大客户=c('大客户','周冰'),
                                                              华北大客户=c('华北大客户','杨宝兴','樊伟','贾东东'),
                                                              西南大客户=c('西南大客户','张义凯'),
                                                              other_level=c('销售专员')
                                        ),
                                        across(ends_with('金额'),replace_na,0),
                                        未开票金额=S.合同金额-开具金额,
                                        未回款金额=S.合同金额-回款金额,
                                        合同总额=S.合同金额+S.消费金额,
                                        质控标记=if_else(is.na(A.合同签订日期),'没有合同签订日期','pass'),
                                        质控标记=if_else(is.na(一级任务类别),'没有填写业务类别',质控标记)
                                ) 
                }
                
        }


#成本产值计算
management_data_cal <- 
        function(dt,time_span,period_type,tag){
                index_cal <- 
                        function(data.supp,data.tb){
                                #functions
                                labor_cal <- 
                                        function(labor.table){
                                                #细胞准备人工按合同额比例拆分给不同病毒类型人工
                                                labor.table <- 
                                                        labor.table %>% 
                                                        mutate(
                                                                腺相关病毒=腺相关病毒+病毒组细胞准备*contribute_ratio_virus["腺相关病毒"],
                                                                慢病毒=慢病毒+病毒组细胞准备*contribute_ratio_virus["慢病毒"],
                                                                腺病毒=病毒组细胞准备*contribute_ratio_virus["腺病毒"]
                                                        )
                                                
                                                #按业务类型拆分人工
                                                dt1 <- 
                                                        map_dfr(names(contribute_ratio),function(type){
                                                                tibble::tibble(
                                                                        type1=type,
                                                                        subtype=c('管理','仓库','产品','销售','企管'),
                                                                        value=c(
                                                                                labor.table$生产管理*contribute_ratio[type],
                                                                                labor.table$仓库*contribute_ratio[type],
                                                                                labor.table$产品生产支持*contribute_ratio[type],
                                                                                labor.table$销售*seals.ratio.not_others[type],
                                                                                labor.table$企管*seals.ratio.not_others[type]
                                                                        )
                                                                ) 
                                                        })
                                                
                                                dt2 <- 
                                                        map_dfr(c('试剂','生信','代理','大综合','预付款'),function(type){
                                                                tibble::tibble(
                                                                        type1=type,
                                                                        subtype=c('产品','销售','企管'),
                                                                        value=c(
                                                                                0,
                                                                                labor.table$销售*seals.ratio.not_others[type],
                                                                                labor.table$企管*seals.ratio.not_others[type]
                                                                        )
                                                                )
                                                        })
                                                
                                                labor.all <- 
                                                        bind_rows(dt1,dt2) %>% 
                                                        add_row(
                                                                type1=c('研发','分子','细胞','慢病毒','腺病毒','腺相关病毒'),
                                                                subtype=c('产品','生产','产品','生产','生产','生产'),
                                                                value=c(labor.table$产品研发,labor.table$分子,labor.table$细胞,
                                                                        labor.table$慢病毒,labor.table$腺病毒,labor.table$腺相关病毒
                                                                )
                                                        ) %>% 
                                                        mutate(
                                                                tag='labor'
                                                        )
                                        }
                                
                                fee_cal <- function(daily.operation.fee,rent.fee){
                                        fee.all <- 
                                                map_dfr(names(seals.ratio.not_others),function(type){
                                                        tibble::tibble(
                                                                type1=type,
                                                                subtype=c('生产','产品','销售','企管','房租','水电','网费','母公司服务费','固定资产折旧'),
                                                                value=c(
                                                                        daily.operation.fee$生产部*contribute_ratio[type],
                                                                        daily.operation.fee$产品部*contribute_ratio[type],
                                                                        daily.operation.fee$销售部*seals.ratio.not_others[type],
                                                                        daily.operation.fee$企管部*seals.ratio.not_others[type],
                                                                        rent.fee$房租*contribute_ratio[type],
                                                                        rent.fee$水电*contribute_ratio[type],
                                                                        rent.fee$网费*contribute_ratio[type],
                                                                        rent.fee$母公司服务费*contribute_ratio[type],
                                                                        rent.fee$固定资产折旧*contribute_ratio[type]
                                                                )
                                                        )
                                                }
                                                ) %>% 
                                                mutate(
                                                        tag='fee'
                                                )
                                }
                                
                                cost_cal <- function(cost.export,cost.seq,cost.pack){
                                        cost.export <- 
                                                cost.export %>% 
                                                mutate(
                                                        分子=分子+公共*contribute_ratio['分子'],
                                                        细胞=细胞+公共*contribute_ratio['细胞'],
                                                        慢病毒=病毒*contribute_ratio_virus['慢病毒']+公共*contribute_ratio['慢病毒'],
                                                        腺病毒=病毒*contribute_ratio_virus['腺病毒']+公共*contribute_ratio['腺病毒'],
                                                        腺相关病毒=病毒*contribute_ratio_virus['腺相关病毒']+公共*contribute_ratio['腺相关病毒']
                                                        # 生产=分子+慢病毒+腺病毒+腺相关病毒,
                                                        # 产品=细胞+研发
                                                )
                                        
                                        cost.seq <- 
                                                cost.seq %>% 
                                                mutate(
                                                        across(everything(),function(x){
                                                                replace_na(x,0)
                                                        })
                                                        # 生产=(分子+慢病毒+腺病毒+腺相关病毒),
                                                        # 产品=细胞+研发
                                                )
                                        
                                        cost.pack <- 
                                                cost.pack %>% 
                                                mutate(
                                                        across(everything(),function(x){
                                                                replace_na(x,0)
                                                        })
                                                        # 生产=(分子+慢病毒+腺病毒+腺相关病毒),
                                                        # 产品=细胞
                                                )
                                        
                                        cost.all <- 
                                                bind_rows(cost.export,cost.seq,cost.pack) %>% 
                                                pivot_longer(everything(),names_to='type1',values_to='value') %>% 
                                                group_by(type1) %>% 
                                                summarise(
                                                        value=sum(value,na.rm = T),
                                                        tag='cost'
                                                ) %>% 
                                                mutate(
                                                        subtype=fct_collapse(type1,
                                                                             生产=c('分子','慢病毒','腺病毒','腺相关病毒'),
                                                                             产品=c('细胞','研发'),
                                                                             other_level='drop'
                                                        )
                                                ) %>% 
                                                dplyr::filter(subtype!='drop')  
                                }
                                
                                
                                dt <- data.tb
                                
                                supp.dt <- data.supp
                                
                                #产值和比例计算
                                
                                ##按产能比例拆分销售额
                                contribute_table <- 
                                        dt %>% 
                                        #dplyr::filter(qctag!='产能计算排除') %>% 
                                        group_by(产值类型) %>% 
                                        summarise(
                                                p.value=sum(产值,na.rm = T)
                                        ) %>% 
                                        drop_na()
                                
                                contribute_table <- bind_rows(contribute_table,
                                                              tibble(
                                                                      产值类型=c('分子','细胞','慢病毒','腺病毒','腺相关病毒'),
                                                                        p.value=as.double(0,0,0,0,0)
                                                              )
                                ) %>% 
                                        distinct(产值类型,.keep_all=T)
                                
                                
                                
                                #计算含细胞产能比例
                                contribute_ratio <- 
                                        contribute_table %>% 
                                        ungroup() %>% 
                                        mutate(
                                                p.ratio=p.value/sum(p.value)
                                        )%>% 
                                        pull(p.ratio,name=产值类型)#以命名向量形式导出比例，方便引用
                                
                                #计算不含细胞的产能比例
                                # contribute_ratio_not_cell <- 
                                #         contribute_table %>% 
                                #         dplyr::filter(产值类型!='细胞') %>% 
                                #         ungroup() %>% 
                                #         mutate(
                                #                 p.ratio.nc=p.value/sum(p.value),
                                #                 .keep="unused"
                                #         ) %>% 
                                #         pull(p.ratio.nc,name=产值类型)
                                
                                #计算仅病毒产能比例
                                contribute_ratio_virus <- 
                                        contribute_table %>% 
                                        dplyr::filter(str_detect(产值类型,'病毒')) %>% 
                                        ungroup() %>% 
                                        mutate(
                                                p.ratio.v=p.value/sum(p.value),
                                                .keep="unused"
                                        )%>% 
                                        pull(p.ratio.v,name=产值类型)
                                
                                ##销售额和比例计算
                                seals.dt <- 
                                        dt %>% 
                                        #dplyr::filter(qctag!='销售额计算排除') %>% 
                                        group_by(销售业务类别) %>% 
                                        summarise(
                                                sum=sum(销售额)
                                        ) %>% 
                                        #drop_na() %>% 
                                        rename(type1='销售业务类别')
                                
                                #无其他比例
                                seals.ratio.not_others <- 
                                        seals.dt %>% 
                                        dplyr::filter(type1!='其他') %>% 
                                        mutate(
                                                ratio=sum/sum(sum)
                                        ) %>% 
                                        pull(ratio,name=type1)
                                
                                #无预付款和其他比例
                                seals.ratio_not_adv_others <- 
                                        seals.dt %>% 
                                        dplyr::filter(!type1%in%c('其他','预付款')) %>% 
                                        mutate(
                                                ratio=sum/sum(sum)
                                        ) %>% 
                                        pull(ratio,name=type1)
                                
                                #按销售额比例计算预付款剩余
                                deposit <- 
                                        enframe(seals.ratio_not_adv_others,name = 'type1') %>% 
                                        mutate(
                                                sum=value*supp.dt$data[supp.dt$tag=='预付款'][[1]]$剩余预付款,
                                                tag='deposit'
                                        )
                                
                                ###人工计算
                                labor.all <- 
                                        labor_cal(supp.dt$data[supp.dt$tag=='人工'][[1]])
                                
                                
                                ###日常和固定成本计算
                                fee.all <- fee_cal(
                                        supp.dt$data[supp.dt$tag=='日常经营'][[1]]
                                        ,
                                        supp.dt$data[supp.dt$tag=='房租水电'][[1]]
                                )
                                
                                
                                ##材料出库，测序，包装成本
                                cost.export <- dplyr::filter(supp.dt$data[supp.dt$tag=='材料、基因合成、测序等'][[1]],
                                                             类型=='材料') %>% 
                                        select(-c(类型,日期))
                                cost.seq <- dplyr::filter(supp.dt$data[supp.dt$tag=='材料、基因合成、测序等'][[1]],
                                                          类型=='基因合成+测序成本')%>% 
                                        select(-c(类型,日期))
                                cost.pack <- dplyr::filter(supp.dt$data[supp.dt$tag=='材料、基因合成、测序等'][[1]],
                                                           类型=='干冰泡沫箱')%>% 
                                        select(-c(类型,日期))
                                cost.all <- cost_cal(cost.export,cost.seq,cost.pack)
                                
                                
                                ##汇总
                                dt.all <- 
                                        bind_rows(fee.all,labor.all,cost.all) %>% 
                                        mutate(
                                                value=replace_na(value,0),
                                                subtype=fct_collapse(subtype,
                                                                     生产=c('仓库','管理'),
                                                                     房租等固定成本=c('固定资产折旧','母公司服务费',
                                                                               '水电','网费','房租')
                                                )
                                        ) %>% 
                                        arrange(type1)
                                
                                #按部门和成本类型汇总成本
                                summary1 <- 
                                        dt.all %>% 
                                        group_by(subtype,tag) %>% 
                                        summarise(
                                                sum=sum(value)
                                        )%>% 
                                        pivot_wider(
                                                names_from = tag,
                                                values_from = sum
                                        ) %>% 
                                        mutate(
                                                小计=sum(cost,fee,labor,na.rm = T)
                                        ) %>% 
                                        ungroup() %>% 
                                        mutate(
                                                `比例%`=round(小计/sum(小计,na.rm = T)*100,digits = 2)
                                        ) %>% 
                                        rename(
                                                材料=cost,
                                                日常经营=fee,
                                                人工=labor
                                        )
                                #按业务类型和成本类型汇总成本
                                summary4 <- 
                                        dt.all %>% 
                                        ungroup() %>% 
                                        mutate(
                                                tag=if_else(subtype=='房租等固定成本','房租等固定成本',tag)
                                        ) %>% 
                                        group_by(type1,tag) %>% 
                                        summarise(
                                                sum=sum(value)
                                        )%>% 
                                        pivot_wider(
                                                names_from = tag,
                                                values_from = sum
                                        ) %>% 
                                        mutate(
                                                小计=sum(cost,fee,labor,na.rm = T)
                                        ) %>% 
                                        ungroup() %>% 
                                        mutate(
                                                `比例%`=round(小计/sum(小计,na.rm = T)*100,digits = 2)
                                        ) %>% 
                                        rename(
                                                材料=cost,
                                                日常经营=fee,
                                                人工=labor
                                        ) 
                                
                                #按业务类型汇总成本
                                summary2 <- 
                                        dt.all %>% 
                                        group_by(type1) %>% 
                                        summarise(
                                                sum=sum(value)
                                        )%>% 
                                        mutate(
                                                ratio=round(sum/sum(sum)*100,digits = 2)
                                        ) 
                                # rename(
                                #         #小计='sum',
                                #         `比例%`=ratio
                                # )
                                
                                #按部门汇总成本
                                summary3 <- 
                                        dt.all %>% 
                                        group_by(subtype,tag) %>% 
                                        summarise(
                                                sum=sum(value)
                                        )%>% 
                                        group_by(subtype) %>% 
                                        summarise(
                                                sum=sum(sum)
                                        ) %>% 
                                        mutate(
                                                ratio=round(sum/sum(sum)*100,digits = 2)
                                        )
                                # rename(
                                #         #小计='sum',
                                #         `比例%`=ratio
                                # )
                                
                                #按业务类型汇总成本、销售额、预付款并计算成本销售比和成本产值比
                                output <- 
                                        bind_rows(
                                                mutate(summary2,
                                                       tag='成本小计'),
                                                mutate(seals.dt,
                                                       tag='销售额小计'),
                                                mutate(deposit,
                                                       tag='预付款小计'),
                                                contribute_table %>% 
                                                        mutate(
                                                                tag='生产产值'
                                                        ) %>% 
                                                        rename(
                                                                'type1'=产值类型,
                                                                'sum'=p.value
                                                        )
                                        ) %>% 
                                        select(type1,sum,tag) %>% 
                                        pivot_wider(names_from = 'tag',values_from='sum') %>% 
                                        # dplyr::filter(
                                        #         type1%in%c('分子','慢病毒','腺病毒','腺相关病毒','细胞')
                                        # ) %>% 
                                        mutate(
                                                `成本销售比%`=round(
                                                        成本小计/(销售额小计+预付款小计)*100,
                                                        digits = 2),
                                                `成本产值比%`=round(
                                                        成本小计/生产产值*100,
                                                        digits = 2),
                                        )
                                list(output,summary1,summary2,summary3,summary4)
                        }
                
                dt2 <-
                        #test %>% 
                        dt %>%
                        dplyr::filter(qctag=='pass') %>% 
                        mutate(
                                年度 = year(A.合同签订日期),
                                月份 = month(A.合同签订日期)
                        ) %>% 
                        group_by(年度,月份) %>% 
                        nest()
                
                db.list <- c("人工","开票额","房租水电","日常经营","材料、基因合成、测序等","预付款" )
                db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
                supp.dt <-
                        map_dfr(db.list,function(dbname){
                                dbReadTable(db,dbname)%>%
                                        #dplyr::filter(year(ymd(日期))==current.year) %>% 
                                        mutate(
                                                年度 = year(ymd(日期)),
                                                月份 =  month(ymd(日期))
                                        )%>% 
                                        group_by(年度,月份) %>% 
                                        nest() %>% 
                                        mutate(
                                                tag=dbname
                                        )
                        }) %>% 
                        group_by(年度,月份) %>% 
                        nest()
                dbDisconnect(db)
                
                dt.fin <- 
                        supp.dt %>% 
                        left_join(dt2,by=c("年度", "月份"),suffix = c(".supp", ".tb")) %>% 
                        mutate(
                                out=map2(data.supp,data.tb,index_cal)
                        ) %>% 
                        select(年度,月份,out) %>% 
                        unnest() %>% 
                        mutate(
                                tag=c('成本销售比和成本产值比','按部门和成本类型汇总成本',
                                      '按业务类型汇总成本','按部门汇总成本','按业务类型和成本类型汇总成本')
                        ) %>% 
                        group_by(tag) %>% 
                        nest() %>% 
                        mutate(
                                data=map(data,unnest)
                        ) %>% 
                        deframe()
                
                dt.fin[['原始数据']] <- dt
                
                list(dt.fin[[2]],
                     dt.fin
                )
        }

###销售提成计算
seals_commission_cal <- function(dt.all,time_span,period_type,tag){
        #filepath <- '/Users/zhuomingx/Desktop/Rbio/std_report_v3/debug/销售提成自动化/2021年4月销售部提成数据.xlsx'
        ##数据整合
        #拆分台账合同号，整合销售序列tb任务
        #增加销售类别和一级业务类别tag
        #筛选各种条件,汇总开票回款和预付数据
        #以tag分组汇总开票，回款，代理成本
        #判断tag赋予开票提成系数、回款提成系数和是否减代理成本
        time_filter <- switch(period_type,
                              '周度' = function(x){
                                      ymd(cut(x+ddays(1),'week',start.on.monday=F))-ddays(1)
                                      #延后实际日期使周数从周五开始
                              },
                              '月度' = month,
                              '年度' = year)
        
        dt <- dt.all %>% 
                dplyr::filter(质控标记!='没有合同签订日期')
        #开票额统计
        invoice.dt <- 
                dt %>% 
                mutate(
                        tag=if_else((是否作废重开票=='是'|作废=='是'|开具金额<0),1,0)
                ) %>% 
                dplyr::filter(
                        tag%in%c(0,NA),
                        A.合同签订日期>as.Date('2020-06-30'),
                        time_filter(ymd(开票日期))==time_filter(time_span),
                        year(ymd(开票日期))==year(time_span)#限定统计周期所在年份
                ) %>% 
                mutate(#大综合和代理的提成以不同基数计算，此处其统一为 开具金额 字段下方便计算
                        开具金额=if_else(一级任务类别=='大综合',
                                           (S.合同金额-Ag.外包成本)*回款金额/S.合同金额,开具金额
                        ),
                        开具金额=if_else(一级任务类别=='代理',
                                           开具金额-Ag.外包成本,开具金额
                        )
                )%>% 
                group_by(一级任务类别,销售类别,销售姓名,time_filter(开票日期)) %>% 
                summarise(
                        开具金额=sum(开具金额,na.rm = T),
                        代理成本=sum(Ag.外包成本,na.rm = T)
                )
        
        #回款额统计
        payment.dt <- 
                dt %>% 
                dplyr::filter(
                        time_filter(回款日期)==time_filter(time_span),year(ymd(回款日期))==year(time_span)
                ) %>% 
                mutate(
                        回款金额=if_else(一级任务类别=='大综合',
                                           (S.合同金额-Ag.外包成本)*回款金额/S.合同金额,回款金额
                        ),
                        回款金额=if_else(一级任务类别=='代理',
                                           回款金额-Ag.外包成本,回款金额
                        )
                ) %>% 
                group_by(一级任务类别,销售类别,销售姓名,time_filter(回款日期)) %>% 
                summarise(
                        回款金额=sum(回款金额,na.rm = T),
                        代理成本=sum(Ag.外包成本,na.rm = T)
                ) 
        
        #预付款额统计
        ##预付款消费信息以tb字段为准，须以S.销售姓名修改销售姓名和销售类别
        advance.dt <- 
                dt %>% 
                dplyr::filter(
                        一级任务类别%in%c('预付款消费','预付款消费代理'),
                        time_filter(A.合同签订日期)==time_filter(time_span),
                        year(A.合同签订日期)==year(time_span)
                ) %>% 
                mutate(
                        销售姓名=S.销售姓名,
                        销售类别=fct_collapse(销售姓名,
                                              刘艳=c('刘艳'),
                                              大客户=c('大客户','周冰'),
                                              华北大客户=c('华北大客户','杨宝兴','樊伟','贾东东'),
                                              西南大客户=c('西南大客户','张义凯'),
                                              other_level=c('销售专员')),
                        消费金额=if_else(一级任务类别=='预付款消费代理',S.消费金额-Ag.外包成本,S.消费金额)
                ) %>% 
                distinct(任务ID,.keep_all=T) %>% #台账合并tb数据时会造成TB字段重复
                group_by(一级任务类别,销售类别,销售姓名,time_filter(A.合同签订日期)) %>% 
                summarise(
                        消费金额=sum(S.消费金额,na.rm = T),
                        代理成本=sum(Ag.外包成本,na.rm = T)
                )
        
        
        dt1 <- purrr::reduce(list(invoice.dt,payment.dt,advance.dt),full_join,by = c("一级任务类别", "销售类别", "销售姓名") )%>% 
                group_by(一级任务类别, 销售类别, 销售姓名) %>% 
                summarise(
                        开具金额=sum(开具金额,na.rm = T),
                        代理成本.开具=sum(代理成本.x,na.rm = T),
                        回款金额=sum(回款金额,na.rm = T),
                        代理成本.回款=sum(代理成本.y,na.rm = T),
                        消费金额=sum(消费金额,na.rm = T),
                        代理成本=sum(代理成本,na.rm = T)
                ) %>% 
                mutate(
                        #across(matches('金额|成本'),replace_na,replace=0),
                        开票提成=pmap(list(销售类别,一级任务类别,开具金额,消费金额,代理成本),
                                  function(销售类别,一级任务类别,开具金额,消费金额,代理成本){
                                          p <- 0
                                          if(销售类别=='销售专员'|str_detect(销售类别,'大客户')){
                                                  p <- switch(一级任务类别,
                                                                    '常规业务'= 开具金额*0.03,
                                                                    '预付款'= 开具金额*0,
                                                                    '预付款消费' = 消费金额*0.04,
                                                                    '预付款消费代理'=(消费金额-代理成本)*1.8*0.04,
                                                                    '代理'=开具金额*1.8*0.03,
                                                                    '其他'=开具金额*0,
                                                                    '大综合'=开具金额*1.8*0.06
                                                  )
                                          }else if(销售类别=='刘艳'){
                                                  p <- switch(一级任务类别,
                                                                    '常规业务'= 开具金额*0.02,
                                                                    '预付款'= 开具金额*0,
                                                                    '预付款消费' = 消费金额*0.03,
                                                                    '预付款消费代理'=(消费金额-代理成本)*1.8*0.04,
                                                                    '代理'=开具金额*1.8*0.02,
                                                                    '其他'=开具金额*0,
                                                                    '大综合'=开具金额*1.8*0.04
                                                  )
                                          }
                                          p
                                  }),
                        回款提成=pmap(list(销售类别,一级任务类别,回款金额,消费金额),
                                  function(销售类别,一级任务类别,回款金额,消费金额){
                                          p <- 0
                                          if(销售类别=='销售专员'|str_detect(销售类别,'大客户')){
                                                  p <- switch(一级任务类别,
                                                                    常规业务=回款金额*0.03,
                                                                    预付款=回款金额*0.03,
                                                                    预付款消费=消费金额*0,
                                                                    大综合=回款金额*1.8*0.06,
                                                                    预付款消费代理=回款金额*0,
                                                                    代理=回款金额*1.8*0.03,
                                                                    其他=回款金额*0)
                                          }else if(销售类别=='刘艳'){
                                                  p <- switch(一级任务类别,
                                                                    常规业务=回款金额*0.02,
                                                                    预付款=回款金额*0.02,
                                                                    预付款消费=消费金额*0,
                                                                    大综合=回款金额*1.8*0.04,
                                                                    预付款消费代理=回款金额*0,
                                                                    代理=回款金额*1.8*0.02,
                                                                    其他=回款金额*0)
                                          }
                                          p
                                  })
                )
        
        #单独计算梁敏提成
        dt2 <- 
        dt1 %>% 
                group_by(一级任务类别) %>% 
                summarise(
                        开具金额=sum(开具金额,na.rm = T),
                        消费金额=sum(消费金额,na.rm = T),
                        代理成本=sum(代理成本,na.rm = T)
                ) %>% 
                mutate(
                        开票提成=pmap(list(一级任务类别,开具金额,消费金额,代理成本),
                                  function(一级任务类别,开具金额,消费金额,代理成本){
                                          switch(
                                                  一级任务类别,
                                                        常规业务=开具金额*0.01,
                                                        预付款消费=消费金额*0.01,
                                                        代理=开具金额*1.8*0.01,
                                                        预付款消费代理=(消费金额-代理成本)*1.8*0.01,
                                                        预付款=0,
                                                        其他=0,
                                                        大综合=0)
                                  }
                        ),
                        销售姓名='梁敏'
                ) 
        
        dt.fin <- 
                bind_rows(dt1,dt2)
        
        
        output <- 
                list(
                        '汇总结果'=dt.fin,
                        '原始数据'=dt.all
                )
        #write.xlsx(output,'4月统计结果.xlsx')
}





