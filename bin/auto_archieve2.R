#自动归档2020tb流程任务
auto_archieve2 <- function(){
        tql.query <- "_projectId=58081fe94863251f4269aaf3 AND _tasklistId=5dedbcd453b99f0020ec76aa OR _tasklistId=5ce122f34f895a001991ae12 isArchived = false"
        uniqueId.prefix <- "DS-"
        
        dt <- tql_query(tql.query,uniqueId.prefix)
        
        dt_new <- 
                bind_rows(discard(dt,is.null)) %>% 
                rename(创建时间=created,
                           标题=content,
                           截止时间=dueDate,
                           开始时间=startDate,
                           任务类型=templateId,
                           任务ID=uniqueId,
                           父任务.ObjectId=parentTaskId,
                           任务.ObjectId=taskId) 
        
        db <- DBI::dbConnect(SQLite(),dbname='data/testDB.db')
        cf_phrase <- dbReadTable(db,'cf_phrase')
        dbDisconnect(db)
        
        #以预制cfid对照表转化customfields为字段名称
        for (i in 1:length(colnames(dt_new))) {
                if(colnames(dt_new)[i]%in%cf_phrase$customfildID){
                        colnames(dt_new)[i] <- subset(cf_phrase,customfildID==colnames(dt_new)[i]) %>% 
                                pull(name)
                }
        }
        
        dt_new <- 
                dt_new %>% 
                select(!matches('^\\d')) #去除没有转化名称的customfields列
        
        subtype <- 
                function(type){
                        dt_sub <- dt_new %>% 
                                select(任务ID,标题,contains(type)) %>% 
                                drop_na() %>% 
                                rename(
                                        CE.实验执行人姓名 = paste0('g5',type,'姓名'),
                                        截止时间 = paste0('d5',type,'截止'),
                                        Su.实验实际开始日期 = paste0('d5',type,'开始'),
                                        Su.实验实际完成日期 = paste0('g5',type,'完成'),
                                        CD.子产能 = paste0('d5',type,'产能')
                                ) %>% 
                                mutate(
                                        开始时间 = Su.实验实际开始日期,
                                        CD.子任务类型 = type,
                                        任务类型 = '生产序列模板',
                                        是否是子任务 = 'Y',
                                        标题 = paste0(任务ID,'-',标题),
                                ) 
                }
        
        #构造虚拟二级生产子任务
        dt_product <- map_dfr(c('分子','病毒','细胞'),subtype) %>% 
                mutate(任务ID = paste0(任务ID,CD.子任务类型),#虚拟任务ID
                         link=str_match(标题,'FW-\\d+'))#提取旧合同号作关联任务链接
        
        dt_product <- 
        dt_new %>% 
                select(X1.基因合成载体产能,标题) %>% 
                mutate(
                        link=str_match(标题,'FW-\\d+'),
                        CD.产能类型 = '基因合成载体'##标记旧项目中基因合成项目
                       ) %>% 
                drop_na() %>%
                select(CD.产能类型,link) %>% 
                right_join(dt_product) %>% 
                mutate(CD.产能类型=if_else(CD.子任务类型=='分子',CD.产能类型,'2020旧项目')) %>% 
                replace_na(list(CD.产能类型='2020旧项目'))#将所有非分子的基因合成载体任务转化为’旧项目‘
        
        #构造虚拟一级任务
        dt_sale <- dt_new %>% 
                select(!contains(c('分子','病毒','细胞'))) %>% 
                #drop_na() %>% 
                rename(S.合同金额 = b4合同金额,
                       S.消费金额 = b4消费金额,
                       A.方案设计者 = 方案设计者,
                       D.方案设计延期 = 方案设计延期,
                       A.方案指派日期 = d5总开始) %>% 
                mutate(
                        任务类型 = '销售序列模板',
                        是否是子任务 = 'N',
                )
        
        dt_update <- bind_rows(dt_product,dt_sale) %>% 
                mutate(
                        import.time = as.numeric(Sys.time()),
                        任务ID = tolower(任务ID),
                        CD.组成产能 = CD.子产能,
                        ) %>% 
                select(-X1.基因合成载体产能,-link)
                 
        
        db <- DBI::dbConnect(SQLite(),dbname='data/testDB.db')
        dt_db <- dbReadTable(db,'db') 
        
        #合并数据库，以导入时间排序，去除旧数据
        dt_fin <- 
                bind_rows(dt_update,dt_db) %>% 
                arrange(desc(import.time)) %>% 
                dplyr::filter(!duplicated(任务ID)) 
        
        dbWriteTable(db,'db',dt_fin,overwrite=T)
        dbDisconnect(db)
        print('2020旧任务自动归档成功')
}

