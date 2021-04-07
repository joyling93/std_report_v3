#自动归档2021tb新流程任务,任务ID=='fw-1287'
auto_archieve <- function(){
        tql.query <- "_projectId=5fd6c35b083cba2bde5df319 AND isArchived = false"
        uniqueId.prefix <- "fw-"
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
                           任务.ObjectId=taskId) %>% 
                #将任务类型id转化为具体任务类型名称
                dplyr::filter(任务类型%in%c('5fd9bae5f0f303707a50a6f5',
                                        '5fd9b4421e0cc70378ce5d94',
                                        '5fdac5257155aa69e4230c0c',
                                        '601376d93921d3e46ca2273b',
                                        '601388f10f3155092eb4fb67')) %>% 
                mutate(
                        任务类型=fct_recode(as.factor(任务类型),
                                        生产序列模板='5fd9bae5f0f303707a50a6f5',
                                        销售序列模板='5fd9b4421e0cc70378ce5d94',
                                        售后序列模板='5fdac5257155aa69e4230c0c',
                                        研发任务模板='601376d93921d3e46ca2273b',
                                        研发执行任务模板='601388f10f3155092eb4fb67')
                )
        
        db <- DBI::dbConnect(SQLite(),dbname='data/testDB.db')
        cf_phrase <- dbReadTable(db,'cf_phrase')
        dt_db <- dbReadTable(db,'db')
        
        #以预制cfid对照表转化customfields为字段名称
        for (i in 1:length(colnames(dt_new))) {
                if(colnames(dt_new)[i]%in%cf_phrase$customfildID){
                        colnames(dt_new)[i] <- subset(cf_phrase,customfildID==colnames(dt_new)[i]) %>% 
                                pull(name)
                }
        }
        
        dt_new <- 
                dt_new %>% 
                select(!matches('^\\d')) %>% #去除没有转化名称的customfields列
                mutate(
                       CD.产能类型 = map_chr(CD.产能类型,str_replace_all,pattern='拼装和酶切连接，LR载体',
                                                replacement='拼装和酶切连接；LR载体'),
                       import.time = as.numeric(Sys.time())
                       )

        #合并数据库，以导入时间排序，去除旧数据
        dt_fin <- 
        bind_rows(dt_new,dt_db) %>% 
                arrange(desc(import.time)) %>% 
                dplyr::filter(!duplicated(任务ID)) 
        
        dbWriteTable(db,'db',dt_fin,overwrite=T)
        dbDisconnect(db)
        print('2021新流程自动归档成功')
}


