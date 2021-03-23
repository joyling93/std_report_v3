
library(httr)
library(reticulate)

#load('./data/cf_phrase')
#use_virtualenv('test', required = FALSE)
sns <- import('jwt')
auto_archieve <- function(){
        encoded <-  sns$encode(list(
                '_appId'= "60497fd21101c251cd202969",#appid
                'iat'= as.numeric(Sys.time()),
                'exp'= as.numeric(Sys.time()) + 3600), "Z5vpPJj0b7QNrihJsGVCTeDXzsPGK0j0")#app密码
        
        headers <- c('Content-Type' = 'application/json',
                     'Authorization' = paste0('Bearer ',encoded),
                     'X-Tenant-Id' = '600e5711f2865f35d6ad662c',#企业编号
                     'X-Tenant-Type' = 'organization')
        url <- 'https://open.teambition.com/api/task/tqlsearch'
        
        pageToken <- ''
        payload <- list("tql"= "_projectId=5fd6c35b083cba2bde5df319 AND isArchived = false",
                        "pageSize"= 100,
                        "pageToken"= pageToken,
                        "orderBy"= "dueDate")
        
        result <- POST(url,
                       add_headers(.headers = headers),
                       body = payload,encode = 'json')
        
        dt_list <- content(result)[[5]]
        
        pageToken <- content(result)[[4]]
        
        #持续请求数据直到 pageToken 返回为空
        while (pageToken!='') {
                payload <- list("tql"= "_projectId=5fd6c35b083cba2bde5df319 AND isArchived = false",
                                "pageSize"= 100,
                                "pageToken"= pageToken,
                                "orderBy"= "dueDate")
                
                result <- POST(url,
                               add_headers(.headers = headers),
                               body = payload,encode = 'json')
                
                dt_list <- append(dt_list,content(result)[[5]])
                
                pageToken <- content(result)[[4]]
        }
        
        dt <- 
                map(dt_list,possibly(function(dt_list){
                        test <- transpose(dt_list$customfields)#转置customfields为cfid，value列
                        dt <- reduce(test,cbind) %>% #将所有customfields合并dataframe
                                as_tibble() %>% 
                                unnest(cols = c(out, elt, V3)) %>% 
                                mutate(V4=map_chr(V3,~.x[['title']])) %>% #取出customfields的值
                                select(out,V4) %>% 
                                distinct(out,.keep_all =T) %>% #对于文件类customfields，可能有多值情况
                                pivot_wider(names_from = out,values_from=V4) %>% #将所有customfields变为一行
                                bind_cols(dt_list[c('created','content','dueDate',
                                                    'startDate','templateId','uniqueId',
                                                    'parentTaskId','taskId')]) %>% 
                                mutate(
                                        是否是子任务=if_else(c("parentTaskId")%in%names(dt_list),#判断parentTaskId存在确定任务是否为子任务
                                                       'Y','N'),
                                        uniqueId=paste0('fw-',uniqueId)#补齐任务ID前缀
                                )
                },NULL
                ))
        
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


