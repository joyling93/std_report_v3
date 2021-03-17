
library(httr)
library(reticulate)

load('./data/cf_phrase')
#use_virtualenv('test', required = FALSE)
sns <- import('jwt')
auto_archieve <- function(){
        encoded <-  sns$encode(list(
                '_appId'= "60497fd21101c251cd202969",
                'iat'= as.numeric(Sys.time()),
                'exp'= as.numeric(Sys.time()) + 3600), "Z5vpPJj0b7QNrihJsGVCTeDXzsPGK0j0")
        
        headers <- c('Content-Type' = 'application/json',
                     'Authorization' = paste0('Bearer ',encoded),
                     'X-Tenant-Id' = '600e5711f2865f35d6ad662c',
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
                        test <- transpose(dt_list$customfields)
                        dt <- reduce(test,cbind) %>% 
                                as_tibble() %>% 
                                unnest(cols = c(out, elt, V3)) %>% 
                                mutate(V4=map_chr(V3,~.x[['title']])) %>% 
                                select(out,V4) %>% 
                                distinct(out,.keep_all =T) %>% #暂时测试
                                pivot_wider(names_from = out,values_from=V4) %>% 
                                bind_cols(dt_list[c('created','content','dueDate',
                                                    'startDate','templateId','uniqueId',
                                                    'parentTaskId','taskId')]) %>% 
                                mutate(
                                        是否是子任务=if_else(c("parentTaskId")%in%names(dt_list),
                                                       'Y','N'),
                                        uniqueId=paste0('fw-',uniqueId)
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
        
        for (i in 1:length(colnames(dt_new))) {
                if(colnames(dt_new)[i]%in%cf_phrase$V1){
                        colnames(dt_new)[i] <- subset(cf_phrase,V1==colnames(dt_new)[i]) %>% 
                                pull(V2)
                }
        }
        
        dt_new <- 
                dt_new %>% 
                select(!matches('^\\d')) %>%
                mutate(
                       CD.产能类型 = map_chr(CD.产能类型,str_replace_all,pattern='拼装和酶切连接，LR载体',
                                                replacement='拼装和酶切连接；LR载体'),
                       import.time = as.numeric(Sys.time())
                       )
        
        db <- DBI::dbConnect(SQLite(),dbname='data/testDB.db')
        dt_db <- dbReadTable(db,'db') 
        
        dt_fin <- 
        bind_rows(dt_new,dt_db) %>% 
                arrange(desc(import.time)) %>% 
                dplyr::filter(!duplicated(任务ID)) 
        
        dbWriteTable(db,'db',dt_fin,overwrite=T)
        dbDisconnect(db)
        print('自动归档成功')
}


