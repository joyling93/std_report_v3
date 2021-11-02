tql.query <- "_projectId=5fd6c35b083cba2bde5df319 AND isArchived = false"
#tql.query <-'_projectId=58081fe94863251f4269aaf3 AND _tasklistId=5dedbcd453b99f0020ec76aa OR _tasklistId=5ce122f34f895a001991ae12 isArchived = false'
uniqueId.prefix <- "fw-"
#dt <- tql_query(tql.query,uniqueId.prefix)
library(httr)
library(reticulate)
#use_virtualenv('test', required = FALSE)
sns <- import('jwt')
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

payload <- list("tql"= tql.query,
                "pageSize"= 10,
                "pageToken"= pageToken,
                "orderBy"= "dueDate"
)

result <- POST(url,
               add_headers(.headers = headers),
               body = payload,encode = 'json')

dt_list <- content(result)[[5]]

dt <- 
        map(dt_list,possibly(function(dt_list){
                test <- transpose(dt_list$customfields)#转置customfields为cfid，value列
                dt <- reduce(test,cbind) %>% #将所有customfields合并dataframe
                        as_tibble() %>% 
                        unnest(cols = c(out, elt, V3)) %>% 
                        mutate(V4=map_chr(V3,~.x[['title']])) %>% #取出customfields的值
                        select(out,V4) %>% 
                        chop(V4)%>%
                        mutate(V4=map_chr(V4,str_c,collapse='|'))%>%
                        #distinct(out,.keep_all =T) %>% #对于文件类customfields，可能有多值情况
                        pivot_wider(names_from = out,values_from=V4) %>% #将所有customfields变为一行
                        bind_cols(dt_list[c('created','content','dueDate',
                                            'startDate','templateId','uniqueId',
                                            'parentTaskId','taskId')]) %>% 
                        mutate(
                                是否是子任务=if_else(c("parentTaskId")%in%names(dt_list),#判断parentTaskId存在确定任务是否为子任务
                                               'Y','N'),
                                uniqueId=paste0(uniqueId.prefix,uniqueId)#补齐任务ID前缀
                        )
        },NULL
        ))

dt <- bind_rows(dt)



