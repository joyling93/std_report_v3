tql_query <- 
function(tql.query,uniqueId.prefix){
        library(httr)
        library(reticulate)
        #use_virtualenv('test', required = FALSE)
        sns <- import('jwt')
        encoded <-  sns$encode(list(
                '_appId'= "659d34d0a46d44c17215c291",#appid
                'iat'= as.numeric(Sys.time()),
                'exp'= as.numeric(Sys.time()) + 7200), "UACb9VA9J2IqTfVSEjtyOW8pT7e5fH6D")#app密码
        
        headers <- c('Content-Type' = 'application/json',
                     'Authorization' = paste0('Bearer ',encoded),
                     'X-Tenant-Id' = '600e5711f2865f35d6ad662c',#企业编号
                     'X-Tenant-Type' = 'organization')
        url <- 'https://open.teambition.com/api/v3/project/5fd6c35b083cba2bde5df319/task/query'
        
        pageToken <- ''

        result <- GET(url,add_headers(.headers = headers),
                      query=list(q=tql.query,pageSize='1000',pageToken=pageToken),
                      encode = 'json')
        
        dt_list <- content(result)[[6]]
        
        pageToken <- content(result)[[5]]
        
        #持续请求数据直到 pageToken 返回为空
        while (pageToken!='') {
                result <- GET(url,add_headers(.headers = headers),
                              query=list(q=tql.query,pageSize='1000',pageToken=pageToken),
                              encode = 'json')
                
                dt_list <- append(dt_list,content(result)[[6]])
                
                pageToken <- content(result)[[5]]
        }
        
        dt <- 
                map(dt_list,possibly(function(dt_list){
                        test <- transpose(dt_list$customfields)#转置customfields为cfid，value列
                        if(length(dt_list$ancestorIds)>0){
                                dt_list['ancestorIds']<-dt_list$ancestorIds#处理列表列  
                        }else{
                                dt_list['ancestorIds']<-NA
                        }
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
                                                    'startDate','sfcId','uniqueId',
                                                    'ancestorIds','id')]) %>% 
                                mutate(
                                        是否是子任务=if_else(is.na(ancestorIds),#判断parentTaskId存在确定任务是否为子任务
                                                       'N','Y'),
                                        uniqueId=paste0(uniqueId.prefix,uniqueId)#补齐任务ID前缀
                                )
                },NULL
                ))
}