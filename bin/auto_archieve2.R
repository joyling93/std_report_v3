# library(httr)
# library(reticulate)
# load('./data/cf_phrase')
# sns <- import('jwt')
# 2020旧项目统计
auto_archieve2 <- function(){
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
        
        payload <- list("tql"= "_projectId=58081fe94863251f4269aaf3 AND _tasklistId=5dedbcd453b99f0020ec76aa OR _tasklistId=5ce122f34f895a001991ae12 isArchived = false",
                        "pageSize"= 1000,
                        "pageToken"= pageToken
        )
        # payload <- list("tql"= "_projectId=58081fe94863251f4269aaf3 AND _tasklistId=5dedbcd453b99f0020ec76aa isArchived = false",
        #                 "pageSize"= 1000,
        #                 "pageToken"= pageToken
        # )
        #5ce122f34f895a001991ae12
        result <- POST(url,
                       add_headers(.headers = headers),
                       body = payload,encode = 'json')
        
        dt_list <- content(result)[[5]]
        
        pageToken <- content(result)[[4]]
        
        #持续请求数据直到 pageToken 返回为空
        while (pageToken!='') {
                payload <- list("tql"= "_projectId=58081fe94863251f4269aaf3 AND _tasklistId=5dedbcd453b99f0020ec76aa OR _tasklistId=5ce122f34f895a001991ae12 isArchived = false",
                                "pageSize"= 1000,
                                "pageToken"= pageToken
                )
                
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
                                        uniqueId=paste0('DS-',uniqueId)#补齐任务ID前缀
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
        
        dt_product <- map_dfr(c('分子','病毒','细胞'),subtype) %>% 
                mutate(任务ID = paste0(任务ID,CD.子任务类型))#虚拟任务ID
        
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
        
        dt_new <- bind_rows(dt_product,dt_sale) %>% 
                mutate(
                        import.time = as.numeric(Sys.time()),
                        CD.子产能 = as.character(CD.子产能),
                        S.合同金额 = as.character(S.合同金额),
                        任务ID = tolower(任务ID),
                        CD.组成产能 = CD.子产能,
                        CD.产能类型 = if_else(is.na(X1.基因合成载体产能),'2020旧项目','基因合成载体')##标记旧项目中基因合成项目
                        ) %>% 
                select(-X1.基因合成载体产能)
                 
        
        db <- DBI::dbConnect(SQLite(),dbname='data/testDB.db')
        dt_db <- dbReadTable(db,'db') 
        
        #合并数据库，以导入时间排序，去除旧数据
        dt_fin <- 
                bind_rows(dt_new,dt_db) %>% 
                arrange(desc(import.time)) %>% 
                dplyr::filter(!duplicated(任务ID)) 
        
        dbWriteTable(db,'db',dt_fin,overwrite=T)
        dbDisconnect(db)
        print('2020旧任务自动归档成功')
}

