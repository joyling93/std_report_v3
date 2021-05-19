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
                "pageSize"= 100,
                "pageToken"= pageToken,
                "orderBy"= "dueDate"
)

result <- POST(url,
               add_headers(.headers = headers),
               body = payload,encode = 'json')

dt_list <- content(result)[[5]]
