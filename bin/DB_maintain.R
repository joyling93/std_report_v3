
library(RSQLite)
library(readxl)
db <- DBI::dbConnect(SQLite(),dbname='/Users/zhuomingx/Desktop/Rbio/std_report_v3/data/testDB.db')
dbListTables(db)

dt_fin <- dbReadTable(db,'材料、基因合成、测序等') 

file_p <- '/Users/zhuomingx/Desktop/Rbio/std_report_v3/debug/test/全任务归档测试.xlsx'
dt <- openxlsx::loadWorkbook(file_p)
names(dt)
dt_list <-
purrr::map_dfr(names(dt),function(x){
        readxl::read_excel(file_p,sheet = x,col_types = 'text')
})
dt_list$import.time <- as.numeric(Sys.time())
colnames(dt_list) <- str_replace_all(colnames(dt_list),'\\"','')# 去除标题中多余“号
colnames(dt_list) <- str_replace_all(colnames(dt_list),'[[:punct:]]','.')# 转化标题中-为.

dt_list <- 
mutate(dt_list,CD.产能类型 = map_chr(CD.产能类型,str_replace_all,pattern='拼装和酶切连接，LR载体',
                         replacement='拼装和酶切连接；LR载体'))


dbWriteTable(db,'db',dt_list,overwrite=T)
dbDisconnect(db)
# #往期数据导入
# file_p2 <- '/Users/zhuomingx/Desktop/Rbio/std_report_v3/debug/test/2020任务归档测试.csv'
# 
# dt <- read.csv("~/Desktop/Rbio/std_report_v3/debug/test/2020任务归档测试.csv",
#                colClasses = 'character')
# subtype <- 
#         function(type){
#                 dt_sub <- dt %>% 
#                         select(任务ID,标题,contains(type)) %>% 
#                         drop_na() %>% 
#                         rename(
#                                 CE.实验执行人姓名 = paste0('g5',type,'姓名'),
#                                 截止时间 = paste0('d5',type,'截止'),
#                                 Su.实验实际开始日期 = paste0('d5',type,'开始'),
#                                 Su.实验实际完成日期 = paste0('g5',type,'完成'),
#                                 CD.子产能 = paste0('d5',type,'产能')
#                         ) %>% 
#                         mutate(
#                                 开始时间 = Su.实验实际开始日期,
#                                 CD.子任务类型 = type,
#                                 任务类型 = '生产序列模板',
#                                 是否是子任务 = 'Y',
#                                 标题 = paste0(任务ID,'-',标题),
#                         ) 
#         }
# 
# dt_product <- map_dfr(c('分子','病毒','细胞'),subtype) %>% 
#         mutate(任务ID = paste0(任务ID,CD.子任务类型))#虚拟任务ID
# 
# dt_sale <- dt %>% 
#         select(!contains(c('分子','病毒','细胞'))) %>% 
#         drop_na() %>% 
#         rename(S.合同金额 = b4合同金额,
#                S.消费金额 = b4消费金额,
#                A.方案设计者 = 方案设计者,
#                D.方案设计延期 = 方案设计延期,
#                A.方案指派日期 = d5总开始) %>% 
#         mutate(
#                 任务类型 = '销售序列模板',
#                 是否是子任务 = 'N',
#         )
# 
# dt_fin <- bind_rows(dt_product,dt_sale) %>% 
#         mutate(
#                 import.time = as.numeric(Sys.time()),
#                 CD.子产能 = as.character(CD.子产能),
#                 S.合同金额 = as.character(S.合同金额),
#                 任务ID = tolower(任务ID),
#                 CD.组成产能 = CD.子产能,
#                 CD.产能类型 = '2020旧项目'
#         ) %>% 
#         bind_rows(dbReadTable(db,'db')) %>% 
#         arrange(desc(import.time)) %>% 
#         dplyr::filter(!duplicated(任务ID))
# 
# out_info <- '归档成功'
# dbWriteTable(db,'db',dt_fin,overwrite=T)
# dbDisconnect(db)

##cf_phrase
cf_phrase <- dbReadTable(db,'cf_phrase')
##update db
dbExecute(db,"UPDATE cf_phrase SET customfildID = '6041950b02a7659ab67d8ab1' WHERE name = 'S.消费金额'")
dbExecute(db,"UPDATE cf_phrase SET name = 'Ag.整体实验售价' WHERE customfildID = '609a51283b6100d8db6a8a69'")
##删除值

##插入值('6057f3ede2cbb21aa9fda492','延期原因')
tribble(
        ~id,~name,
        '60acbccfb8bb4bc40bd67660','MD.包装细胞盘数'
        
) %>% 
        pwalk(function(id,name){
                dbExecute(db,paste0("INSERT INTO cf_phrase (customfildID,name) VALUES ('",id,"'",",","'",name,"')"))
        })



#辅助表读取
temp <- dir('/Users/zhuomingx/Desktop/Rbio/std_report_v3/debug/test/'
            ,'^企管输入.xlsx',full.names = T)
sheet.list <- names(openxlsx::loadWorkbook(temp))
date.input <- 1
supp.dt <- 
        map(sheet.list,function(sheet){
                openxlsx::read.xlsx(temp,sheet=sheet,detectDates = T) %>% 
                        dplyr::filter(month(日期)==date.input) %>% 
                        dplyr::select(-日期)
        })
names(supp.dt) <- sheet.list
enframe(supp.dt) %>% 
        pwalk(function(name,value){
                dbWriteTable(db,name,value,overwrite=T)
        })
dbDisconnect(db)
