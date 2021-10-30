
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
        '601cc91d2daa3dd3bdb4007d','Ag.外包成本'
        
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

cf_new <- 
        tibble::tribble(
                ~customfildID,         ~name,
                "617176b413ea411257b7f2e1",      "b4消费金额",
                "617176b413ea411257b7f47d",      "d5分子开始",
                "617176b413ea411257b7f491",      "d5分子截止",
                "617176b413ea411257b7f4a8",      "d5分子产能",
                "617176b413ea411257b7f4c6",      "d5病毒开始",
                "617176b413ea411257b7f4e6",      "d5病毒截止",
                "617176b413ea411257b7f4f7",      "d5病毒产能",
                "617176b413ea411257b7f515",      "d5细胞开始",
                "617176b413ea411257b7f535",      "d5细胞截止",
                "617176b413ea411257b7f549",      "d5细胞产能",
                "617176b413ea411257b7f561",       "d5总开始",
                "617176b413ea411257b7f626",      "b4合同金额",
                "617176b413ea411257b7f641",      "g5分子完成",
                "617176b413ea411257b7f65a",      "g5分子姓名",
                "617176b413ea411257b7f670",      "g5病毒完成",
                "617176b413ea411257b7f682",      "g5病毒姓名",
                "617176b413ea411257b7f697",      "g5细胞完成",
                "617176b413ea411257b7f6b5",      "g5细胞姓名",
                "617176b413ea411257b812b9",      "方案设计延期",
                "617176b413ea411257b81310",       "方案设计者",
                "6171772e13ea411257cfa694",      "S.合同金额",
                "6171772e13ea411257cfab22",      "S.消费金额",
                "6171772e13ea411257cfa6c3",      "S.客户姓名",
                "6171772e13ea411257cfa6de",      "S.客户单位",
                "6171772e13ea411257cfa6f3",      "S.销售姓名",
                "6171772e13ea411257cfa708",    "S.是否新增客户",
                "6171772e13ea411257cfa72e",      "S.销售区域",
                "6171772e13ea411257cfa749",    "A.合同签订日期",
                "6171772e13ea411257cfa764",     "A.方案设计者",
                "6171772e13ea411257cfa777",    "A.方案指派日期",
                "6171772e13ea411257cfaa03",    "A.方案设计延期",
                "6171772e13ea411257cfa81e",    "CD.子任务类型",
                "6171772e13ea411257cfa854",      "CD.子产能",
                "6171772e13ea411257cfae47",     "CD.产能类型",
                "6171772e13ea411257cfa882",     "CD.组成产能",
                "6171772e13ea411257cfa898",  "CE.实验执行人姓名",
                "6171772e13ea411257cfa929",     "CW.入库日期",
                "6171772e13ea411257cfa949",  "P.结题报告是否规范",
                "6171772e13ea411257cfa9bb", "Su.实验实际开始日期",
                "6171772e13ea411257cfa9ce", "Su.实验实际完成日期",
                "6171772e13ea411257cfa9e6",  "Su.实验分配人姓名",
                "6171772e13ea411257cfaa51",    "P.实际完成时间",
                "6171772e13ea411257cfaabc",      "A.业务类别",
                "601cc7102745a7c6e1648b20", "X1.基因合成载体产能",
                "6171772e13ea411257cfab75",        "延期原因",
                "6171772e13ea411257cfaaa9",    "S.客户联系信息",
                "6171772e13ea411257cfaad7",   "S.预付款任务ID",
                "6171772e13ea411257cfa90f",        "S.折扣",
                "6171772e13ea411257cfab5b",        "S.促销",
                "6171772e13ea411257cfa982",     "Sp.是否退单",
                "6171772e13ea411257cfa788",      "A.变动金额",
                "6171772e13ea411257cfa7a6",       "F.已开票",
                "6171772e13ea411257cfa7c1",       "F.未回款",
                "6171772e13ea411257cfacff", "Ag.整体实验固定费用",
                "6171772e13ea411257cfad0e",   "Ag.整体实验售价",
                "6171772e13ea411257cfa8f6",      "W.出库日期",
                "617176b413ea411257b7f626",      "b4合同金额",
                "617176b413ea411257b7f235",      "a4联系方式",
                "617176b413ea411257b7f178",      "a4客户姓名",
                "617176b413ea411257b7f199",      "a4客户单位",
                "617176b413ea411257b7f1f5",      "a4销售姓名",
                "617176b413ea411257b7f1b9",      "a4新增客户",
                "617176b413ea411257b7f21a",      "a4销售区域",
                "617176b413ea411257b7f364",      "b4签订日期",
                "617176b413ea411257b7f31e",      "b4变动金额",
                "617176b413ea411257b7f38a",     "b4已开票金额",
                "617176b413ea411257b7f39e",      "b4未回款额",
                "617176b413ea411257b7f3ba",      "c6外包售价",
                "617176b413ea411257b7f3d0",      "c6外包成本",
                "617176b413ea411257b7f57c",      "e4发货日期",
                "617176b413ea411257b7f287",        "a4折扣",
                "617176b413ea411257b7f607",      "f5结题规范",
                "617176b413ea411257b7f6d6",       "f5总完成",
                "617176b413ea411257b7f344",      "b4业务类别",
                "617176b413ea411257b7f2f9",      "b4消费合同",
                "6171772e13ea411257cfad43",   "MD.包装细胞盘数",
                "617176b413ea411257b7ff5a",   "X1.基因合成载体产能"
        ) %>% 
        as.data.frame()

dbWriteTable(db,'cf_phrase',cf_new,overwrite=T)
dbDisconnect(db)


