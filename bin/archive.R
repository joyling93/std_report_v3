
# filename <- dir('/Users/zhuomingx/Desktop/Rbio/std_report_v3/debug/test/信息表归档测试','.xlsx')
# filepath <- dir('/Users/zhuomingx/Downloads/info_table_test','.xlsx',full.names = T)
# archive_files(
#         filepath=filepath,
#         filename=filename,
#         db=db
# )
# dbReadTable(db,'分子信息表')
# db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')

archive_files <- function(type,filepath,db){
        out_info <- '请选择归档文件类型'
        out_info <- switch (type,
                            'TB任务' = tb_db(filepath,db),
                            'TB任务2020' = tb_db2(filepath,db),
                            '实验记录（信息）表' = record_db(filepath,db),
                            '成本和产值统计辅助表' = supp_db(filepath,db),
                            '台账' = ledger_db(filepath,db),
                            '企管校正表' = adjust_db(filepath,db)
        )
        return(out_info)
}

supp_db <- function(filepath,db){
        out_info <- '归档成功'
        file_path <- filepath[str_detect(filepath,'\\.xlsx|\\.xls')]
        
        sheet.list <- names(loadWorkbook(file_path))
        
        dt.list <- 
                purrr::map(sheet.list,function(sheet){
                        read.xlsx(file_path,sheet=sheet,detectDates = T) %>% 
                                mutate(日期=as.character(日期))
                })
        names(dt.list) <- sheet.list
        db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
        purrr::walk2(dt.list,sheet.list,function(dt,name){
                dbWriteTable(db,name,dt,overwrite=T)
        })
        dbDisconnect(db)
        out_info
}

tb_db <- function(filepath,db){
        out_info <- '归档成功'
        dt <- loadWorkbook(filepath)
        # names(dt)
        dt_list <- 
                purrr::map(names(dt),function(x){
                y <- readWorkbook(dt,sheet=x,
                                  skipEmptyCols=T)
                y$import_time <- as.double(Sys.time())
                colnames(y) <- str_replace_all(colnames(y),'\\"','')# 去除标题中多余“号
                colnames(y) <- str_replace_all(colnames(y),'[-()（）]','.')# 转化标题中-为.
                y <- y %>% 
                        select(!where(~all(is.na(.x))))
        })
        dt_fin <- dbReadTable(db,'db') %>% 
                bind_rows(dt_list) %>% 
                arrange(desc(import_time)) %>% 
                filter(!duplicated(任务ID))
        
        ##预处理一些遗留问题
        dt_fin <- 
        dt_fin %>% mutate(CD.产能类型 = map_chr(CD.产能类型,str_replace_all,pattern='拼装和酶切连接，LR载体',
                                            replacement='拼装和酶切连接；LR载体'))
        
        dbWriteTable(db,'db',dt_fin,overwrite=T)
        return(out_info)
}


tb_db2 <- function(filepath,db){
        if(str_detect(filepath,fixed('.csv'))){
                dt <- read_csv(filepath,col_types = cols(
                        b4消费金额 = col_character()
                ))
                subtype <- 
                function(type){
                        dt_sub <- dt %>% 
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
                
                dt_sale <- dt %>% 
                        select(!contains(c('分子','病毒','细胞'))) %>% 
                        drop_na() %>% 
                        rename(S.合同金额 = b4合同金额,
                               S.消费金额 = b4消费金额,
                               A.方案设计者 = 方案设计者,
                               D.方案设计延期 = 方案设计延期,
                               A.方案指派日期 = d5总开始) %>% 
                        mutate(
                                任务类型 = '销售序列模板',
                                是否是子任务 = 'N',
                        )
                
                dt_fin <- bind_rows(dt_product,dt_sale) %>% 
                        mutate(
                                import_time = as.double(Sys.time()),
                                CD.子产能 = as.character(CD.子产能),
                                S.合同金额 = as.character(S.合同金额),
                                任务ID = tolower(任务ID),
                                CD.组成产能 = CD.子产能,
                                CD.产能类型 = '2020旧项目'
                               ) %>% 
                        mutate(across(where(is.POSIXt),as.character)) %>% 
                        bind_rows(dbReadTable(db,'db')) %>% 
                        arrange(desc(import_time)) %>% 
                        filter(!duplicated(任务ID))

                out_info <- '归档成功'
                dbWriteTable(db,'db',dt_fin,overwrite=T)
                return(out_info)
        }
        else{
                out_info <- '归档失败，检查文件是否为csv格式。'
        }

}

record_db <- 
function(filepath,db){
        file_path <- filepath[str_detect(filepath,'\\.xlsx|\\.xls')]
        
        dt_new <- purrr::map_dfr(file_path,function(x){
                dt <- readxl::read_xlsx(x,col_types = "text")%>% 
                        mutate(import.time=as.numeric(Sys.time()))
                if('载体编号'%in%colnames(dt)){
                        colnames(dt)[match('载体编号',colnames(dt))] <- '载体'
                }else if('载体信息'%in%colnames(dt)){
                        colnames(dt)[match('载体信息',colnames(dt))] <- '载体'
                }
                dt
        })
        colnames(dt_new) <- str_replace_all(colnames(dt_new),'[:punct:]','.')
        
        db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
        dt_db <- dbReadTable(db,'exp_info')
        
        dt_new <- 
                dt_new %>% 
                select(any_of(colnames(dt_db)))
        
        dt_fin <-
        bind_rows(dt_new,dt_db) %>%
                arrange(desc(import.time)) %>%
                dplyr::filter(!duplicated(paste0(生产主任务标题,载体)))
        
        
        dbWriteTable(db,'exp_info',dt_fin,overwrite=T)
        dbDisconnect(db)
        out_info <- '归档成功'
}

ledger_db <- 
function(filepath,db){
        file_path <- filepath[str_detect(filepath,'\\.xlsx')]
        ledger_raw <- openxlsx::read.xlsx(filepath,detectDates=T,sheet='原始数据-台账') %>% 
                mutate(
                        开票日期=if_else(str_detect(开票日期,'0000/0/0'),NA_character_,开票日期),
                        across(matches('日期$'),as.Date)
                )
        db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
        dbWriteTable(db,'ledger_raw',ledger_raw,overwrite=T)
        dbDisconnect(db)
        out_info <- '归档成功'
}

adjust_db <- 
        function(filepath,db){
                file_path <- filepath[str_detect(filepath,'\\.xlsx')]
                adjust_raw <- openxlsx::read.xlsx(filepath,detectDates=T,sheet='原始数据') 
                db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
                dbWriteTable(db,'adjust_raw',adjust_raw,overwrite=T)
                dbDisconnect(db)
                out_info <- '归档成功'
        }


