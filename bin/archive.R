
# filename <- dir('./debug/test',pattern = '信息表.xlsx')
# filepath <- dir('./debug/test',pattern = '全任务归档测试.xlsx',full.names = T)
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
                        
                            '记录表' = record_db(filepath,db)
        )
        return(out_info)
}

tb_db <- function(filepath,db){
        out_info <- '归档成功'
        dt <- openxlsx::loadWorkbook(filepath)
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
        # dt_old <- dbReadTable(db,'db')
        # dt_new <- bind_rows(purrr::map(filepath,read.csv)) %>% 
        #         mutate(import_time=Sys.time())
        # if(all(colnames(dt_old)%in%colnames(dt_new))){
        #         # 检查子任务中包含缺失值得行
        #         cols_contain_na <- 
        #                 dt_new %>% 
        #                 filter(是否是子任务=='Y') %>% 
        #                 select(where(~ any(is.na(.x)))) %>% 
        #                 colnames()
        #         cols_na_allowed <- c('MD.总产能','CW.入库日期')
        #         if(all(cols_contain_na%in%cols_na_allowed)){
        #                 dbWriteTable(db,'product_db',dt_new,append = TRUE)
        #                 out_info <- '归档成功'
        #         }else{
        #                 cols_na_illigal <- cols_contain_na[!(cols_contain_na%in%cols_na_allowed)]
        #                 rows_na_illigal <- dt_new %>% 
        #                         select(任务ID,cols_na_illigal) %>% 
        #                         drop_na() %>% 
        #                         pull(任务ID)
        #                 rows_na_illigal <- dt_new$任务ID[!dt_new$任务ID%in%rows_na_illigal]
        #                 out_info <- paste0('归档失败。归档文件中，表头：',
        #                                    str_c(cols_na_illigal,collapse = '，'),
        #                                    '；任务：',
        #                                    str_c(rows_na_illigal,collapse = '，'),
        #                                    ' 包含未填缺失项。'
        #                                    )
        #         }
        # }else{
        #         miss_info <- str_c(colnames(dt_old)[!colnames(dt_old)%in%colnames(dt_new)],collapse = '，')
        #         out_info <- paste0('归档失败，归档文件表头与数据库不匹配，缺失字段：',miss_info)
        # }
        return(out_info)
}


record_db <- 
function(filename,filepath,db){
        if(any(stringr::str_detect(filename,'分子实验记录表'))){
                filelist <- filepath[stringr::str_which(filename,'分子实验记录表')]
                dt <- bind_rows(purrr::map(filelist,openxlsx::read.xlsx)) %>% 
                        mutate(import_time=Sys.time())
                dbWriteTable(db,'分子实验记录表',dt,append = TRUE)
        }
        if(any(stringr::str_detect(filename,'病毒实验记录表'))){
                filelist <- stringr::str_subset(filepath,'病毒实验记录表')
                dt <- bind_rows(purrr::map(filelist,openxlsx::read.xlsx)) %>% 
                        mutate(import_time=Sys.time())
                dbWriteTable(db,'病毒实验记录表',dt,append = TRUE)
        }
        if(any(stringr::str_detect(filename,'细胞实验记录表'))){
                filelist <- stringr::str_subset(filepath,'细胞实验记录表')
                dt <- bind_rows(purrr::map(filelist,openxlsx::read.xlsx)) %>% 
                        mutate(import_time=Sys.time())
                dbWriteTable(db,'细胞实验记录表',dt,append = TRUE)
        }
        out_info <- '归档成功'
}

