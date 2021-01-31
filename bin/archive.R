
# filename <- dir('./debug/dbtest',pattern = '信息表.xlsx')
# filepath <- dir('./debug/dbtest',pattern = '信息表.xlsx',full.names = T)
# archive_files(
#         filepath=filepath,
#         filename=filename,
#         db=db
# )
# dbReadTable(db,'分子信息表')

archive_files <- function(type,filepath,db){
        out_info <- '请选择归档文件类型'
        out_info <- switch (type,
                            '生产任务' = product_db(filepath,db),
                            '销售任务' = sale_db(filepath,db),
                            '记录表' = record_db(filepath,db)
        )
        return(out_info)
}

product_db <- function(filepath,db){
        dt_old <- dbReadTable(db,'product_db')
        dt_new <- bind_rows(purrr::map(filepath,read.csv)) %>% 
                mutate(import_time=Sys.time())
        if(all(colnames(dt_old)%in%colnames(dt_new))){
                # 检查子任务中包含缺失值得行
                cols_contain_na <- 
                        dt_new %>% 
                        filter(是否是子任务=='Y') %>% 
                        select(where(~ any(is.na(.x)))) %>% 
                        colnames()
                cols_na_allowed <- c('MD.总产能','CW.入库日期')
                if(all(cols_contain_na%in%cols_na_allowed)){
                        dbWriteTable(db,'product_db',dt_new,append = TRUE)
                        out_info <- '归档成功'
                }else{
                        cols_na_illigal <- cols_contain_na[!(cols_contain_na%in%cols_na_allowed)]
                        rows_na_illigal <- dt_new %>% 
                                select(任务ID,cols_na_illigal) %>% 
                                drop_na() %>% 
                                pull(任务ID)
                        rows_na_illigal <- dt_new$任务ID[!dt_new$任务ID%in%rows_na_illigal]
                        out_info <- paste0('归档失败。归档文件中，表头：',
                                           str_c(cols_na_illigal,collapse = '，'),
                                           '；任务：',
                                           str_c(rows_na_illigal,collapse = '，'),
                                           ' 包含未填缺失项。'
                                           )
                }
        }else{
                miss_info <- str_c(colnames(dt_old)[!colnames(dt_old)%in%colnames(dt_new)],collapse = '，')
                out_info <- paste0('归档失败，归档文件表头与数据库不匹配，缺失字段：',miss_info)
        }
        return(out_info)
}

sale_db <- function(filepath,db){
        dt_old <- dbReadTable(db,'sale_db')
        dt_new <- bind_rows(purrr::map(filepath,read.csv)) %>% 
                mutate(import_time=Sys.time())
        if(all(colnames(dt_old)%in%colnames(dt_new))){
                dbWriteTable(db,'sale_db',dt_new,append = TRUE)
                out_info <- '归档成功'
        }else{
                miss_info <- str_c(colnames(dt_old)[!colnames(dt_old)%in%colnames(dt_new)],collapse = '，')
                out_info <- paste0('归档失败，归档文件表头与数据库不匹配，缺失字段：',miss_info)
        }
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

