
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
                            'TB任务2020' = tb_db2(filepath,db),
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
        return(out_info)
}


tb_db2 <- function(filepath,db){
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

