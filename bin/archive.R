
# filename <- dir('./debug/dbtest',pattern = '信息表.xlsx')
# filepath <- dir('./debug/dbtest',pattern = '信息表.xlsx',full.names = T)
# archive_files(
#         filepath=filepath,
#         filename=filename,
#         db=db
# )
# dbReadTable(db,'分子信息表')
archive_files <- 
function(filename,filepath,db){
        if(any(stringr::str_detect(filename,'分子信息表'))){
                filelist <- filepath[stringr::str_which(filename,'分子信息表')]
                dt <- bind_rows(purrr::map(filelist,openxlsx::read.xlsx))
                dbWriteTable(db,'分子信息表',dt,append = TRUE)
        }
        if(any(stringr::str_detect(filename,'病毒信息表'))){
                filelist <- stringr::str_subset(filepath,'病毒信息表')
                dt <- bind_rows(purrr::map(filelist,openxlsx::read.xlsx))
                dbWriteTable(db,'病毒信息表',dt,append = TRUE)
        }
        if(any(stringr::str_detect(filename,'细胞信息表'))){
                filelist <- stringr::str_subset(filepath,'细胞信息表')
                dt <- bind_rows(purrr::map(filelist,openxlsx::read.xlsx))
                dbWriteTable(db,'细胞信息表',dt,append = TRUE)
        }
}

