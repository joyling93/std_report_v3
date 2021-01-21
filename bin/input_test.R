 # file_name <- dir('debug/模板文件','分子实验记录表\\(2\\).xlsx')
 # file_path <- dir('debug/模板文件','分子实验记录表\\(2\\)',full.names = T)
 # project2 <- '载体构建'
 # table_type <- '分子实验记录表'
 # input_test(file_name,file_path,project2)


input_test <- 
function(file_name,file_path,project2){
        ##检测测试
        info <- '未自动检测到问题'
        
        col_test <- function(table_type){
                col_std <- colnames(openxlsx::read.xlsx(
                        dir('data/template',table_type,full.names = T)))
                
                test_result <- 
                        sapply(file_path[str_which(file_name,table_type)], function(x){
                                col_dt <- colnames(openxlsx::read.xlsx(x))
                                all(col_std%in%col_dt)}
                        )
                
                if(!all(test_result)){
                        info <- paste0('系统检测上传',
                                       table_type,
                                       '并非最新，建议下载上传最新模板(https://www.teambition.com/project/
                                       5fd6c35b083cba2bde5df319/works/5fd6c35b77deb9004478660c)')
                }
                info
        }
        
        if(project2=='载体构建'){
                if(!any(str_detect(file_name,'分子实验记录表'))){
                        info <- '系统检测未上传分子实验记录表'
                }
                info <- col_test('分子实验记录表')
        }else if(project2=='病毒包装'){
                if(!any(str_detect(file_name,'病毒实验记录表'))){
                        info <- '系统检测未上传病毒实验记录表'
                }
                info <- col_test('病毒实验记录表')
        }else{
                if(!any(str_detect(file_name,'病毒实验记录表|分子实验记录表'))){
                        info <- '系统检测未上传分子和病毒实验记录表'
                }else if(!any(str_detect(file_name,'病毒实验记录表'))){
                        info <- '系统检测未上传病毒实验记录表'
                }else if(!any(str_detect(file_name,'分子实验记录表'))){
                        info <- '系统检测未上传分子实验记录表'
                }else{
                        info <- col_test('分子实验记录表')
                        if(info=='未自动检测到问题'){
                                info <- col_test('病毒实验记录表')
                        }
                        
                }
        }
        return(info)
}
