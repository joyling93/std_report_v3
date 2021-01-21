input_test <- 
function(file_name,project2){
        ##检测测试
        info <- '未自动检测到问题'
        if(project2=='载体构建'){
                if(!any(str_detect(file_name,'分子实验记录表'))){
                        info <- '系统检测未上传分子记录表'
                }
                
        }else if(project2=='病毒包装'){
                if(!any(str_detect(file_name,'病毒实验记录表'))){
                        info <- '系统检测未上传病毒实验记录表'
                }
        }else{
                if(!any(str_detect(file_name,'病毒实验记录表|分子实验记录表'))){
                        info <- '系统检测未上传分子和病毒实验记录表'
                }else if(!any(str_detect(file_name,'病毒实验记录表'))){
                        info <- '系统检测未上传病毒实验记录表'
                }else if(!any(str_detect(file_name,'分子实验记录表'))){
                        info <- '系统检测未上传分子实验记录表'
                }else{
                        info <- '未自动检测到问题'
                }
        }
        return(info)
}
