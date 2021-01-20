input_test <- 
function(file_name,project2){
        ##检测测试
        info <- '未自动检测到问题'
        if(project2=='载体构建'){
                if(!any(str_detect(file_name,'分子信息表'))){
                        info <- '系统检测未上传分子信息表'
                }
        }else if(project2=='病毒包装'){
                if(!any(str_detect(file_name,'病毒信息表'))){
                        info <- '系统检测未上传病毒信息表'
                }
        }else{
                if(!any(str_detect(file_name,'病毒信息表|分子信息表'))){
                        info <- '系统检测未上传分子和病毒信息表'
                }else if(!any(str_detect(file_name,'病毒信息表'))){
                        info <- '系统检测未上传病毒信息表'
                }else if(!any(str_detect(file_name,'分子信息表'))){
                        info <- '系统检测未上传分子信息表'
                }else{
                        info <- '未自动检测到问题'
                }
        }
        return(info)
}
