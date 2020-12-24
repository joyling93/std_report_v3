
#contract_num <- 'a-fw-112'
#name_list <- c("pHS-AVC-LW1909","pHS-BVC-LW233","pHS-AVC-LW2088")
#project <- '载体构建和病毒包装'
#zip_list('test.zip')
#temp_dir <- '.'

dir_creator <- 
        function(temp_dir,contract_num,vector_list,project){
                
                if(str_detect(project,'病毒包装')){
                        dir_list1 <- c('质粒转染48h','质粒转染72h','病毒感染48h')
                        fp <- file.path(temp_dir,contract_num,'病毒实验数据',dir_list1)
                        for(fp2 in fp){
                                purrr::walk(file.path(fp2,vector_list),dir.create,recursive = T)
                        }
                }    
                if(str_detect(project,'载体构建')){
                        dir_list1 <- c('骨架载体','质粒图谱和测序数据')
                        fp <- file.path(temp_dir,contract_num,'分子实验数据',dir_list1)
                        purrr::walk(fp,dir.create,recursive = T)
                        fp <- str_subset(fp,'质粒图谱和测序数据')
                        for(fp2 in fp){
                                purrr::walk(file.path(fp2,vector_list),dir.create,recursive = T)
                        }
                }
                return(file.path(temp_dir,contract_num))
                #zipr('test.zip',file.path(temp_dir,contract_num))
        }









                
              
                

        
                
                
