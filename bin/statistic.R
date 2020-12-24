# library(dplyr)
# library(ggplot2)
# file_name <- '统计测试.csv'
# raw_data <- readr::read_csv(file.path('./debug',file_name))
# dt <- 
# raw_data %>% 
#         group_by(父任务) %>% 
#         tidyr::fill(1:26,.direction='downup')

tidy_data <- 
        function(x,y){
                raw_data <- readr::read_csv(x)
                dt1 <- 
                        raw_data %>% 
                        mutate(across(contains('截止'),lubridate::date)) %>%
                        group_by(病毒完成人) %>% 
                        summarise(profit = sum(病毒产能,na.rm = T)) %>%
                        tidyr::drop_na()
                
                dt2 <- 
                        raw_data %>% 
                        mutate(across(contains('截止'),lubridate::date)) %>%
                        group_by(分子完成人) %>% 
                        summarise(profit = sum(分子产能,na.rm = T)) %>%
                        tidyr::drop_na()
                
                dt3 <- 
                        raw_data %>% 
                        mutate(across(contains('截止'),lubridate::date)) %>%
                        group_by(细胞完成人) %>% 
                        summarise(profit = sum(细胞产能,na.rm = T)) %>%
                        tidyr::drop_na()
                
                dt4 <- raw_data %>% 
                        select(病毒完成人,病毒滴度) %>% 
                        tidyr::drop_na() %>% 
                        mutate(across(2,~strsplit(.x,split = '[\f\n\r\t\v]'))) %>% 
                        tidyr::unnest() %>% 
                        mutate(across(病毒滴度,as.numeric))
                
                return(list(dt1,dt2,dt3,dt4))
        }











