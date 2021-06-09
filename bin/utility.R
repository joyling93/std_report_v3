# dt.all <- dt2
# time_span <- '2021-04-02'
# period_type <- '月度'
# tag <- '无'
admin_data_cal <- 
function(dt.all,time_span,period_type,tag){
        time_filter <- switch(period_type,
                              '周度' = function(x){
                                      ymd(cut(x+ddays(1),'week',start.on.monday=F))-ddays(1)
                                      #延后实际日期使周数从周五开始
                              },
                              '月度' = month,
                              '年度' = year)
        
        dt <- 
                dt.all %>% 
                dplyr::filter(!is.na(合同号)) %>% 
                mutate(
                        销售业务类别=if_else(一级任务类别%in%c('常规业务','预付款消费'),二级任务类别,一级任务类别)
                ) %>% 
                select("合同号",'S.销售姓名','A.合同签订日期','S.合同金额','S.消费金额',
                       '开票日期','开具金额','回款日期','回款金额',销售业务类别,'项目延期',未开票金额,
                       未回款金额,合同总额
                ) %>% 
                group_by(合同号) %>% 
                mutate(
                        开票次数=n(),
                        去重合同金额=S.合同金额/n()
                )
        
        
        dt2 <- 
        dt %>% dplyr::filter(
                        year(as.Date(A.合同签订日期))==year(time_span),
                        time_filter(as.Date(A.合同签订日期))==time_filter(time_span)
                ) %>%
                group_by(合同号) %>% 
                mutate(
                        延期数= 项目延期/n()
                ) %>% 
                group_by(S.销售姓名,销售业务类别,time_filter(as.Date(A.合同签订日期))) %>% 
                summarise(
                        合同总数=n(),
                        合同金额=sum(S.合同金额,na.rm = T),
                        消费金额=sum(S.消费金额,na.rm = T),
                        延期数=floor(sum(延期数,na.rm = T))
                )
        
        summary_dt <- 
                function(x,y){
                        dt %>% 
                                select(.data[[x]],.data[[y]]) %>% 
                                mutate(
                                        年度=year(.data[[x]]),
                                        季度=quarter(.data[[x]]),
                                        月度=month(.data[[x]]),
                                ) %>% 
                                group_by(年度,季度,月度) %>% 
                                summarise(
                                        金额总计=sum(.data[[y]],na.rm = T)
                                ) %>% 
                                ungroup() %>% 
                                slice_max(年度,n=25) %>% #提取最近三年至少25个月份的数据
                                group_by(年度) %>% 
                                mutate(
                                        `环比增长%`=(金额总计-dplyr::lag(金额总计,order_by =月度,n=1))
                                        /dplyr::lag(金额总计,order_by =月度,n=1)*100
                                ) %>% 
                                group_by(月度) %>% 
                                mutate(
                                        
                                        `同比增长%`=(金额总计-dplyr::lag(金额总计,order_by =年度,n=1))
                                        /dplyr::lag(金额总计,order_by =年度,n=1)*100
                                ) 
                }
        
        summary.list <- tibble(
                统计周期=c('A.合同签订日期','开票日期','回款日期'),
                统计金额=c('去重合同金额','开具金额','回款金额')
        ) %>% 
                mutate(
                        data=map2(统计周期,统计金额,summary_dt)
                ) %>% 
                select(2,3) %>% 
                deframe()
        
        output.list <- append(list(
                '销售任务延期'=dt2,
                '项目管理原始数据'=dt),
                summary.list)
                
}



