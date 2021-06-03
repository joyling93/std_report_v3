# dt.all <- dt2
# time_span <- '2021-05-02'
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
                select("合同号",'S.销售姓名','A.合同签订日期','S.合同金额','S.消费金额',
                       '开票日期','开具金额','回款日期','回款金额','二级任务类别','项目延期'
                )
        
        dt2 <- 
        dt %>% dplyr::filter(
                        year(as.Date(A.合同签订日期))==year(time_span),
                        time_filter(as.Date(A.合同签订日期))==time_filter(time_span)
                ) %>%
                group_by(S.销售姓名,二级任务类别,time_filter(as.Date(A.合同签订日期))) %>% 
                summarise(
                        合同总数=n(),
                        合同金额=sum(S.合同金额,na.rm = T),
                        消费金额=sum(S.消费金额,na.rm = T),
                        延期数=sum(项目延期,na.rm = T)
                ) 
        
        output.list <- list(
                '项目管理统计'=dt2,
                '项目管理原始数据'=dt
        )
                
}



