#filepath <- '/Users/zhuomingx/Desktop/Rbio/std_report_v3/debug/销售提成自动化/2021年4月销售部提成数据.xlsx'


seals_commission_cal <- function(filepath,tb.raw,period_type,time_span){
        ##数据整合
        #拆分台账合同号，整合销售序列tb任务
        #增加销售类别和一级业务类别tag
        #筛选各种条件,汇总开票回款和预付数据
        #以tag分组汇总开票，回款，代理成本
        #判断tag赋予开票提成系数、回款提成系数和是否减代理成本
        
        ledger_raw <- openxlsx::read.xlsx(filepath,detectDates = T,sheet='原始数据-台账')
        time_filter <- switch(period_type,
                              '周度' = function(x){
                                      ymd(cut(x+ddays(1),'week',start.on.monday=F))-ddays(1)
                                      #延后实际日期使周数从周五开始
                              },
                              '月度' = month,
                              '年度' = year)
        #开票日期格式化
        tb.data <- 
                tb_raw %>% 
                
                mutate(
                        任务ID=if_else(str_detect(任务ID,'ds'),str_match(标题,regex('A-FW-\\d+',ignore_case = T)),任务ID),
                        
                        link=tolower(str_replace_all(任务ID,'[ -]',''))
                ) %>% 
                dplyr::filter(任务类型=='销售序列模板') %>% 
                select(标题,任务ID,A.合同签订日期,Ag.外包成本,A.业务类别,S.销售姓名,
                         S.客户姓名,S.合同金额,S.消费金额,F.未回款,F.已开票,link) %>% 
                separate(A.业务类别,into = c('一级任务类别','二级任务类别'),sep = ' / ') %>% 
                mutate(
                        一级任务类别=if_else(一级任务类别=='预付款消费'&二级任务类别=='代理','预付款消费代理',一级任务类别),
                        销售类别=fct_collapse(S.销售姓名,
                                          刘艳=c('刘艳'),
                                          大客户=c('大客户','周冰'),
                                          华北大客户=c('华北大客户','杨宝兴','樊伟','贾东东'),
                                          西南大客户=c('西南大客户','张义凯'),
                                          other_level=c('销售专员')
                        ),
                        across(matches('成本|金额'),as.numeric)
                )
        
        
        dt.all <- 
                ledger_raw %>% 
                mutate(
                        link=tolower(str_replace_all(合同号,'[ -]',''))
                ) %>% 
                left_join(tb.data) %>% 
                drop_na(开票日期,回款日期,A.合同签订日期)
        
        invoice.dt <- 
                dt.all %>% 
                dplyr::filter(
                        !(是否作废重开票=='是'&作废=='是'&开具金额<0),A.合同签订日期>as.Date('2020-07-31'),
                        time_filter(ymd(开票日期))==time_filter(time_span)
                ) %>% 
                group_by(一级任务类别,销售类别,S.销售姓名,time_filter(开票日期)) %>% 
                summarise(
                        开具金额=sum(开具金额)
                )
        
        payment.dt <- 
                dt.all %>% 
                dplyr::filter(
                        time_filter(回款日期)==time_filter(time_span)
                ) %>% 
                group_by(一级任务类别,销售类别,S.销售姓名,time_filter(回款日期)) %>% 
                summarise(
                        回款金额=sum(回款金额,na.rm = T),
                        签单金额=sum(S.合同金额,na.rm = T)
                ) %>% 
                drop_na(一级任务类别)
        
        advance.dt <- 
                dt.all %>% 
                dplyr::filter(
                        一级任务类别=='预付款消费',
                        time_filter(A.合同签订日期)==time_filter(time_span)) %>% 
                group_by(一级任务类别,销售类别,S.销售姓名,time_filter(A.合同签订日期)) %>% 
                summarise(
                        消费金额=sum(S.消费金额,na.rm = T),
                        代理成本=sum(Ag.外包成本,na.rm = T)
                ) %>% 
                drop_na(一级任务类别)
        
        
        dt.fin <- purrr::reduce(list(invoice.dt,payment.dt,advance.dt),left_join) %>% 
                mutate(
                        across(matches('金额|成本'),replace_na,replace=0),
                        开票提成=pmap(list(销售类别,一级任务类别,开具金额,消费金额,
                                           代理成本,签单金额,回款金额),
                                  function(销售类别,一级任务类别,开具金额,消费金额,
                                               代理成本,签单金额,回款金额){
                                          p <- 0
                                          if(销售类别=='销售专员'){
                                                  if(一级任务类别=='常规业务'){
                                                          p <- 开具金额*0.03
                                                  }else if(一级任务类别=='预付款'){
                                                          p <- 开具金额*0
                                                  }else if(一级任务类别=='预付款消费'){
                                                          p <- 开具金额*0.04
                                                  }else if(一级任务类别=='预付款消费代理'){
                                                          p <- (消费金额-代理成本)*1.8*0.04
                                                  }else if(一级任务类别=='代理'){
                                                          p <- (开具金额-代理成本)*1.8*0.03
                                                  }else if(一级任务类别=='其他'){
                                                          p <- 开具金额*0
                                                  }else{#大综合
                                                          p <- (签单金额-代理成本)*1.8*0.06*(回款金额/签单金额)
                                                  }
                                          }else if(销售类别=='刘艳'){
                                                  if(一级任务类别=='常规业务'){
                                                          p <- 开具金额*0.02
                                                  }else if(一级任务类别=='预付款'){
                                                          p <- 开具金额*0
                                                  }else if(一级任务类别=='预付款消费'){
                                                          p <- 开具金额*0.03
                                                  }else if(一级任务类别=='预付款消费代理'){
                                                          p <- (消费金额-代理成本)*1.8*0.04
                                                  }else if(一级任务类别=='代理'){
                                                          p <- (开具金额-代理成本)*1.8*0.02
                                                  }else if(一级任务类别=='其他'){
                                                          p <- 开具金额*0
                                                  }else{
                                                          p <- (签单金额-代理成本)*1.8*0.04*(回款金额/签单金额)
                                                  }
                                          }else{#各种大客户
                                                  p <- sum(开具金额,na.rm = T)
                                          }
                                          p
                                  }),
                        回款提成=pmap(list(销售类别,一级任务类别,开具金额,消费金额,
                                           代理成本,签单金额,回款金额),
                                  function(销售类别,一级任务类别,开具金额,消费金额,
                                               代理成本,签单金额,回款金额){
                                          p <- 0
                                          if(销售类别=='销售专员'){
                                                  p <- switch(一级任务类别,
                                                                    常规业务=开具金额*0.03,
                                                                    预付款=开具金额*0.03,
                                                                    预付款消费=开具金额*0,
                                                                    大综合=0,
                                                                    预付款消费代理=开具金额*0,
                                                                    代理=(开具金额-代理成本)*1.8*0.03,
                                                                    其他=开具金额*0)
                                          }else if(销售类别=='刘艳'){
                                                  p <- switch(一级任务类别,
                                                                    常规业务=开具金额*0.02,
                                                                    预付款=开具金额*0.02,
                                                                    预付款消费=开具金额*0,
                                                                    大综合=0,
                                                                    预付款消费代理=开具金额*0,
                                                                    代理=(开具金额-代理成本)*1.8*0.02,
                                                                    其他=开具金额*0)
                                          }else{
                                                  p <- sum(回款金额,na.rm = T)
                                          }
                                          p
                                  })
                )
        
        
        list(
                '汇总结果'=dt.fin,
                '原始数据'=dt.all
        )
}
