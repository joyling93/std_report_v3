# dt <- dt2
# time_span <- '2021-02-02'
# period_type <- '月度'
# tag <- '无'

management_data_cal <- 
        function(dt,time_span,period_type,tag){
                index_cal <- 
                        function(x){
                                #functions
                                labor_cal <- 
                                        function(labor.table){
                                                #细胞准备人工按合同额比例拆分给不同病毒类型人工
                                                labor.table <- 
                                                        labor.table %>% 
                                                        mutate(
                                                                腺相关病毒=腺相关病毒+病毒组细胞准备*contribute_ratio_virus["腺相关病毒"],
                                                                慢病毒=慢病毒+病毒组细胞准备*contribute_ratio_virus["慢病毒"],
                                                                腺病毒=病毒组细胞准备*contribute_ratio_virus["腺病毒"]
                                                        )
                                                
                                                #按业务类型拆分人工
                                                dt1 <- 
                                                        map_dfr(names(contribute_ratio),function(type){
                                                                tibble::tibble(
                                                                        type1=type,
                                                                        subtype=c('管理','仓库','产品','销售','企管'),
                                                                        value=c(
                                                                                labor.table$生产管理*contribute_ratio_not_cell[type],
                                                                                labor.table$仓库*contribute_ratio[type],
                                                                                0,
                                                                                labor.table$销售*seals.ratio.not_others[type],
                                                                                labor.table$企管*seals.ratio.not_others[type]
                                                                        )
                                                                ) 
                                                        })
                                                
                                                dt2 <- 
                                                        map_dfr(c('试剂','生信','代理','大综合','预付款'),function(type){
                                                                tibble::tibble(
                                                                        type1=type,
                                                                        subtype=c('产品','销售','企管'),
                                                                        value=c(
                                                                                0,
                                                                                labor.table$销售*seals.ratio.not_others[type],
                                                                                labor.table$企管*seals.ratio.not_others[type]
                                                                        )
                                                                )
                                                        })
                                                
                                                labor.all <- 
                                                        bind_rows(dt1,dt2) %>% 
                                                        add_row(
                                                                type1=c('研发','分子','细胞','慢病毒','腺病毒','腺相关病毒'),
                                                                subtype=c('产品','生产','产品','生产','生产','生产'),
                                                                value=c(labor.table$产品研发,labor.table$分子,labor.table$细胞,
                                                                        labor.table$慢病毒,labor.table$腺病毒,labor.table$腺相关病毒
                                                                )
                                                        ) %>% 
                                                        mutate(
                                                                tag='labor'
                                                        )
                                        }
                                
                                fee_cal <- function(daily.operation.fee,rent.fee){
                                        fee.all <- 
                                                map_dfr(names(seals.ratio.not_others),function(type){
                                                        tibble::tibble(
                                                                type1=type,
                                                                subtype=c('生产','产品','销售','企管','房租','水电','网费','母公司服务费','固定资产折旧'),
                                                                value=c(
                                                                        daily.operation.fee$生产部*contribute_ratio[type],
                                                                        daily.operation.fee$产品部*contribute_ratio[type],
                                                                        daily.operation.fee$销售部*seals.ratio.not_others[type],
                                                                        daily.operation.fee$企管部*seals.ratio.not_others[type],
                                                                        rent.fee$房租*contribute_ratio[type],
                                                                        rent.fee$水电*contribute_ratio[type],
                                                                        rent.fee$网费*contribute_ratio[type],
                                                                        rent.fee$母公司服务费*contribute_ratio[type],
                                                                        rent.fee$固定资产折旧*contribute_ratio[type]
                                                                )
                                                        )
                                                }
                                                ) %>% 
                                                mutate(
                                                        tag='fee'
                                                )
                                }
                                
                                cost_cal <- function(cost.export,cost.seq,cost.pack){
                                        cost.export <- 
                                                cost.export %>% 
                                                mutate(
                                                        分子=分子+公共*contribute_ratio['分子'],
                                                        细胞=细胞+公共*contribute_ratio['细胞'],
                                                        慢病毒=病毒*contribute_ratio_virus['慢病毒']+公共*contribute_ratio['慢病毒'],
                                                        腺病毒=病毒*contribute_ratio_virus['腺病毒']+公共*contribute_ratio['腺病毒'],
                                                        腺相关病毒=病毒*contribute_ratio_virus['腺相关病毒']+公共*contribute_ratio['腺相关病毒']
                                                        # 生产=分子+慢病毒+腺病毒+腺相关病毒,
                                                        # 产品=细胞+研发
                                                )
                                        
                                        cost.seq <- 
                                                cost.seq %>% 
                                                mutate(
                                                        across(everything(),function(x){
                                                                replace_na(x,0)
                                                        })
                                                        # 生产=(分子+慢病毒+腺病毒+腺相关病毒),
                                                        # 产品=细胞+研发
                                                )
                                        
                                        cost.pack <- 
                                                cost.pack %>% 
                                                mutate(
                                                        across(everything(),function(x){
                                                                replace_na(x,0)
                                                        })
                                                        # 生产=(分子+慢病毒+腺病毒+腺相关病毒),
                                                        # 产品=细胞
                                                )
                                        
                                        cost.all <- 
                                                bind_rows(cost.export,cost.seq,cost.pack) %>% 
                                                pivot_longer(everything(),names_to='type1',values_to='value') %>% 
                                                group_by(type1) %>% 
                                                summarise(
                                                        value=sum(value,na.rm = T),
                                                        tag='cost'
                                                ) %>% 
                                                mutate(
                                                        subtype=fct_collapse(type1,
                                                                             生产=c('分子','慢病毒','腺病毒','腺相关病毒'),
                                                                             产品=c('细胞','研发'),
                                                                             other_level='drop'
                                                        )
                                                ) %>% 
                                                dplyr::filter(subtype!='drop')  
                                }
                                
                                
                                dt <- dt %>% 
                                        dplyr::filter(统计周期==x)
                                
                                supp.dt <- supp.dt %>% 
                                        dplyr::filter(日期==x)
                                
                                #产值和比例计算
                                #统计字段提取和质控
                                dt.ori <- 
                                        dt %>% 
                                        # dplyr::filter(
                                        #         lubridate::month(A.合同签订日期)==date.input
                                        # ) %>% 
                                        group_by(任务ID,生产执行人,产值类型,产值
                                                   ,S.合同金额,S.消费金额,A.合同签订日期,A.业务类别,标题) %>% 
                                        nest() %>% 
                                        dplyr::filter(
                                                !str_detect(A.业务类别,'大综合|试剂|其他|生信|预付款$')#去除不纳入产值统计的项目
                                        ) %>% 
                                        mutate(
                                                qc.tag=if_else(
                                                        is.na(A.合同签订日期)|is.na(产值类型)|is.na(产值),#标记疑似异常项目
                                                        'error',
                                                        'pass')
                                        )
                                #异常项目
                                error.table <- 
                                        dt.ori %>% 
                                        dplyr::filter(qc.tag=='error')
                                #正常纳入统计的项目
                                dt.stat <- 
                                        dt.ori %>% 
                                        dplyr::filter(qc.tag=='pass') %>% 
                                        mutate(
                                                S.消费金额=map_int(S.消费金额,function(x){
                                                        sum(as.integer(str_split(x, ',',simplify = T)),na.rm = T)
                                                }),#拆分逗号分隔的消费金额并计算消费总额
                                                S.合同金额=as.numeric(replace_na(S.合同金额,0)),
                                                new.group=str_replace(A.业务类别,'.* / ','')#去除业务类别的一级内容
                                        ) %>% 
                                        group_by(任务ID) %>% 
                                        mutate(
                                                contract.all=S.合同金额,S.消费金额,
                                                p.ratio=产值/sum(产值),
                                                p.value=contract.all*p.ratio
                                        )
                                
                                ##按产能比例拆分销售额
                                contribute_table <- 
                                        dt.stat %>% 
                                        mutate(
                                                产值类型=if_else(产值类型=='病毒',new.group,产值类型),
                                                产值类型=fct_collapse(产值类型,
                                                                      慢病毒=c('慢病毒','细胞检测')
                                                )
                                        ) %>% 
                                        group_by(产值类型) %>% 
                                        summarise(
                                                p.value=sum(产值)
                                        )
                                contribute_table <- bind_rows(contribute_table,
                                                              tibble(
                                                                      产值类型=c('分子','细胞','慢病毒','腺病毒','腺相关病毒'),
                                                                        p.value=as.double(0,0,0,0,0)
                                                              )
                                ) %>% 
                                        distinct(产值类型,.keep_all=T)
                                
                                
                                
                                #计算含细胞产能比例
                                contribute_ratio <- 
                                        contribute_table %>% 
                                        ungroup() %>% 
                                        mutate(
                                                p.ratio=p.value/sum(p.value)
                                        )%>% 
                                        pull(p.ratio,name=产值类型)#以命名向量形式导出比例，方便引用
                                
                                #计算不含细胞的产能比例
                                contribute_ratio_not_cell <- 
                                        contribute_table %>% 
                                        dplyr::filter(产值类型!='细胞') %>% 
                                        ungroup() %>% 
                                        mutate(
                                                p.ratio.nc=p.value/sum(p.value),
                                                .keep="unused"
                                        ) %>% 
                                        pull(p.ratio.nc,name=产值类型)
                                
                                #计算仅病毒产能比例
                                contribute_ratio_virus <- 
                                        contribute_table %>% 
                                        dplyr::filter(str_detect(产值类型,'病毒')) %>% 
                                        ungroup() %>% 
                                        mutate(
                                                p.ratio.v=p.value/sum(p.value),
                                                .keep="unused"
                                        )%>% 
                                        pull(p.ratio.v,name=产值类型)
                                
                                ##销售额和比例计算
                                #按业务类别拆分销售额比例
                                dt.stat2 <- dt %>% 
                                        mutate(
                                                S.消费金额=map_int(S.消费金额,function(x){
                                                        sum(as.integer(str_split(x, ',',simplify = T)),na.rm = T)
                                                }),#拆分逗号分隔的消费金额并计算消费总额
                                                S.合同金额=as.numeric(replace_na(S.合同金额,0)),
                                                new.group=str_replace(A.业务类别,'.* / ',''),#去除业务类别的一级内容
                                                new.group=fct_collapse(new.group,
                                                                       细胞=c('细胞检测','细胞系构建','细胞'),
                                                                       大综合=c('大综合','代理大综合')
                                                )
                                        ) 
                                
                                seals.dt <- 
                                        dt.stat2 %>% 
                                        dplyr::filter(!str_detect(A.业务类别,'预付款消费')) %>% 
                                        group_by(new.group) %>% 
                                        summarise(
                                                sum=sum(S.合同金额)
                                        ) %>% 
                                        rename(type1='new.group')
                                
                                #无其他比例
                                seals.ratio.not_others <- 
                                        seals.dt %>% 
                                        dplyr::filter(type1!='其他') %>% 
                                        mutate(
                                                ratio=sum/sum(sum)
                                        ) %>% 
                                        pull(ratio,name=type1)
                                
                                #无预付款和其他比例
                                seals.ratio_not_adv_others <- 
                                        seals.dt %>% 
                                        dplyr::filter(!type1%in%c('其他','预付款')) %>% 
                                        mutate(
                                                ratio=sum/sum(sum)
                                        ) %>% 
                                        pull(ratio,name=type1)
                                
                                #按销售额比例计算预付款剩余
                                deposit <- 
                                        enframe(seals.ratio_not_adv_others,name = 'type1') %>% 
                                        mutate(
                                                sum=value*supp.dt$data[supp.dt$tag=='预付款'][[1]]$剩余预付款,
                                                tag='deposit'
                                        )
                                
                                ###人工计算
                                labor.all <- 
                                        labor_cal(supp.dt$data[supp.dt$tag=='人工'][[1]])
                                
                                
                                ###日常和固定成本计算
                                fee.all <- fee_cal(
                                        supp.dt$data[supp.dt$tag=='日常经营'][[1]]
                                        ,
                                        supp.dt$data[supp.dt$tag=='房租水电'][[1]]
                                )
                                
                                
                                ##材料出库，测序，包装成本
                                cost.export <- dplyr::filter(supp.dt$data[supp.dt$tag=='材料、基因合成、测序等'][[1]],
                                                             类型=='材料') %>% 
                                        select(-类型)
                                cost.seq <- dplyr::filter(supp.dt$data[supp.dt$tag=='材料、基因合成、测序等'][[1]],
                                                          类型=='基因合成+测序成本')%>% 
                                        select(-类型)
                                cost.pack <- dplyr::filter(supp.dt$data[supp.dt$tag=='材料、基因合成、测序等'][[1]],
                                                           类型=='干冰泡沫箱')%>% 
                                        select(-类型)
                                cost.all <- cost_cal(cost.export,cost.seq,cost.pack)
                                
                                
                                ##汇总
                                dt.all <- 
                                        bind_rows(fee.all,labor.all,cost.all) %>% 
                                        mutate(
                                                value=replace_na(value,0),
                                                subtype=fct_collapse(subtype,
                                                                     生产=c('仓库','管理'),
                                                                     房租等固定成本=c('固定资产折旧','母公司服务费',
                                                                               '水电','网费','房租')
                                                )
                                        )
                                
                                #按部门和成本类型汇总成本
                                summary1 <- 
                                        dt.all %>% 
                                        group_by(subtype,tag) %>% 
                                        summarise(
                                                sum=sum(value)
                                        )%>% 
                                        pivot_wider(
                                                names_from = tag,
                                                values_from = sum
                                        ) %>% 
                                        mutate(
                                                小计=sum(cost,fee,labor,na.rm = T)
                                        ) %>% 
                                        ungroup() %>% 
                                        mutate(
                                                `比例%`=round(小计/sum(小计,na.rm = T)*100,digits = 2)
                                        ) %>% 
                                        rename(
                                                材料=cost,
                                                日常经营=fee,
                                                人工=labor
                                        )
                                
                                #按业务类型汇总成本
                                summary2 <- 
                                        dt.all %>% 
                                        group_by(type1) %>% 
                                        summarise(
                                                sum=sum(value)
                                        )%>% 
                                        mutate(
                                                ratio=sum/sum(sum)
                                        ) 
                                
                                #按部门汇总成本
                                summary3 <- 
                                        dt.all %>% 
                                        group_by(subtype,tag) %>% 
                                        summarise(
                                                sum=sum(value)
                                        )%>% 
                                        group_by(subtype) %>% 
                                        summarise(
                                                sum=sum(sum)
                                        ) %>% 
                                        mutate(
                                                ratio=sum/sum(sum)
                                        )
                                
                                #按业务类型汇总成本、销售额、预付款并计算成本销售比和成本产值比
                                output <- 
                                        bind_rows(
                                                mutate(summary2,
                                                       tag='成本小计'),
                                                mutate(seals.dt,
                                                       tag='销售额小计'),
                                                mutate(deposit,
                                                       tag='预付款剩余小计'),
                                                contribute_table %>% 
                                                        mutate(
                                                                tag='生产产值'
                                                        ) %>% 
                                                        rename(
                                                                'type1'=产值类型,
                                                                'sum'=p.value
                                                        )
                                        ) %>% 
                                        select(type1,sum,tag) %>% 
                                        pivot_wider(names_from = 'tag',values_from='sum') %>% 
                                        # dplyr::filter(
                                        #         type1%in%c('分子','慢病毒','腺病毒','腺相关病毒','细胞')
                                        # ) %>% 
                                        mutate(
                                                `成本销售比%`=round(
                                                        成本小计/(销售额小计+预付款剩余小计)*100,
                                                        digits = 2),
                                                `成本产值比%`=round(
                                                        成本小计/生产产值*100,
                                                        digits = 2),
                                        )
                                list(output,summary1,summary2,summary3)
                        }
                
                #限制统计数据为当年
                current.year <- year(time_span)
                
                dt <-
                        dt %>%
                        dplyr::filter(year(A.合同签订日期)==current.year) %>% 
                        mutate(
                                统计周期 = month(A.合同签订日期)
                        ) 
                
                db.list <- c("人工","开票额","房租水电","日常经营","材料、基因合成、测序等","预付款" )
                db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
                supp.dt <-
                        map_dfr(db.list,function(dbname){
                                dbReadTable(db,dbname)%>%
                                        dplyr::filter(year(ymd(日期))==current.year) %>% 
                                        mutate(
                                                日期 = month(ymd(日期))  
                                        )%>% 
                                        group_by(日期) %>% 
                                        nest() %>% 
                                        mutate(
                                                tag=dbname
                                        )
                        })
                dbDisconnect(db)
                
                dt.fin <- 
                map(unique(supp.dt$日期),index_cal) 
                
                names(dt.fin) <- paste0(current.year,'-',unique(supp.dt$日期))
                
                dt.fin <- 
                enframe(dt.fin,name='统计周期') %>% 
                        mutate(
                                tag=list(c('成本销售比和成本产值比','按部门和成本类型汇总成本',
                                      '按业务类型汇总成本','按部门汇总成本'))
                        ) %>% 
                        unnest(c(value,tag)) %>% 
                        group_by(tag) %>% 
                        nest() %>% 
                        mutate(
                                data2=map(data,unnest) 
                                ) %>% 
                        select(1,3) %>% 
                        deframe()
                dt.fin[['原始数据']] <- dt
                
                list(dt.fin[[1]],
                     dt.fin
                     )
}

data_extraction <- 
        function(dt){
                dt_s <- dt %>% 
                        dplyr::filter(任务类型%in%c('销售序列模板','售后序列模板'))
                
                dt_p <- dt %>% 
                        dplyr::filter(任务类型%in%c('生产序列模板')) %>% 
                        dplyr::filter(是否是子任务=='Y') %>% 
                        mutate(
                                主任务ID = unlist(map(标题,
                                                     ~tolower(str_extract(.x,regex('fw-?\\d+|DS-?\\d+', ignore_case = T))))),
                                #为没有’-‘的任务ID添加’-‘，避免匹配丢失问题
                                主任务ID = if_else(
                                        str_detect(主任务ID,'-'),
                                        主任务ID,
                                        paste0(str_sub(主任务ID,1,2),'-',str_extract(主任务ID,'\\d+'))
                                )
                        ) %>% 
                        select(CD.子任务类型,CD.子产能,CE.实验执行人姓名,主任务ID)
                
                dt.fin <- 
                        dt_p %>%         
                        group_by(主任务ID) %>% 
                        mutate(
                                CD.子任务类型 = if_else(CD.子任务类型=='','类型字段未填',CD.子任务类型)
                        ) %>% 
                        group_by(主任务ID,CE.实验执行人姓名,CD.子任务类型) %>% 
                        summarise(CD.子产能=sum(as.numeric(CD.子产能))) %>% 
                        rename(
                                任务ID=主任务ID,
                                生产执行人=CE.实验执行人姓名,
                                产值类型=CD.子任务类型,
                                产值=CD.子产能
                        ) %>% 
                        right_join(dt_s,by=c('任务ID'))
        }



# contribute_table <-
#         tibble::tribble(
#                 ~分子,    ~慢病毒, ~腺相关病毒,   ~腺病毒,    ~细胞,
#                 197750L, 113604L, 44001L, 12000L, 26572L
#         ) %>%
#         pivot_longer(
#                 everything(),
#                 names_to='产值类型',
#                 values_to='p.value'
#         )

#main
#过滤统计周期
# time_filter <- switch(period_type,
#                       '周度' = function(x){
#                               as.character(cut(x,'week',start.on.monday=F))
#                       },
#                       '月度' = month,
#                       '年度' = year)
# 
# 
# if(tag=='无'){
#         dt <-
#                 dt %>%
#                 mutate(
#                         #D.任务周期.工作日 = as.numeric(D.任务周期.工作日),
#                         统计周期 = time_filter(A.合同签订日期)
#                 ) %>%
#                 dplyr::filter(统计周期==time_filter(time_span))
#         
#         db.list <- c("人工","开票额","房租水电","日常经营","材料、基因合成、测序等","预付款" )
#         db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
#         supp.dt <- 
#                 map(db.list,function(dbname){
#                         dbReadTable(db,dbname)%>%
#                                 dplyr::filter(time_filter(ymd(日期))==time_filter(time_span)) %>% 
#                                 select(-日期)
#                 })
#         dbDisconnect(db)
#         names(supp.dt) <- db.list
#         
# }else if(tag=='不筛选特定时间'){
#         dt
#         # dt %>%
#         # mutate(
#         #         D.任务周期.工作日 = as.numeric(D.任务周期.工作日),
#         # )
# }else{
#         dt
# }