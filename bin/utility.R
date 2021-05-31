# dt <- dt2
# time_span <- '2021-04-02'
# period_type <- '月度'
# tag <- '无'

management_data_cal <- 
        function(dt,time_span,period_type,tag){
                index_cal <- 
                        function(data.supp,data.tb){
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
                                                                                labor.table$生产管理*contribute_ratio[type],
                                                                                labor.table$仓库*contribute_ratio[type],
                                                                                labor.table$产品生产支持*contribute_ratio[type],
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
                                
                                
                                dt <- data.tb
                                
                                supp.dt <- data.supp
                                
                                #产值和比例计算
                                
                                ##按产能比例拆分销售额
                                contribute_table <- 
                                        dt %>% 
                                        #dplyr::filter(qctag!='产能计算排除') %>% 
                                        group_by(产值类型) %>% 
                                        summarise(
                                                p.value=sum(产值,na.rm = T)
                                        ) %>% 
                                        drop_na()
                                
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
                                # contribute_ratio_not_cell <- 
                                #         contribute_table %>% 
                                #         dplyr::filter(产值类型!='细胞') %>% 
                                #         ungroup() %>% 
                                #         mutate(
                                #                 p.ratio.nc=p.value/sum(p.value),
                                #                 .keep="unused"
                                #         ) %>% 
                                #         pull(p.ratio.nc,name=产值类型)
                                
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
                                seals.dt <- 
                                        dt %>% 
                                        #dplyr::filter(qctag!='销售额计算排除') %>% 
                                        group_by(销售业务类别) %>% 
                                        summarise(
                                                sum=sum(销售额)
                                        ) %>% 
                                        #drop_na() %>% 
                                        rename(type1='销售业务类别')
                                
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
                                        select(-c(类型,日期))
                                cost.seq <- dplyr::filter(supp.dt$data[supp.dt$tag=='材料、基因合成、测序等'][[1]],
                                                          类型=='基因合成+测序成本')%>% 
                                        select(-c(类型,日期))
                                cost.pack <- dplyr::filter(supp.dt$data[supp.dt$tag=='材料、基因合成、测序等'][[1]],
                                                           类型=='干冰泡沫箱')%>% 
                                        select(-c(类型,日期))
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
                                        ) %>% 
                                        arrange(type1)
                                
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
                                #按业务类型和成本类型汇总成本
                                summary4 <- 
                                        dt.all %>% 
                                        ungroup() %>% 
                                        mutate(
                                                 tag=if_else(subtype=='房租等固定成本','房租等固定成本',tag)
                                         ) %>% 
                                        group_by(type1,tag) %>% 
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
                                                ratio=round(sum/sum(sum)*100,digits = 2)
                                        ) 
                                        # rename(
                                        #         #小计='sum',
                                        #         `比例%`=ratio
                                        # )
                                
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
                                                ratio=round(sum/sum(sum)*100,digits = 2)
                                        )
                                        # rename(
                                        #         #小计='sum',
                                        #         `比例%`=ratio
                                        # )
                                
                                #按业务类型汇总成本、销售额、预付款并计算成本销售比和成本产值比
                                output <- 
                                        bind_rows(
                                                mutate(summary2,
                                                       tag='成本小计'),
                                                mutate(seals.dt,
                                                       tag='销售额小计'),
                                                mutate(deposit,
                                                       tag='预付款小计'),
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
                                                        成本小计/(销售额小计+预付款小计)*100,
                                                        digits = 2),
                                                `成本产值比%`=round(
                                                        成本小计/生产产值*100,
                                                        digits = 2),
                                        )
                                list(output,summary1,summary2,summary3,summary4)
                        }
                
                dt2 <-
                        #test %>% 
                        dt %>%
                        dplyr::filter(qctag=='pass') %>% 
                        mutate(
                                年度 = year(A.合同签订日期),
                                月份 = month(A.合同签订日期)
                        ) %>% 
                        group_by(年度,月份) %>% 
                        nest()
                
                db.list <- c("人工","开票额","房租水电","日常经营","材料、基因合成、测序等","预付款" )
                db <- DBI::dbConnect(SQLite(),dbname='./data/testDB.db')
                supp.dt <-
                        map_dfr(db.list,function(dbname){
                                dbReadTable(db,dbname)%>%
                                        #dplyr::filter(year(ymd(日期))==current.year) %>% 
                                        mutate(
                                                年度 = year(ymd(日期)),
                                                月份 =  month(ymd(日期))
                                        )%>% 
                                        group_by(年度,月份) %>% 
                                        nest() %>% 
                                        mutate(
                                                tag=dbname
                                        )
                        }) %>% 
                        group_by(年度,月份) %>% 
                        nest()
                dbDisconnect(db)
                
                dt.fin <- 
                        supp.dt %>% 
                        left_join(dt2,by=c("年度", "月份"),suffix = c(".supp", ".tb")) %>% 
                        mutate(
                                out=map2(data.supp,data.tb,index_cal)
                        ) %>% 
                        select(年度,月份,out) %>% 
                        unnest() %>% 
                        mutate(
                                tag=c('成本销售比和成本产值比','按部门和成本类型汇总成本',
                                           '按业务类型汇总成本','按部门汇总成本','按业务类型和成本类型汇总成本')
                        ) %>% 
                        group_by(tag) %>% 
                        nest() %>% 
                        mutate(
                                data=map(data,unnest)
                        ) %>% 
                        deframe()
                
                dt.fin[['原始数据']] <- dt
                
                list(dt.fin[[2]],
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
                        select(CD.子任务类型,CD.子产能,CE.实验执行人姓名,主任务ID,CD.产能类型)
                
                dt.fin <- 
                        dt_p %>%         
                        group_by(主任务ID) %>% 
                        mutate(
                                CD.子任务类型 = if_else(CD.子任务类型=='病毒',CD.产能类型,CD.子任务类型),#确定具体病毒子类
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
                
                #qc
                output <- 
                dt.fin %>% 
                        mutate(
                                S.消费金额=map_int(S.消费金额,function(x){
                                        sum(as.integer(str_split(x, ',',simplify = T)),na.rm = T)
                                }),#拆分逗号分隔的消费金额并计算消费总额
                                S.合同金额=as.numeric(replace_na(S.合同金额,0)),
                                销售额=S.合同金额,
                                销售业务类别=str_replace(A.业务类别,'.* / ','')#去除业务类别的一级内容
                        ) %>% 
                        group_by(任务ID) %>% 
                        mutate(
                                产值比例=产值/sum(产值,na.rm = T),
                                销售额=if_else(is.na(产值比例),销售额,销售额*产值比例),
                                销售业务类别=if_else(
                                        str_detect(销售业务类别,'分子|病毒|细胞')&!is.na(产值类型),
                                        产值类型,销售业务类别),
                                qctag=if_else(
                                        S.客户单位=='合生生物'|is.na(S.客户单位),
                                        '统计排除项','pass'),
                                销售业务类别=fct_collapse(销售业务类别,
                                                          大综合=c('代理大综合','代理大综合'),
                                                          细胞=c('细胞检测','细胞系构建')
                                ),
                                销售业务类别=fct_other(
                                        销售业务类别, 
                                        keep=c('分子','慢病毒','腺病毒','腺相关病毒','细胞','试剂','其他','预付款','代理','生信','大综合'),
                                        other_level = "类别存疑"),
                                销售业务类别=fct_relevel(
                                        销售业务类别,
                                        c('分子','慢病毒','腺病毒','腺相关病毒','细胞','试剂','其他','预付款','代理','生信','大综合','类别存疑')
                                )
                               
                        ) %>% 
                        # mutate(
                        #         qctag=if_else(销售业务类别%in%c('分子','细胞','慢病毒','腺病毒','腺相关病毒')&销售业务类别%in%产值类型,qctag,'测试')
                        # ) %>% 
                        select(任务ID,产值比例,生产执行人,标题,任务类型,
                                 产值类型,销售业务类别,A.业务类别,产值,销售额,S.合同金额,S.消费金额,A.变动金额,A.合同签订日期,
                                 S.客户单位,S.销售姓名,qctag)
        }


