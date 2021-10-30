#自动归档2020tb流程任务，202流程任务生产和销售序列是合并的，故须构造出虚拟的生产主、子任务和销售主任务以便整合入新流程中
#任务ID=='fw-1287'
auto_archieve2 <- function(){
        #查询2020部门运营中，2020销售技术新流程和分子构建（确定基因合成项目）任务列表中的所欲任务
        tql.query <- "_projectId=58081fe94863251f4269aaf3 AND _tasklistId=5dedbcd453b99f0020ec76aa OR _tasklistId=5ce122f34f895a001991ae12 isArchived = false"
        uniqueId.prefix <- "DS-"
        
        dt <- tql_query(tql.query,uniqueId.prefix)
        
        dt_new <- 
                bind_rows(discard(dt,is.null)) %>% 
                rename(创建时间=created,
                           标题=content,
                           截止时间=dueDate,
                           开始时间=startDate,
                           任务类型=templateId,
                           任务ID=uniqueId,
                           父任务.ObjectId=parentTaskId,
                           任务.ObjectId=taskId) 
        
        db <- DBI::dbConnect(SQLite(),dbname='data/testDB.db')
        cf_phrase <- dbReadTable(db,'cf_phrase')
        dbDisconnect(db)
        
        #以预制cfid对照表转化customfields为字段名称
        for (i in 1:length(colnames(dt_new))) {
                if(colnames(dt_new)[i]%in%cf_phrase$customfildID){
                        colnames(dt_new)[i] <- subset(cf_phrase,customfildID==colnames(dt_new)[i]) %>% 
                                pull(name)
                }
        }
        
        #保存任务分组不是2020销售技术新流的任务
        dt_tag <- dt_new %>%
                dplyr::filter(任务类型!='5dedcf9a5704e70001cef3bc')
        
        #仅保存任务类型为2020销售技术服务流程的任务
        dt_new <- 
                dt_new %>% 
                dplyr::filter(任务类型=='5dedcf9a5704e70001cef3bc') %>% 
                select(!matches('^\\d')) #去除没有转化名称的customfields列
        
        subtype <- 
                function(type){
                        dt_sub <- dt_new %>% 
                                select(任务ID,标题,contains(type)) %>% 
                                drop_na() %>% #没有分子、病毒、细胞字段的任务
                                rename(
                                        CE.实验执行人姓名 = paste0('g5',type,'姓名'),
                                        截止时间 = paste0('d5',type,'截止'),
                                        Su.实验实际开始日期 = paste0('d5',type,'开始'),
                                        Su.实验实际完成日期 = paste0('g5',type,'完成'),
                                        CD.子产能 = paste0('d5',type,'产能')
                                ) %>% 
                                mutate(
                                        开始时间 = Su.实验实际开始日期,
                                        CD.子任务类型 = type,
                                        任务类型 = '生产序列模板',
                                        是否是子任务 = 'Y',
                                        标题 = paste0(任务ID,'-',标题),
                                ) 
                }
        
        #构造虚拟二级生产子任务
        dt_product <- map_dfr(c('分子','病毒','细胞'),subtype) %>% 
                mutate(任务ID = paste0(任务ID,CD.子任务类型),#虚拟任务ID
                         link=str_match(标题,'FW-\\d+'))#提取旧合同号作关联任务链接
        
        dt_product_sub <- 
        dt_tag %>% 
                select(X1.基因合成载体产能,标题) %>% 
                mutate(
                        link=str_match(标题,'FW-\\d+'),
                        CD.产能类型 = '基因合成载体'##标记旧项目中基因合成项目
                       ) %>% 
                drop_na() %>%
                select(CD.产能类型,link) %>% 
                right_join(dt_product) %>% 
                mutate(CD.产能类型=if_else(CD.子任务类型=='分子',CD.产能类型,'2020旧项目')) %>% 
                replace_na(list(CD.产能类型='2020旧项目'))#将所有非分子的基因合成载体任务转化为’旧项目‘
        
        #构造虚拟生产主任务
        dt_product_main <- 
        dt_new %>% 
                select(任务ID,开始时间,截止时间) %>% 
                # #drop_na() %>% 
                # rename(S.合同金额 = b4合同金额,
                #        S.消费金额 = b4消费金额,
                #        A.方案设计者 = 方案设计者,
                #        D.方案设计延期 = 方案设计延期,
                #        A.方案指派日期 = d5总开始) %>% 
                mutate(
                        任务类型 = '生产序列模板',
                        是否是子任务 = 'N',
                        任务ID = paste0(任务ID,'主任务')
                )
        
        
        #构造虚拟一级任务
        dt_sale <- dt_new %>% 
                dplyr::select(!contains(c('分子','病毒','细胞')))%>%
                #dplyr::select(!contains(c('分子','病毒','细胞')),
                #       -c(S.客户姓名,S.客户单位,S.是否新增客户,S.销售姓名,S.销售区域)) %>%
                #drop_na() %>% 
                rename(S.合同金额 = b4合同金额,
                       S.消费金额 = b4消费金额,
                       A.方案设计者 = 方案设计者,
                       D.方案设计延期 = 方案设计延期,
                       A.方案指派日期 = d5总开始,
                       S.合同金额=b4合同金额,
                       S.客户联系信息=a4联系方式,
                       S.客户姓名=a4客户姓名,
                       S.客户单位=a4客户单位,
                       S.销售姓名=a4销售姓名,
                       S.是否新增客户=a4新增客户,
                       S.销售区域=a4销售区域,
                       A.合同签订日期=b4签订日期,
                       A.变动金额=b4变动金额,
                       F.已开票=b4已开票金额,
                       F.未回款=b4未回款额,
                       Ag.外包售价=c6外包售价,
                       Ag.外包成本=c6外包成本,
                       W.出库日期=e4发货日期,
                       S.折扣=a4折扣,
                       P.结题报告是否规范=f5结题规范,
                       P.实际完成时间=f5总完成,
                       A.业务类别=b4业务类别,
                       S.预付款任务ID=b4消费合同
                       ) %>% 
                mutate(
                        任务类型 = '销售序列模板',
                        是否是子任务 = 'N',
                )
        
        dt_update <- bind_rows(dt_product_main,dt_product_sub,dt_sale) %>% 
                mutate(
                        import.time = as.numeric(Sys.time()),
                        任务ID = tolower(任务ID),
                        CD.组成产能 = CD.子产能,
                        ) %>% 
                select(-X1.基因合成载体产能,-link)
                 
        
        db <- DBI::dbConnect(SQLite(),dbname='data/testDB.db')
        dt_db <- dbReadTable(db,'db') 
        
        #合并2021数据库，统一表头
        dt_fin <- bind_rows(dt_db,dt_update)
        
        dbWriteTable(db,'db',dt_fin,overwrite=T)
        dbDisconnect(db)
        print('2020旧任务自动归档成功')
}

