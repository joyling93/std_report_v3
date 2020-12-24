# library(reticulate)
# virtualenv_create("r-pandas") 创建虚拟环境
# py_install('openpyxl',envname = 'r-pandas') 向虚拟环境中安装模块
# use_virtualenv(virtualenv = "r-pandas", required =TRUE) 指定虚拟环境
# source_python('merge_excel.py')
# filelist <- dir('../debug',pattern = '.xls$',full.names = T)
# df <- file_merge(filelist)
# df <- openxlsx::read.xlsx('../debug/细胞信息表.xlsx')

# dbWriteTable(db,'test',df,append = TRUE) 向表中追加信息
# dbListTables(db) 查询tables
# dbWriteTable(db,'细胞信息表',df) 创建表
# dbGetQuery(db,'DROP TABLE 分子') 删除表
# dbDisconnect(db) 断开连接

library(RSQLite)
db <- DBI::dbConnect(SQLite(),dbname='../data/testDB.db')
dbReadTable(db,'')

