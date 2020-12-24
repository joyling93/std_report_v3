library(reticulate)

source_python('test.py')
gb_list <- dir('../debug/gb','*.gb',full.names = T)

test <- purrr::map(gb_list,feature_extract)

